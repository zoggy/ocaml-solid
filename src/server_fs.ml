(** *)

let homes () =
  Filename.concat (Ocf.get Server_conf.storage_root) "home"

let documents () =
  Filename.concat (Ocf.get Server_conf.storage_root) "documents"

let string_of_file ?(on_err=fun _ -> Lwt.return_none) filename =
  try%lwt
    let%lwt str = Lwt_io.(with_file Input filename read) in
    Lwt.return_some str
  with e -> on_err e

let split_string ?(keep_empty=false) s chars =
  let re =
    Re.(
     let re = alt (List.map char chars) in
     let re = if keep_empty then re else rep1 re in
     compile re
    )
  in
  Re.split re s

let normalize =
  let rec iter acc = function
    [] -> List.rev acc
  | "." :: q -> iter acc q
  | ".." :: q ->
      begin
        match acc with
          [] -> iter acc q
        | _ :: acc -> iter acc q
      end
  | h :: q -> iter (h :: acc) q
  in
  iter []

type path =
  {
    iri: Iri.t ;
    root : string ;
    rel : string list ; (* must not start nor end with '/' *)
    kind : [`File | `Dir | `Acl | `Meta ] option ;
    mutable mime : string option ;
  }

let acl_suffix = ",acl"
let meta_suffix = ",meta"

let get_kind root rel =
  let fname = List.fold_left Filename.concat root rel in
  try%lwt
    let%lwt st = Lwt_unix.stat fname in
    match st.Unix.st_kind with
      Unix.S_REG ->
        if Filename.check_suffix fname acl_suffix then
          Lwt.return (Some `Acl)
        else if  Filename.check_suffix fname meta_suffix then
            Lwt.return (Some `Meta)
          else
            Lwt.return (Some `File)
    | Unix.S_DIR -> Lwt.return (Some `Dir)
    | _ -> Lwt.return_none
  with _ -> Lwt.return_none

let path_of_uri uri =
  let path = Uri.path uri in
  let path = split_string path ['/'] in
  let path = List.map Uri.pct_decode path in
  let path = normalize path in
  let (root, rel) =
    match path with
    | [] -> (documents (), path)
    | h :: q ->
        let len = String.length h in
        if len > 0 && String.get h 0 = '~' then
          (homes(), (String.sub h 1 (len - 1)) :: q)
        else
          (documents(), path)
  in
  let%lwt kind = get_kind root rel in
  let iri =
    let path =
      match kind with Some `Dir -> path @ [""] | _ -> path
    in
    Iri.iri ?scheme:(Uri.scheme uri)
      ?user:(Uri.user uri)
      ?host:(Uri.host uri)
      ?port:(Uri.port uri)
      ~path:(Iri.Absolute path) ()
  in
  Lwt.return { iri ; root ; rel ; kind ; mime = None }

let path_to_filename p =
  let fname = List.fold_left Filename.concat "/" (p.root :: p.rel) in
  match p.kind with
  | Some `Dir -> fname ^ "/"
  | Some _ | None -> fname

let iri p = p.iri
let kind p = p.kind

let ext_path suffix kind p =
  let (rel, mime) =
    match p.kind with
      Some `Dir ->  (p.rel @ [suffix], None)
    | Some k when k = kind -> (p.rel, p.mime)
    | _ ->
        match List.rev p.rel with
        | h :: q -> (List.rev ((h ^ suffix) :: q), None)
        | [] -> assert false
  in
  let iri =
    if rel = p.rel then
      p.iri
    else
      match Iri.path p.iri with
      | Iri.Relative _ -> assert false
      | Iri.Absolute path ->
          match List.rev path with
            [] -> assert false
          | h :: q ->
              let path = List.rev ((h^suffix)::q) in
              Iri.with_path p.iri (Iri.Absolute path)
  in
  { p with iri ; rel ; kind = Some kind ; mime }

let acl_path = ext_path acl_suffix `Acl
let meta_path = ext_path meta_suffix `Meta

(* CHOICE: acl and meta are stored in turtle format *)
let read_graph base filename =
  if not (Sys.file_exists filename) then
    Lwt.return_none
  else
    let on_err e =
      let%lwt () = Server_log._warn_lwt
        (fun f -> f "Could not read graph from %s: %s"
           filename (Printexc.to_string e))
      in Lwt.return_none
    in
    match%lwt string_of_file ~on_err filename with
      None -> Lwt.return_none
    | Some str ->
        let g = Rdf_graph.open_graph base in
        try Rdf_ttl.from_string g str;
          Lwt.return (Some g)
        with e ->
          let%lwt _ = on_err e in
          Lwt.return_none

let read_path_graph p =
  read_graph p.iri (path_to_filename p)

let path_mime p =
  match p.mime with
    Some _ -> Lwt.return p.mime
  | None ->
      let meta = meta_path p in
      match%lwt read_path_graph meta with
        None -> Lwt.return_none
      | Some g ->
          let open Rdf_term in
          match Rdf_graph.(literal_objects_of g
             ~sub:(Iri p.iri) ~pred:Rdf_dc.format)
          with
            [] -> Lwt.return_none
          | lit :: _ ->
              p.mime <- Some lit.lit_value ;
              Lwt.return (Some lit.lit_value)

let iri_parent_path =
  let rec remove_empty_strings = function
    "" :: q -> remove_empty_strings q
  | x -> x
  in
  fun iri ->
    let path =
      match Iri.path iri with
        Iri.Relative p
      | Iri.Absolute p -> p
    in
    let path = remove_empty_strings (List.rev path) in
    match path with
      [] -> None
    | h :: q ->
        let path = List.rev (""::q) in
        let iri =
          match Iri.path iri with
          | Iri.Relative _ -> Iri.with_path iri (Iri.Relative path)
          | Iri.Absolute _ -> Iri.with_path iri (Iri.Absolute path)
        in
        Some iri

let parent p =
  match List.rev p.rel with
    [] -> None
  | h :: q ->
      let rel = List.rev q in
      match iri_parent_path p.iri with
        None -> assert false
      | Some iri ->
          Some
            { iri ; root = p.root ;
              rel ; kind = Some `Dir ; mime = None ;
            }

let container_graph_of_dir iri dirname =
  let g = Rdf_graph.open_graph iri in
  let sub = Rdf_term.Iri iri in
  let pred_contains = Rdf_ldp.contains in
  let add_file g file =
    if file = Filename.current_dir_name
      || file = Filename.parent_dir_name
        || Filename.check_suffix file acl_suffix
        || Filename.check_suffix file meta_suffix
    then
      Lwt.return_unit
    else
      let absname = Filename.concat dirname file in
      match%lwt Lwt_unix.stat absname with
      | exception e ->
          let%lwt () = Server_log._err_lwt
            (fun m -> m "%s: %s" absname (Printexc.to_string e))
          in
          Lwt.return_unit
      | { Unix.st_kind = Unix.S_REG | Unix.S_DIR } as st ->
          let obj =
            let file =
              if st.Unix.st_kind = Unix.S_DIR then
                file^"/"
              else
                file
            in
            let iri = Iri.normalize (Iri.append_path iri [file]) in
            (* normalize will remove // *)
            Rdf_term.Iri iri
          in
          g.Rdf_graph.add_triple ~sub ~pred: pred_contains ~obj;
          Lwt.return_unit
      | _ -> Lwt.return_unit
  in
  try%lwt
    let%lwt () = Lwt_stream.iter_s (add_file g)
      (Lwt_unix.files_of_directory dirname)
    in
    g.Rdf_graph.add_triple ~sub ~pred: Rdf_rdf.type_
      ~obj:(Rdf_term.Iri Rdf_ldp.c_Container) ;
    g.Rdf_graph.add_triple ~sub ~pred: Rdf_rdf.type_
      ~obj:(Rdf_term.Iri Rdf_ldp.c_BasicContainer) ;
    Lwt.return g
  with e ->
      let%lwt () = Server_log._err_lwt
        (fun m -> m "%s: %s" dirname (Printexc.to_string e))
      in
      Lwt.return g

let create_container_graph p =
  match p.kind with
    Some `Dir -> container_graph_of_dir p.iri (path_to_filename p)
  | None | Some `File | Some `Acl | Some `Meta ->
      assert false

let path_is_container path =
  match path.kind with
    Some `Dir ->
      begin
        let meta = meta_path path in
        match%lwt read_path_graph meta with
          None -> Lwt.return_false
        | Some g ->
            Lwt.return (Ldp_http.is_container ~iri: path.iri g)
      end
  | _ -> Lwt.return_false

let iri_append_path iri strings =
  let p =
    match Iri.path iri with
      Iri.Absolute l | Iri.Relative l -> l
  in
  let p =
    match List.rev p with
    | "" :: q -> q
    | _ -> p
  in
  let p = List.rev p @ strings in
  let p =
    match Iri.path iri with
      Iri.Absolute _ -> Iri.Absolute p
    | Iri.Relative _ -> Iri.Relative p
  in
  Iri.with_path iri p

let available_dir_entry k =
  let rec iter dir slug cpt =
    let basename =
      if cpt = 0 then slug else Printf.sprintf "%s-%d" slug cpt
    in
    let file = Filename.concat dir basename in
    if%lwt Lwt_unix.file_exists file then
      iter dir slug (cpt+1)
    else
      Lwt.return basename
  in
  (* FIXME: concurrent calls with the same
     slug and directory could lead to return a filename
     already found but not yet created. *)
  fun ?(slug="") path ->
    match path.kind with
    | Some `Dir ->
        begin
          let dir = path_to_filename path in
          let%lwt basename = iter dir slug (if slug = "" then 1 else 0) in
          let rel = path.rel @ [ basename ] in
          let iri =
            let l = match k with
              | `Dir -> [basename ; ""]
              | _ -> [basename]
            in
            iri_append_path path.iri l
          in
          Lwt.return_some
            { root = path.root; rel ; iri ; kind = Some k ; mime = None }
        end
    | _ -> Lwt.return_none

let available_file = available_dir_entry `File
let available_dir = available_dir_entry `Dir

let bool_of_unix_call f arg =
  try%lwt let%lwt () = f arg in Lwt.return_true
  with Unix.Unix_error (e,s1,s2) ->
      let%lwt () = Server_log._err_lwt (fun m -> m "%s: %s %s"
         s1 (Unix.error_message e) s2)
      in
      Lwt.return_false

let safe_unlink abs_file =
  let%lwt _b = bool_of_unix_call Lwt_unix.unlink abs_file in
  Lwt.return_unit

let store_graph abs_file g =
  let str = Rdf_ttl.to_string g in
  bool_of_unix_call
    Lwt_io.(with_file ~mode: Output abs_file)
    (fun oc -> Lwt_io.write oc str)

let store_path_graph path g =
  let file = path_to_filename path in
  store_graph file g

let post_mkdir path meta_graph =
  let abs_dir = path_to_filename path in
  match%lwt bool_of_unix_call (Lwt_unix.mkdir abs_dir) 0o770 with
  | false -> Lwt.return_false
  | true ->
      let abs_meta = Filename.concat abs_dir meta_suffix in
      match%lwt store_graph abs_meta meta_graph with
        false ->
          let%lwt () = safe_unlink abs_meta in
          let%lwt () = safe_unlink abs_dir in
          Lwt.return_false
      | true ->
          let%lwt () = Server_log._info_lwt
            (fun f -> f "Container directory created: %s" abs_dir)
          in
          Lwt.return_true

let post_file path write =
  let abs_file = path_to_filename path in
  match%lwt bool_of_unix_call
    Lwt_io.(with_file ~mode:Output abs_file) write
  with
    false ->
      let%lwt () = safe_unlink abs_file in
      Lwt.return_false
  | true ->
      let%lwt () = Server_log._info_lwt
        (fun f -> f "File created: %s" abs_file)
      in
      Lwt.return_true

let sha256 str =
  let hash = Cryptokit.Hash.sha256 () in
  hash#add_string str ;
  let t = Cryptokit.Hexa.encode () in
  t#put_string hash#result ;
  t#get_string

let path_etag p =
  let file = path_to_filename p in
  try
    let%lwt st = Lwt_unix.stat file in
    let mtime = string_of_float st.Unix.st_mtime in
    Lwt.return_some (sha256 mtime)
  with _ -> Lwt.return_none

let date_rfc1123_fmt = "%a, %d %b %Y %T GMT"
let path_last_modified p =
  let file = path_to_filename p in
  try
    let%lwt st = Lwt_unix.stat file in
    let mtime = st.Unix.st_mtime in
    let t = CalendarLib.Fcalendar.from_unixfloat mtime in
    let t = CalendarLib.Fcalendar.to_gmt t in
    let str = CalendarLib.Printer.Fcalendar.sprint date_rfc1123_fmt t in
    Lwt.return_some str
  with _ -> Lwt.return_none
