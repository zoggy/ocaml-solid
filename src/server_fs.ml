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

let kind root rel =
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
  let%lwt kind = kind root rel in
  let iri =
    let path =
      match kind with Some `Dir -> path @ [] | _ -> path
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
  { p with rel ; kind = Some kind ; mime }

let acl_path = ext_path acl_suffix `Acl
let meta_path = ext_path meta_suffix `Meta

(* CHOICE: acl are stored in turtle format *)
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
    match%lwt string_of_file filename with
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

let parent p =
  match List.rev p.rel with
    [] -> None
  | h :: q ->
      let rel = List.rev q in
      let iri = Iri.with_path p.iri (Iri.Absolute (rel @ [""])) in
      Some 
        { iri ; root = p.root ;
          rel ; kind = Some `Dir ; mime = None ;
        }

  