(*********************************************************************************)
(*                OCaml-Solid                                                    *)
(*                                                                               *)
(*    Copyright (C) 2016-2017 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** *)

open Lwt.Infix

let homes () =
  Filename.concat (Ocf.get Server_conf.storage_root) "home"

let documents () =
  Filename.concat (Ocf.get Server_conf.storage_root) "documents"

let string_of_file ?(on_err=fun _ -> Lwt.return_none) filename =
  try%lwt
    let%lwt str = Lwt_io.(with_file Input filename read) in
    Lwt.return_some str
  with e -> on_err e

type file_dir = [`File | `Dir | `Unknown]
type kind = [file_dir | `Acl of file_dir | `Meta of file_dir]
type path =
  {
    root_iri: Iri.t ;
    root_dir : string ;
    rel : string list ; (* must not start nor end with '/' *)
    mutable kind : kind ;
    mutable mime : string option ;
  }

type Ldp_types.error +=
  | Not_a_container of path
  | Invalid_dirname of string
  | Missing_route of Uri.t

let acl_suffix = ",acl"
let meta_suffix = ",meta"

let is_file fname =
  let%lwt r =
    match%lwt Lwt_unix.stat fname with
    | exception _ -> Lwt.return_none
    | { Unix.st_kind = Unix.S_REG } -> Lwt.return_some true
    | { Unix.st_kind = Unix.S_DIR } -> Lwt.return_some false
    | _ -> Lwt.return_none
  in
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "%s is_file: %s"
       fname (match r with None -> "NONE" | Some true -> "true" | Some false -> "false")
    )
  in
  Lwt.return r

let get_kind fname =
  match%lwt is_file fname with
  | None -> Lwt.return `Unknown
  | Some true when Filename.check_suffix fname acl_suffix ->
      begin
        match%lwt is_file (Filename.chop_suffix fname acl_suffix) with
          None -> Lwt.return (`Acl `Unknown)
        | Some true -> Lwt.return (`Acl `File)
        | Some false -> Lwt.return (`Acl `Dir)
      end
  | Some true when Filename.check_suffix fname meta_suffix ->
      begin
        match%lwt is_file (Filename.chop_suffix fname meta_suffix) with
          None -> Lwt.return (`Meta `Unknown)
        | Some true -> Lwt.return (`Meta `File)
        | Some false -> Lwt.return (`Meta `Dir)
      end
  | Some true -> Lwt.return `File
  | Some false -> Lwt.return `Dir

let debug_p p =
  Server_log._debug_lwt
    (fun f -> f "Path: root_iri=%s\nroot_dir=%s\nrel=%s"
      (Iri.to_string p.root_iri) p.root_dir
      (List.fold_left Filename.concat "/" p.rel))

let mk_path root_iri root_dir rel =
  let%lwt () = Server_log._debug_lwt
    (fun f -> f "mk_path: root_iri=%S, root_dir=%s, rel=%S"
       (Iri.to_string root_iri) root_dir (String.concat "|" rel)
     )
  in
  let%lwt kind =
    let fname = List.fold_left Filename.concat root_dir rel in
    get_kind fname
  in
  let rel =
    let chop_ext =
      match kind with
        `Acl _ -> Some acl_suffix
      | `Meta _ -> Some meta_suffix
      | _ -> None
    in
    match chop_ext with
      None -> rel
    | Some ext ->
        match List.rev rel with
          [] -> assert false
        | h :: q ->
            match Filename.chop_suffix h ext with
            | "" -> List.rev q
            | str -> List.rev (str :: q)
  in
  let p = { root_iri ; root_dir ; rel ; kind ; mime = None } in
  let%lwt () = debug_p p in
  Lwt.return p

let path_of_uri uri =
  match Server_fs_route.route (Ocf.get Server_conf.storage_root) uri with
  | None -> Ldp_types.fail (Missing_route uri)
  | Some (root_iri_path, root_dir, rel, ro) ->
      let root_iri =
        Iri.iri ?scheme:(Uri.scheme uri)
          ?user:(Uri.user uri)
          ?host:(Uri.host uri)
          ?port:(Uri.port uri)
          ~path:(Iri.Absolute root_iri_path) ()
      in
      let%lwt p = mk_path root_iri root_dir rel in
      Lwt.return (p, ro)

let append_rel p strings = mk_path p.root_iri p.root_dir (p.rel @ strings)

let path_to_filename p =
  let fname = List.fold_left Filename.concat p.root_dir p.rel in
  match p.kind with
    `Unknown -> fname
  | `Dir -> fname ^ "/"
  | `File -> fname
  | `Acl `Dir -> Filename.concat fname acl_suffix
  | `Acl `File
  | `Acl `Unknown -> fname ^ acl_suffix
  | `Meta `Dir -> Filename.concat fname meta_suffix
  | `Meta `File
  | `Meta `Unknown -> fname ^ meta_suffix

let iri =
  let add_file_ext iri ext =
    let add p =
      match List.rev p with
        [] -> [ext]
      | h::q -> List.rev ((h^ext) :: q)
    in
    let path =
      match Iri.path iri with
        Iri.Absolute p -> Iri.Absolute (add p)
      | Iri.Relative p -> Iri.Relative (add p)
    in
    Iri.with_path iri path
  in
  fun p ->
    let app = Iri.append_path in
    let iri = app p.root_iri p.rel in
    match p.kind with
      `Unknown -> iri
    | `Dir -> app iri [""]
    | `File -> iri
    | `Acl `Dir -> app iri [acl_suffix]
    | `Acl `File
    | `Acl `Unknown -> add_file_ext iri acl_suffix
    | `Meta `Dir -> app iri [meta_suffix]
    | `Meta `File
    | `Meta `Unknown -> add_file_ext iri meta_suffix

let kind p = p.kind

let () = Ldp_types.register_string_of_error
  (fun fb -> function
     | Not_a_container path ->
         Printf.sprintf "Not a container: %s" (Iri.to_string (iri path))
     | Invalid_dirname s ->
         Printf.sprintf "Invalid directory name: %s" s
     | Missing_route uri ->
         Printf.sprintf "Missing FS route for %s" (Uri.to_string uri)
     | e -> fb e
  )

(*
let remove_ext p =
  match p.kind with
    Some `Dir -> p
  | _ ->
      match List.rev p.rel with
        [] -> p
      | h :: q ->
          let h2 =
            if Filename.check_suffix h acl_suffix then
              Filename.chop_suffix h acl_suffix
            else if Filename.check_suffix h meta_suffix then
                Filename.chop_suffix h meta_suffix
              else h
          in
          if h = h2 then
            p
          else
        let rel = List.rev (h2 :: q) in
        let path =
          match Iri.path p with
            Iri.Absolute p | Iri.relation p ->
              match List.rev p with
                [] -> assert false
              |*)

let ext_path (f : file_dir -> kind) p =
  match p.kind with
    (`Unknown | `Dir | `File) as x
  | `Meta x | `Acl x ->
      let kind =  f x in
      if kind = p.kind then
        p
      else
        { p with kind = f x ; mime = None }

let acl_path = ext_path (fun x -> `Acl x)
let meta_path = ext_path (fun x -> `Meta x)
let noext_path = ext_path (fun x -> (x:>kind))

let is_graph_path p =
  match p.kind with
  | `Unknown | `Dir | `File -> false
  | `Acl _ | `Meta _ -> true

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
  read_graph (iri p) (path_to_filename p)

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
             ~sub:(Iri (iri p)) ~pred:Rdf_dc.format)
          with
            [] -> Lwt.return_none (* FIXME: try to guess with magic-mime ? *)
          | lit :: _ ->
              p.mime <- Some lit.lit_value ;
              Lwt.return (Some lit.lit_value)

(*
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
*)

let parent p =
  let mk_filename rel = List.fold_left Filename.concat p.root_dir rel in
  match List.rev p.rel, p.kind with
    [], _ -> Lwt.return_none
  | h :: q, (`Dir | `File) ->
      let rel = List.rev q in
      Lwt.return_some { p with rel ; kind = `Dir ; mime = None }
  | h :: q, (`Unknown | `Acl `Unknown | `Acl `File | `Meta `Unknown | `Meta `File) ->
      let rel = List.rev q in
      let%lwt kind = get_kind (mk_filename rel) in
      Lwt.return_some { p with rel ; kind ; mime = None }
  | _, (`Acl `Dir | `Meta `Dir) ->
      Lwt.return_some { p with kind = `Dir ; mime = None }

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
                [file;""]
              else
                [file]
            in
            let iri = Iri.normalize (Iri.append_path iri file) in
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
    `Dir -> container_graph_of_dir (iri p) (path_to_filename p)
  | _ -> assert false

let path_is_container path =
  match path.kind with
    `Dir ->
      begin
        let meta = meta_path path in
        match%lwt read_path_graph meta with
          None -> Lwt.return_false
        | Some g ->
            Lwt.return (Ldp_http.is_container ~iri: (iri path) g)
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
    | `Dir ->
        begin
          let dir = path_to_filename path in
          let%lwt basename = iter dir slug (if slug = "" then 1 else 0) in
          let rel = path.rel @ [ basename ] in
          let root_iri =
            let l = match k with
              | `Dir -> [basename ; ""]
              | _ -> [basename]
            in
            iri_append_path (iri path) l
          in
          Lwt.return_some
            { root_dir = path.root_dir; rel ; root_iri ; kind = k ; mime = None }
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
  let str = Rdf_ttl.to_string ~compact: true g in
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
          path.kind <- `Dir ;
          let%lwt () = Server_log._info_lwt
            (fun f -> f "Directory created: %s" abs_dir)
          in
          Lwt.return_true

let try_write_file abs_file write =
  match%lwt bool_of_unix_call
    Lwt_io.(with_file ~mode:Output abs_file) write
  with
    false ->
      let%lwt () = safe_unlink abs_file in
      Lwt.return_false
  | true ->
      let%lwt () = Server_log._info_lwt
        (fun f -> f "File written: %s" abs_file)
      in
      Lwt.return_true

let set_path_format path mime =
  let meta_path = meta_path path in
  let%lwt g =
    match%lwt read_path_graph meta_path with
    | None -> Lwt.return (Rdf_graph.open_graph (iri meta_path))
    | Some g -> Lwt.return g
  in
  let sub = Rdf_term.Iri (iri path) in
  let pred = Rdf_dc.format in
  let old = g.Rdf_graph.objects_of ~sub ~pred in
  List.iter (fun obj -> g.Rdf_graph.rem_triple ~sub ~pred ~obj) old;
  g.Rdf_graph.add_triple ~sub ~pred
    ~obj:(Rdf_term.term_of_literal_string mime);
  let%lwt _ok = store_path_graph meta_path g in
  Lwt.return_unit

let post_file path ?mime write =
  let abs_file = path_to_filename path in
  let%lwt ok = try_write_file abs_file write in
  let%lwt () =
    match mime with
    | Some mime when ok && not (is_graph_path path) ->
        set_path_format path mime
    | _ -> Lwt.return_unit
  in
  Lwt.return ok

let rec mkdirp =
  let rec list acc = function
  | None -> Lwt.return acc
  | Some path ->
      match path.kind with
      | `Dir -> Lwt.return acc
      | `File | `Acl _| `Meta _ ->
          Ldp_types.fail (Not_a_container path)
      | `Unknown ->
          parent path >>= list (path :: acc)
  in
  let rec create = function
    [] -> Lwt.return_true
  | p::q ->
      let g = Rdf_graph.open_graph (iri p) in
      g.Rdf_graph.add_triple
        ~sub: (Rdf_term.Iri (iri p))
        ~pred: Rdf_rdf.type_
        ~obj:(Rdf_term.Iri Rdf_ldp.c_BasicContainer);
      if%lwt post_mkdir p g then
        create q
      else
        Lwt.return_false
  in
  fun path ->
    match%lwt list [] path with
    | exception e -> Lwt.fail e
    | l -> create l

let put_file path ?mime write =
  let%lwt ok =
    match path.kind with
      `Dir -> Lwt.return false
    | `File | `Acl _ | `Meta _ ->
        post_file path ?mime write
    | `Unknown ->
        if%lwt parent path >>= mkdirp then
          post_file path ?mime write
        else
          Lwt.return_false
  in
  if ok then
    (* update kind if it was unknown *)
    match path.kind with
      `Unknown ->
        let%lwt kind = get_kind
          (List.fold_left Filename.concat path.root_dir path.rel)
        in
        path.kind <- kind ;
        Lwt.return ok
    | _ -> Lwt.return ok
  else
    Lwt.return ok

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

let path_can_be_deleted p =
  match p.kind with
  | `Unknown -> Lwt.return_false
  | `Acl _
  | `Meta _
  | `File -> Lwt.return_true
  | `Dir ->
      let abs = path_to_filename p in
      let open Lwt_unix in
      match%lwt opendir abs with
      | exception _ -> Lwt.return_false
      | h ->
          let rec iter () =
            match%lwt readdir h with
            | exception _ -> Lwt.return_true
            | entry when
                  entry = Filename.current_dir_name
                  || entry = Filename.parent_dir_name
                  || entry = acl_suffix
                  || entry = meta_suffix -> iter ()
            | _ -> Lwt.return_false
          in
          let%lwt empty = iter () in
          let%lwt () = closedir h in
          Lwt.return empty

let delete_path p =
  match p.kind with
  | `Unknown -> Lwt.return_false
  | `Acl _
  | `Meta _ -> bool_of_unix_call Lwt_unix.unlink (path_to_filename p)
  | `File ->
      let%lwt () = safe_unlink (path_to_filename (acl_path p)) in
      let%lwt () = safe_unlink (path_to_filename (meta_path p)) in
      bool_of_unix_call Lwt_unix.unlink (path_to_filename p)
  | `Dir ->
      let%lwt () = safe_unlink (path_to_filename (acl_path p)) in
      let%lwt () = safe_unlink (path_to_filename (meta_path p)) in
      bool_of_unix_call Lwt_unix.rmdir (path_to_filename p)

let default_container_listing path =
  let open Rdf_graph in
  let open Rdf_term in
  let module Xh = Xtmpl_xhtml in
  let%lwt g = create_container_graph path in
  let iri = iri path in
  let title_of iri =
    let sub = Iri iri in
    match literal_objects_of g ~sub ~pred: Rdf_dc.title with
      lit :: _ -> lit.lit_value
    | _ ->
        let p = match Iri.path iri with
            | Iri.Absolute p | Iri.Relative p -> p
        in
        match List.rev p with
          [] | [""] -> ""
        | "" :: s :: _ -> s
        | s :: _ -> s
  in
  let lis =
    let sub = Iri iri in
    let hrefs = iri_objects_of g ~sub ~pred:Rdf_ldp.contains in
    List.map (fun href ->
       let title = title_of href in
       Xh.li [Xh.a ~href:(Iri.to_string href) [Xtmpl_rewrite.cdata title]])
      hrefs
  in
  let title = title_of iri in
  let contents =
    [ Xh.h1 [Xtmpl_rewrite.cdata title] ;
      Xh.ul lis ;
    ]
  in
  Lwt.return (Server_page.page title contents)
