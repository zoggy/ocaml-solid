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

let string_opt_of_file ?(on_err=fun _ -> Lwt.return_none) filename =
  try%lwt
    let%lwt str = Lwt_io.(with_file Input filename read) in
    Lwt.return_some str
  with e -> on_err e

let string_of_file ?(on_err=fun _ -> Lwt.return "") filename =
  try%lwt Lwt_io.(with_file Input filename read)
  with e -> on_err e

let has_suffix str suf =
  let len = String.length str in
  let len_suf = String.length suf in
  len >= len_suf && String.sub str (len - len_suf) len_suf = suf

let ends_with_dirsep str = has_suffix str Filename.dir_sep

let lookup_mime filename =
  if has_suffix filename ".ttl" then
    Ldp_http.mime_turtle
  else
    Magic_mime.lookup filename

type file_dir = [`File | `Dir | `Unknown | `UnknownDir]
type kind = [file_dir | `Acl of file_dir | `Meta of file_dir]

type path =
  {
    root_iri: Iri.t ;
    root_dir : string ;
    rel : string list ; (* must not start nor end with '/' *)
    mutable kind : kind ;
    mutable mime : string option ;
  }

let rec string_of_kind (k:kind) =
  match k with
  | `Acl x -> "`Acl "^(string_of_kind (x:>kind))
  | `Meta x -> "`Meta "^(string_of_kind (x:>kind))
  | `File -> "`File"
  | `Dir -> "`Dir"
  | `Unknown -> "`Unknown"
  | `UnknownDir -> "`UnknownDir"

type Ldp_types.error +=
  | Not_a_container of path
  | Invalid_dirname of string
  | Missing_route of Uri.t

let acl_suffix = ",acl"
let meta_suffix = ",meta"

let debug_p p =
  Server_log._debug_lwt
    (fun f -> f "Path: root_iri=%s\nroot_dir=%s\nrel=%s"
      (Iri.to_string p.root_iri) p.root_dir
      (String.concat Filename.dir_sep p.rel))

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
    | `UnknownDir | `Dir -> app iri [""]
    | `File -> iri
    | `Acl `Dir
    | `Acl `UnknownDir-> app iri [acl_suffix]
    | `Acl `File
    | `Acl `Unknown -> add_file_ext iri acl_suffix
    | `Meta `Dir
    | `Meta `UnknownDir -> app iri [meta_suffix]
    | `Meta `File
    | `Meta `Unknown -> add_file_ext iri meta_suffix

let kind p = p.kind

let ext_path (f : file_dir -> kind) p =
  match p.kind with
    (`Unknown | `UnknownDir | `Dir | `File) as x
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
  | `Unknown | `UnknownDir | `Dir | `File -> false
  | `Acl _ | `Meta _ -> true

let sha256 str =
  let hash = Cryptokit.Hash.sha256 () in
  hash#add_string str ;
  let t = Cryptokit.Hexa.encode () in
  t#put_string hash#result ;
  t#get_string

let date_rfc1123_fmt = "%a, %d %b %Y %T GMT"

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

module type P =
  sig
    val to_filename : string -> string list -> string
    val is_file : string -> bool option Lwt.t
    val file_exists : string -> bool Lwt.t
    val string_of_file : ?on_err: (exn -> string Lwt.t) -> string -> string Lwt.t
    val string_opt_of_file : ?on_err: (exn -> string option Lwt.t) -> string -> string option Lwt.t
    val stat_kind : string -> Unix.file_kind Lwt.t
    val stat_mtime : string -> float Lwt.t
    val dir_entries : string -> string Lwt_stream.t Lwt.t
    val store_graph : string -> Rdf_graph.graph -> bool Lwt.t
    val mkdir : string -> int -> bool Lwt.t
    val safe_unlink : string -> unit Lwt.t
    val unlink : string -> bool Lwt.t
    val rmdir : string -> bool Lwt.t
    val try_write_file : string ->
           (Lwt_io.output Lwt_io.channel -> unit Lwt.t) -> bool Lwt.t
  end

module type Options = sig val read_only : bool end

module Make_unix (O:Options) : P =
  struct
    let to_filename root_dir rel =
      List.fold_left Filename.concat root_dir rel
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

    let file_exists = Lwt_unix.file_exists
    let string_of_file = string_of_file
    let string_opt_of_file = string_opt_of_file

    let stat_kind fn =
      let%lwt st = Lwt_unix.stat fn in
      Lwt.return st.Unix.st_kind

    let stat_mtime fn =
      let%lwt st = Lwt_unix.stat fn in
      Lwt.return st.Unix.st_mtime

    let dir_entries dirname =
      Lwt.return (Lwt_unix.files_of_directory dirname)

    let safe_unlink = safe_unlink

    let unlink abs_file = bool_of_unix_call Lwt_unix.unlink abs_file
    let rmdir abs_file = bool_of_unix_call Lwt_unix.rmdir abs_file

    let store_graph abs_file g =
      let str = Rdf_ttl.to_string ~compact: true g in
      bool_of_unix_call
        Lwt_io.(with_file ~mode: Output abs_file)
        (fun oc -> Lwt_io.write oc str)

    let mkdir abs_dir = bool_of_unix_call (Lwt_unix.mkdir abs_dir)

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

  end

module type Fs =
  sig
    val path_exists : path -> bool Lwt.t
    val mk_path : Iri.t -> string -> string list -> path Lwt.t
    val read_path_graph : path -> Rdf_graph.graph option Lwt.t
    val parent : path -> path option Lwt.t
    val path_to_filename : path -> string
    val append_rel : path -> string list -> path Lwt.t
    val path_mime : path -> string Lwt.t
    val string_of_path : ?on_err:  (exn -> string Lwt.t) -> path -> string Lwt.t
    val string_opt_of_path : ?on_err:  (exn -> string option Lwt.t) -> path -> string option Lwt.t
    val default_container_listing : path -> (path -> bool Lwt.t) -> string Lwt.t
    val store_path_graph : path -> Rdf_graph.graph -> bool Lwt.t
    val path_etag : ?accept:string -> path -> string option Lwt.t
    val path_last_modified : path -> string option Lwt.t
    val path_is_container : path -> bool Lwt.t
    val path_can_be_deleted : path -> bool Lwt.t
    val available_dir : ?slug:string -> path -> path option Lwt.t
    val available_file : ?slug:string -> path -> path option Lwt.t
    val post_mkdir : path -> Rdf_graph.graph -> bool Lwt.t
    val post_file : path ->
      ?mime:string -> (Lwt_io.output Lwt_io.channel -> unit Lwt.t) -> bool Lwt.t
    val put_file : path ->
      ?mime:string -> (Lwt_io.output Lwt_io.channel -> unit Lwt.t) -> bool Lwt.t
    val delete_path : path -> bool Lwt.t
    val create_container_graph :
      path -> (path -> bool Lwt.t) -> Rdf_graph.graph Lwt.t
  end

module Make_fs (P:P) (O:Options) : Fs =
  struct
    include O
    (*let ends_with_dirsep f =
      let str = Fpath.to_string f in
      let len = String.length str in
      len > 0 && has_suffix str Fpath.dir_sep*)

    let get_kind ~root_dir ~rel =
      let fname = P.to_filename root_dir rel in
      if has_suffix fname acl_suffix then
        let nosuf = Filename.chop_suffix fname acl_suffix in
        match%lwt P.is_file nosuf with
        | None when ends_with_dirsep nosuf -> Lwt.return (`Acl `UnknownDir)
        | None -> Lwt.return (`Acl `Unknown)
        | Some true -> Lwt.return (`Acl `File)
        | Some false -> Lwt.return (`Acl `Dir)
      else
        if has_suffix fname meta_suffix then
          let nosuf = Filename.chop_suffix fname meta_suffix in
          match%lwt P.is_file nosuf with
          | None when ends_with_dirsep nosuf -> Lwt.return (`Meta `UnknownDir)
          | None -> Lwt.return (`Meta `Unknown)
          | Some true -> Lwt.return (`Meta `File)
          | Some false -> Lwt.return (`Meta `Dir)
        else
          match%lwt P.is_file fname with
          | None when ends_with_dirsep fname -> Lwt.return `UnknownDir
          | None -> Lwt.return `Unknown
          | Some true -> Lwt.return `File
          | Some false -> Lwt.return `Dir


    let mk_path root_iri root_dir rel =
      let%lwt () = Server_log._debug_lwt
        (fun f -> f "mk_path: root_iri=%S, root_dir=%s, rel=%S"
           (Iri.to_string root_iri)
           root_dir (String.concat "|" rel)
        )
      in
      let%lwt kind = get_kind ~root_dir ~rel in
      let%lwt () = Server_log._debug_lwt
        (fun f -> f "kind = %s" (string_of_kind kind))
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

    (* CHOICE: acl and meta are stored in turtle format *)
    let read_graph base filename =
      match%lwt P.file_exists filename with
      | false -> Lwt.return_none
      | true ->
          let on_err e =
            let%lwt () = Server_log._warn_lwt
              (fun f -> f "Could not read graph from %s: %s"
                 filename (Printexc.to_string e))
            in Lwt.return_none
          in
          match%lwt P.string_opt_of_file filename with
            None -> Lwt.return_none
          | Some str ->
              let g = Rdf_graph.open_graph base in
              try Rdf_ttl.from_string g str;
                Lwt.return (Some g)
              with e ->
                  let%lwt _ = on_err e in
                  Lwt.return_none

    let path_to_filename p =
      let fname = P.to_filename p.root_dir p.rel in
      match p.kind with
        `Unknown -> fname
      | `UnknownDir | `Dir -> fname ^ "/"
      | `File -> fname
      | `Acl `Dir
      | `Acl `UnknownDir -> Filename.concat fname acl_suffix
      | `Acl `File
      | `Acl `Unknown -> fname ^ acl_suffix
      | `Meta `Dir
      | `Meta `UnknownDir -> Filename.concat fname meta_suffix
      | `Meta `File
      | `Meta `Unknown -> fname ^ meta_suffix

    let read_path_graph p = read_graph (iri p) (path_to_filename p)

    let lookup_path_mime p = lookup_mime (path_to_filename p)

    let path_mime p =
      match p.mime with
        Some m -> Lwt.return m
      | None ->
          let meta = meta_path p in
          let%lwt mime =
            match%lwt read_path_graph meta with
              None -> Lwt.return_none
            | Some g ->
                let open Rdf_term in
                match Rdf_graph.(literal_objects_of g
                   ~sub:(Iri (iri p)) ~pred:Rdf_dc.format)
                with
                  [] -> Lwt.return_none
                | lit :: _ -> Lwt.return_some lit.lit_value
          in
          let mime =
            match mime with
              None -> lookup_path_mime p
            | Some m -> m
          in
          p.mime <- Some mime;
          Lwt.return mime

    let string_of_path ?on_err p =
      let absfile = path_to_filename p in
      P.string_of_file ?on_err absfile

    let string_opt_of_path ?on_err p =
      let absfile = path_to_filename p in
      P.string_opt_of_file ?on_err absfile

    let append_rel p strings = mk_path p.root_iri p.root_dir (p.rel @ strings)

    let parent p =
      match List.rev p.rel, p.kind with
        [], _ -> Lwt.return_none
      | h :: q, (`Dir | `File) ->
          let rel = List.rev q in
          Lwt.return_some { p with rel ; kind = `Dir ; mime = None }
      | h :: q, (`Unknown | `UnknownDir | `Acl `Unknown | `Acl `File | `Meta `Unknown | `Meta `File) ->
          let rel = List.rev q in
          let%lwt kind = get_kind ~root_dir:p.root_dir ~rel in
          Lwt.return_some { p with rel ; kind ; mime = None }
      | _, (`Acl `Dir | `Meta `Dir) ->
          Lwt.return_some { p with kind = `Dir ; mime = None }
      | _, (`Acl `UnknownDir | `Meta `UnknownDir) ->
          Lwt.return_some { p with kind = `UnknownDir ; mime = None }

    let container_graph_of_dir base_path dirname can_read =
      (* List only items the user has read access, waiting for
         more precise authorizations; see https://github.com/solid/solid/issues/29
         *)
      let base_iri = iri base_path in
      let g = Rdf_graph.open_graph base_iri in
      let sub = Rdf_term.Iri base_iri in
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
          match%lwt P.stat_kind absname with
          | exception e ->
             let%lwt () = Server_log._err_lwt
               (fun m -> m "%s: %s" absname (Printexc.to_string e))
             in
             Lwt.return_unit
         | Unix.S_REG | Unix.S_DIR ->
            let%lwt path_item = append_rel base_path [file] in
            if%lwt can_read path_item then
              begin
                let iri_item = iri path_item in
                let%lwt () = Server_log._debug_lwt
                  (fun f -> f "user can read %s" (Iri.to_string iri_item))
                in
                let obj = Rdf_term.Iri iri_item in
                g.Rdf_graph.add_triple ~sub ~pred: pred_contains ~obj;
                Lwt.return_unit
              end
            else
              Lwt.return_unit
         | _ -> Lwt.return_unit
      in
      try%lwt
        let%lwt () =
          P.dir_entries dirname >>=
            Lwt_stream.iter_s (add_file g)
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

    let create_container_graph p can_read =
      match p.kind with
        `Dir -> container_graph_of_dir p (path_to_filename p) can_read
      | _ -> assert false

    let path_is_container path =
      match path.kind with
        `Dir ->
          begin
            let meta = meta_path path in
            match%lwt read_path_graph meta with
          None ->
                let%lwt () = Server_log._debug_lwt
                  (fun f -> f "%s is not a container because it has not meta graph"
                     (String.concat "|" path.rel))
                in
                Lwt.return_false
            | Some g ->
                Lwt.return (Ldp_http.is_container ~iri: (iri path) g)
          end
      | _ ->
          let%lwt () = Server_log._debug_lwt
            (fun f -> f "%s not a container because not a directory"
               (String.concat "|" path.rel))
          in
          Lwt.return_false

    let available_dir_entry k =
      let rec iter dir slug ext cpt =
        let basename =
          if cpt = 0
          then slug^ext
          else Printf.sprintf "%s-%d%s" slug cpt ext
        in
        let file = Filename.concat dir basename in
        if%lwt P.file_exists file then
          iter dir slug ext (cpt+1)
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
              let (slug, ext) =
                try
                  let p = String.rindex slug '.' in
                  let len = String.length slug in
                  String.sub slug 0 p, String.sub slug p (len-p)
                with Not_found -> slug, ""
              in
              let%lwt basename = iter dir slug ext (if slug = "" then 1 else 0) in
              let rel = path.rel @ [ basename ] in
              Lwt.return_some
                { root_dir = path.root_dir; rel ;
                  root_iri = path.root_iri ;
                  kind = k ; mime = None }
            end
        | _ -> Lwt.return_none

    let available_file = available_dir_entry `File
    let available_dir = available_dir_entry `Dir

    let store_path_graph path g =
      let file = path_to_filename path in
      P.store_graph file g

    let post_mkdir path meta_graph =
      let abs_dir = path_to_filename path in
      match%lwt P.mkdir abs_dir 0o770 with
      | false -> Lwt.return_false
      | true ->
          let abs_meta = Filename.concat abs_dir meta_suffix in
          match%lwt P.store_graph abs_meta meta_graph with
            false ->
              let%lwt () = P.safe_unlink abs_meta in
              let%lwt () = P.safe_unlink abs_dir in
              Lwt.return_false
          | true ->
              path.kind <- `Dir ;
              let%lwt () = Server_log._info_lwt
                (fun f -> f "Directory created: %s" abs_dir)
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
      let%lwt ok = P.try_write_file abs_file write in
      let%lwt () =
        match mime with
        | Some mime when ok && not (is_graph_path path) ->
            let%lwt () = Server_log._debug_lwt
              (fun f -> f "%s is graph path: %b"
                 abs_file (is_graph_path path))
            in
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
          | `Unknown | `UnknownDir ->
              parent path >>= list (path :: acc)
      in
      let rec create = function
        [] -> Lwt.return_true
      | p::q ->
          (* set kind to `Dir to create iri with ending / *)
          let p = { p with  kind = `Dir } in
          let base = iri p in
          let g = Rdf_graph.open_graph base in
          g.Rdf_graph.add_triple
            ~sub: (Rdf_term.Iri base)
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
        | l ->
          let%lwt () = Server_log._debug_lwt
           (fun f -> f "mkdirp: create [%s]"
             (String.concat ", "
              (List.map (fun p -> String.concat "|" p.rel) l)))
          in
          create l

    let put_file path ?mime write =
      let%lwt ok =
        match path.kind with
          `Dir -> Lwt.return false
        | `Unknown | `UnknownDir
        | `Acl (`Unknown | `UnknownDir)
        | `Meta (`Unknown | `UnknownDir) ->
            if%lwt parent path >>= mkdirp then
              post_file path ?mime write
            else
              Lwt.return_false
        | `File | `Acl (`File | `Dir) | `Meta (`File | `Dir) ->
            post_file path ?mime write
      in
      if ok then
        (* update kind if it was unknown *)
        match path.kind with
          `Unknown ->
            let%lwt kind = get_kind ~root_dir: path.root_dir ~rel:path.rel in
            path.kind <- kind ;
            Lwt.return ok
        | _ -> Lwt.return ok
      else
        Lwt.return ok

    let path_etag ?(accept="") p =
      let file = path_to_filename p in
      try
        let%lwt mtime = P.stat_mtime file in
        let mtime = string_of_float mtime in
        Lwt.return_some (sha256 (mtime^accept))
      with _ -> Lwt.return_none

    let path_last_modified p =
      let file = path_to_filename p in
      try
        let%lwt mtime = P.stat_mtime file in
        let t = CalendarLib.Fcalendar.from_unixfloat mtime in
        let t = CalendarLib.Fcalendar.to_gmt t in
        let str = CalendarLib.Printer.Fcalendar.sprint date_rfc1123_fmt t in
        Lwt.return_some str
      with _ -> Lwt.return_none

    (* TODO: abstract this function with P *)
    let path_can_be_deleted p =
      match p.kind with
      | `Unknown | `UnknownDir -> Lwt.return_false
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
      | `Unknown | `UnknownDir -> Lwt.return_false
      | `Acl _
      | `Meta _ -> P.unlink (path_to_filename p)
      | `File ->
          let%lwt () = P.safe_unlink (path_to_filename (acl_path p)) in
          let%lwt () = P.safe_unlink (path_to_filename (meta_path p)) in
          P.unlink (path_to_filename p)
      | `Dir ->
          let%lwt () = P.safe_unlink (path_to_filename (acl_path p)) in
          let%lwt () = P.safe_unlink (path_to_filename (meta_path p)) in
          P.rmdir (path_to_filename p)

    let default_container_listing path can_read =
      let open Rdf_graph in
      let open Rdf_term in
      let module Xh = Xtmpl_xhtml in
      let%lwt g = create_container_graph path can_read in
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

    let path_exists p = P.file_exists (path_to_filename p)
  end

module type T =
  sig
    module Options : Options
    module Fs : Fs
    module Fs_acl : Fs
  end

let make_t o fs fs_acl =
  let module M =
  struct
    module Options = (val o : Options)
    module Fs = (val fs : Fs)
    module Fs_acl = (val fs_acl : Fs)
  end
  in
  Lwt.return (module M : T)

module SMap = Map.Make(String)

type fs_type_fun =
  root_iri: Iri.t -> root_dir:string -> rel:string list ->
    options: Yojson.Safe.json -> (module T) Lwt.t

let fs_types = ref (SMap.empty : fs_type_fun SMap.t)
let register_fs_type name f = fs_types := SMap.add name f !fs_types

type fs_options =
  {
    read_only : bool [@ocf Ocf.Wrapper.bool, false] ;
  } [@@ocf]

let options_of_json json =
  let t = fs_options_wrapper.Ocf.Wrapper.from_json json in
  let module M =
  struct
    let read_only = t.read_only
  end
  in
  (module M : Options)


let unix_fun ~root_iri ~root_dir ~rel ~options =
  let o = options_of_json options in
  let module O = (val o) in
  let module U = Make_unix (O) in
  let module Fs = Make_fs (U) (O) in
  make_t o (module Fs) (module Fs)

let () = register_fs_type "unix" unix_fun

let path_of_uri uri =
  match Server_fs_route.route (Ocf.get Server_conf.storage_root) uri with
  | None -> Ldp_types.fail (Missing_route uri)
  | Some (root_iri_path, root_dir, rel, fs_type, options) ->
      let%lwt f =
        try Lwt.return (SMap.find fs_type !fs_types)
        with Not_found ->
            Lwt.fail_with (Printf.sprintf "Unknown fs type %S" fs_type)
      in
      let root_iri =
        Iri.iri ?scheme:(Uri.scheme uri)
          ?user:(Uri.user uri)
          ?host:(Uri.host uri)
          ?port:(Uri.port uri)
          ~path:(Iri.Absolute root_iri_path) ()
      in
      let%lwt t =
        try%lwt f ~root_iri ~root_dir ~rel ~options
        with Ocf.Error e -> Lwt.fail_with (Ocf.string_of_error e)
      in
      let module T = (val t) in
      let%lwt p = T.Fs.mk_path root_iri root_dir rel in
      Lwt.return (p, t)

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


