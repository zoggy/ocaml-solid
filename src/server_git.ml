open Lwt.Infix
open Server_fs
open Git_unix
open Git_unix.Value_IO
module Search = Git.Search.Make(FS)

type path = { path : Server_fs.path ; commit : Git.Hash.Commit.t }

let str_opt = function None -> "" | Some s -> s
let map_opt f def = function None -> def | Some x -> f x

type Ldp_types.error +=
| Missing_git_reference of Iri.t * string list
| Invalid_git_reference of Iri.t * string
| File_not_found of string list * Hash_IO.t option
| Invalid_git_path of string list  * Hash_IO.t option

let () = Ldp_types.register_string_of_error
  (fun fb -> function
     | Missing_git_reference (iri, rel) ->
         Printf.sprintf "Missing git reference: %s (%s)"
           (String.concat "/" rel) (Iri.to_string iri)
     | Invalid_git_reference (iri, str) ->
         Printf.sprintf "Invalid git reference: %s (%s)"
           str (Iri.to_string iri)
     | File_not_found (file, hash) ->
         Printf.sprintf "File not found: %s [%s]"
           (String.concat "/" file)
           (map_opt Hash_IO.to_hex "" hash)
     | Invalid_git_path (file, hash) ->
         Printf.sprintf "File not found: %s [%s]"
           (String.concat "/" file)
           (map_opt Hash_IO.to_hex "" hash)
     | e -> fb e
  )

module Wm = Server_webmachine.Wm

let git_read git file hash =
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "git_read %S" (String.concat "|" file))
  in
  match%lwt Search.find git hash (`Commit (`Path file)) with
  | None -> Ldp_types.fail (File_not_found (file, Some hash))
  | Some sha -> FS.read_exn git sha >>= function
      | Git.Value.Blob b -> Lwt.return (`Blob (Git.Blob.to_raw b))
      | Git.Value.Tree t -> Lwt.return (`Tree t)
      | _ -> Ldp_types.fail (Invalid_git_path (file, Some hash))
 ;;

module type Ref = sig
    val git : Git_unix.FS.t
    val hash : Git_unix.Hash_IO.t
  end

module type Options =
  sig
    include Server_fs.Options
    val bare : bool
  end
module Make (O:Options) (R:Ref) : Server_fs.P =
  struct
    let to_filename root_dir = function
    | "tags" :: _ :: q
    | _ :: q -> String.concat Filename.dir_sep q
    | _ -> ""

    let to_file = function
    | "" -> Lwt.return []
    | s -> Lwt.return (Server_fs_route.split_string s ['/'])

    let file_exists file =
      let%lwt file = to_file file in
      match%lwt Search.find R.git R.hash (`Commit (`Path file)) with
      | None   -> Lwt.return_false
      | Some _ -> Lwt.return_true

    let is_file file_ =
      let%lwt file = to_file file_ in
      match%lwt Search.find R.git R.hash (`Commit (`Path file)) with
      | None ->
          let%lwt () = Server_log._debug_lwt
            (fun m -> m "is_file %S [%s]: None" (file_)
             (Hash_IO.to_hex R.hash))
          in
          Lwt.return_none
      | Some sha -> FS.read_exn R.git sha >>= function
          | Git.Value.Blob b -> Lwt.return_some true
          | _ -> Lwt.return_some false

    let string_of_file ?(on_err=fun _ -> Lwt.return "") file =
      let%lwt file = to_file file in
      match%lwt git_read R.git file R.hash with
      | `Blob str -> Lwt.return str
      | _ -> on_err (Ldp_types.Error (File_not_found (file, Some R.hash)))
      | exception e -> on_err e

    let string_opt_of_file ?(on_err=fun _ -> Lwt.return_none) file =
      let%lwt file = to_file file in
      match%lwt git_read R.git file R.hash with
      | `Blob str -> Lwt.return_some str
      | _ -> on_err (Ldp_types.Error (File_not_found (file, Some R.hash)))
      | exception e -> on_err e

    let stat_kind file =
      let%lwt file = to_file file in
      match%lwt Search.find R.git R.hash (`Commit (`Path file)) with
      | None -> Ldp_types.fail (File_not_found (file, Some R.hash))
      | Some sha -> FS.read_exn R.git sha >>= function
          | Git.Value.Blob b -> Lwt.return Unix.S_REG
          | Git.Value.Tree t -> Lwt.return Unix.S_DIR
          | _ -> Ldp_types.fail (Invalid_git_path (file, Some R.hash))

    let stat_mtime file =
      match%lwt FS.read R.git R.hash with
        Some (Git.Value.Commit c) ->
          let a = c.Git.Commit.author in
          let (d, _) = a.Git.User.date in
          let%lwt () = Server_log._debug_lwt
            (fun m -> m "commit date for %S: %s" file (Int64.to_string d))
          in
          Lwt.return (Int64.to_float d)
      | _ ->
          let%lwt () = Server_log._debug_lwt
            (fun m -> m "no mtime found for %S" file)
          in
          Lwt.return 0.0

    let dir_entries dir =
      let%lwt dir = to_file dir in
      match%lwt git_read R.git dir R.hash with
      | `Tree entries ->
          let names = List.map (fun e -> e.Git.Tree.name) entries in
          Lwt.return (Lwt_stream.of_list names)
      | _ -> Lwt.return (Lwt_stream.of_list [])

    let store_graph _ = assert false
    let store_graph _ = assert false
    let mkdir _ = assert false
    let safe_unlink _ = assert false
    let unlink _ = assert false
    let rmdir _ = assert false
    let try_write_file _ = assert false
  end

let git_fun ~root_iri ~root_dir ~rel ~options =
  let o = Server_fs.options_of_json options in
  let bare = Ocf.bool ~doc: "bare git repository" false in
  let g = Ocf.add Ocf.group ["bare"] bare in
  ignore(Ocf.from_json g options);
  let module O =
  struct
    include (val o)
    let read_only = true
    let bare = Ocf.get bare
  end
  in
  let%lwt git =
    let dot_git = if O.bare then Some root_dir else None in
    FS.create ~root: root_dir ?dot_git ()
  in
  let%lwt hash =
    match rel with
    | [] -> Ldp_types.fail (Missing_git_reference (root_iri, rel))
    | "tags" :: tag :: _ ->
        begin
          try%lwt
            let%lwt hash = FS.read_reference_exn git
              (Git.Reference.of_raw ("refs/tags/"^tag))
            in
            (* if it is a tag object, get the object hash *)
            match%lwt FS.read git hash with
            | Some (Git.Value.Tag t) -> Lwt.return t.Git.Tag.obj
            | _ -> Lwt.return hash
          with _ -> Ldp_types.fail (Invalid_git_reference (root_iri, tag))
        end
    | r :: rel ->
        try%lwt FS.read_reference_exn git
          (Git.Reference.of_raw ("refs/heads/"^r))
        with _ ->
            try Lwt.return (Git_unix.Hash_IO.of_hex r)
            with exn -> Ldp_types.fail (Invalid_git_reference (root_iri, r))
  in
  let%lwt master = FS.read_reference_exn git Git.Reference.master in
  let module R = Make (O) (struct let git = git let hash = hash end) in
  let module Fs = Server_fs.Make_fs (R) (O) in
  let module R_acl = Make (O) (struct let git = git let hash = master end) in
  let module Fs_acl = Server_fs.Make_fs (R_acl) (O) in
  Server_fs.make_t (module O) (module Fs) (module Fs_acl)

let () = Server_fs.register_fs_type "git" git_fun
