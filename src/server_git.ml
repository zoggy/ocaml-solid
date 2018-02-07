open Lwt.Infix
open Server_fs
open Git_unix
open Git_unix.Value_IO
module Search = Git.Search.Make(FS)

type path = { path : Server_fs.path ; commit : Git.Hash.Commit.t }

type Ldp_types.error +=
| Missing_git_reference of Server_fs.path
| Invalid_git_reference of Server_fs.path * string
| File_not_found of Server_fs.path * string list * Hash_IO.t
| Invalid_git_path of Server_fs.path * string list  * Hash_IO.t

let () = Ldp_types.register_string_of_error
  (fun fb -> function
     | Missing_git_reference path ->
         Printf.sprintf "Missing git reference: %s" (Iri.to_string (Server_fs.iri path))
     | Invalid_git_reference (path, str) ->
         Printf.sprintf "Invalid git reference: %s (%s)"
           str (Iri.to_string (Server_fs.iri path))
     | File_not_found (path, file, hash) ->
         Printf.sprintf "File not found: %s [%s] (%s)"
           (String.concat "/" file)
           (Hash_IO.to_hex hash)
           (Iri.to_string (Server_fs.iri path))
     | Invalid_git_path (path, file, hash) ->
         Printf.sprintf "File not found: %s [%s] (%s)"
           (String.concat "/" file)
           (Hash_IO.to_hex hash)
           (Iri.to_string (Server_fs.iri path))
     | e -> fb e
  )

module Wm = Server_webmachine.Wm

let git_read git path file hash =
    Search.find git hash (`Commit (`Path file)) >>= function
    | None     -> Ldp_types.fail (File_not_found (path, file, hash))
    | Some sha -> FS.read_exn git sha >>= function
      | Git.Value.Blob b -> Lwt.return (`Blob (Git.Blob.to_raw b))
      | Git.Value.Tree t -> Lwt.return (`Tree)
      | _ -> Ldp_types.fail (Invalid_git_path (path, file, hash))
 ;;

class r user path git hash master =
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided rd =
      Wm.continue ["*/*", self#read_resource] rd
    method content_types_accepted rd = Wm.continue [] rd

    method private read_resource rd =
      let file = path.rel in
      match%lwt git_read git path file hash with
      | `Blob body -> Wm.continue (`String body) rd
      | `Tree -> Wm.continue (`String "Tree") rd

TODO: abstraire l'accès aux fichiers dans server_fs et utiliser
des foncteurs dans server_fs et serveur_acl pour les utiliser
ici et dans server_webmachine
  end

let r user path =
  match path.rel with
  | [] -> Ldp_types.fail (Missing_git_reference path)
  | r :: rel ->
      let%lwt git = FS.create ~root: path.root_dir () in
      let path = { path with rel } in
      let%lwt hash =
        try%lwt FS.read_reference_exn git
          (Git.Reference.of_raw ("refs/heads/"^r))
        with _ ->
           try Lwt.return (Git_unix.Hash_IO.of_hex r)
           with exn -> Ldp_types.fail (Invalid_git_reference (path, r))
      in
      let%lwt master = FS.read_reference_exn git Git.Reference.master in
      Lwt.return (new r user path git hash master)
