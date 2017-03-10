(** *)

open Rdf_acl.Open
open Rdf_graph
open Rdf_term

(** See {{:https://github.com/solid/web-access-control-spec#individual-resource-acls} Solid WAC} *)

type rights = int
let no_right = 0

let add_read = (lor) 1
let rem_read = (land) (lnot 1)
let has_read r = r land 1 <> 0
let add_write = (lor) 2
let rem_write = (land) (lnot 2)
let has_write r = r land 2 <> 0
let add_append = (lor) 4
let rem_append = (land) (lnot 4)
let has_append r = r land 4 <> 0
let add_control = (lor) 8
let rem_control = (land) (lnot 8)
let has_control r = r land 8 <> 0

(* FIXME: acl:defautlForNew will be renamed acl:default *)
let auths ~default g iri =
  let filter a =
    let pred =
      if default then acl_defaultForNew else acl_accessTo
    in
    g.exists ~sub: (Iri a) ~pred ~obj:(Iri iri) ()
  in
  List.filter filter
    (iri_subjects_of g ~pred:Rdf_dc.type_ ~obj: (Iri acl_c_Authorization))

let add_rights_of_modes =
  List.fold_left
    (fun acc mode ->
       if Iri.equal mode acl_c_Read then
         add_read acc
       else if Iri.equal mode acl_c_Write then
          add_write acc
         else if Iri.equal mode acl_c_Append then
             add_append acc
           else if Iri.equal mode acl_c_Control then
               add_control acc
             else acc)

let gather_rights g user acc auth =
  let sub = Iri auth in
  let modes = iri_objects_of g ~sub ~pred:acl_mode in
  let for_any = g.exists ~sub ~pred:acl_agent ~obj:(Iri (Rdf_foaf.c_Agent)) () in
  let for_user =
    match user with
      None -> false
    | Some user -> g.exists ~sub ~pred:acl_agent ~obj:(Iri user) ()
  in
  if for_any || for_user then
    add_rights_of_modes acc modes
  else
    acc

(** FIXME: we don't handle groups of agent yet *)
(* FIXME: hand control access over ,acl *)
let rights ~default user g iri =
  let auths = auths ~default g iri in
(*  let%lwt () =

  in*)
  List.fold_left (gather_rights g user) no_right auths

let rights_for_path user p =
  let rec iter ~default p =
    let acl = Server_fs.acl_path p in
    match%lwt Server_fs.read_path_graph acl with
    | Some g -> Lwt.return (rights ~default user g (Server_fs.iri p))
    | None ->
        match Server_fs.parent p with
          None -> Server_log._err
            (fun m -> m "No root acl in %s" (Server_fs.path_to_filename p));
            Lwt.return no_right
        | Some parent -> iter ~default: true parent
  in
  iter ~default: false p
        