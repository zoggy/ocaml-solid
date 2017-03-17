(** *)

open Rdf_term
open Rdf_graph
open Rdf_pim.Open
open Rdf_acl.Open

let namespaces = [
    Rdf_pim.pim, "pim" ;
    Rdf_acl.acl, "acl" ;
  ]

let iri_dot = Iri.of_string "./"
let iri_empty = Iri.of_string ""
let iri_sharp str = Iri.of_string ("#"^str)

let mk_root_acl root_path webid =
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "preparing root acl graph for %s" (Server_fs.path_to_filename root_path))
  in
  let acl_path = Server_fs.acl_path root_path in
  let g = open_graph (Server_fs.iri acl_path) in
  g.set_namespaces namespaces ;
  let sub = Iri (iri_sharp "owner") in
  let add (pred, objs) =
    List.iter (fun obj -> g.add_triple ~sub ~pred ~obj) objs in
  List.iter add [
    Rdf_rdf.type_, [ Iri acl_c_Authorization ] ;
    acl_accessTo, [ Iri iri_dot ] ;
    acl_agent, [ Iri webid ] ;
    acl_defaultForNew, [ Iri iri_dot ] ;
    acl_mode, [ Iri acl_c_Control ; Iri acl_c_Read ; Iri acl_c_Write ] ;
  ] ;
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "creating acl file %s" (Server_fs.path_to_filename acl_path))
  in
  Server_fs.store_path_graph acl_path g

let add ?webid login =
  let%lwt webid =
    match webid with
    | Some iri -> Lwt.return iri
    | None -> Lwt.fail_with "missing webid"
  in
  (* FIXME: change when virtual host will be handled in config file *)
  let root_uri = Printf.sprintf "https://localhost:%d/~%s/"
    (Ocf.get Server_conf.port) login
  in
  let%lwt root_path = Server_fs.path_of_uri (Uri.of_string root_uri) in
  let root_dir = Server_fs.path_to_filename root_path in
  if Sys.file_exists root_dir then
    Lwt.fail_with (Printf.sprintf "Directory %s exists" root_dir)
  else
    let g = open_graph Server_fs.(iri (meta_path root_path)) in
    if%lwt Server_fs.post_mkdir root_path g then
      let%lwt b = mk_root_acl root_path webid in
      Lwt.return_unit
    else
      Lwt.fail_with (Printf.sprintf "Directory %s not created" root_dir)

  