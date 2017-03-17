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

let re_var var = Re.(compile (str ("{"^var^"}")))

let templates iri =
  [ iri^"Preferences/prefs.ttl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Preferences/prefs.ttl"] ;

    iri^"Private/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Private/,acl"] ;

    iri^"Applications/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Applications/,acl"] ;

    iri^"Public/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Public/,acl"] ;

    iri^"Inbox/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Inbox/,acl"] ;

    iri^"Shared/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Shared/,acl"] ;

    iri^"Work/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Work/,acl"] ;

  ]

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
  if%lwt Server_fs.store_path_graph acl_path g then
    Lwt.return_unit
  else
    Server_log._err_lwt
      (fun f -> f "Could not create %s" (Server_fs.path_to_filename acl_path))

let mk_templates root_path webid =
  let templates = templates root_path in
  let vars = [
      re_var "webid", Iri.to_string webid ;
    ]
  in
  let replace_var str (re, by) = Re.replace_string re ~all:true ~by str in
  let mk (uri, mime, template) =
    let content = List.fold_left replace_var template vars in
    let%lwt path = Server_fs.path_of_uri (Uri.of_string uri) in
    let () =
      match mime with
      Some s when s = Ldp_http.mime_turtle ->
          begin
            (* test graph syntax *)
            let iri = Server_fs.iri path in
            let g = open_graph iri in
            try Rdf_ttl.from_string g content
            with e -> Server_log._err
                (fun f -> f "%s: %s" (Iri.to_string iri) (Printexc.to_string e))
          end
      | _ -> ()
    in
    match%lwt Server_fs.put_file path ?mime
      (fun oc -> Lwt_io.write oc content)
    with
      false -> Server_log._err_lwt
        (fun f -> f "Could not create %s" (Server_fs.path_to_filename path))
    | true -> Lwt.return_unit
  in
  Lwt_list.iter_p mk templates

let add ?webid login =
  let%lwt webid =
    match webid with
    | Some iri -> Lwt.return iri
    | None -> Lwt.fail_with "missing webid"
  in
  (* FIXME: change when virtual hosts will be handled in config file *)
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
      let%lwt () = mk_root_acl root_path webid in
      mk_templates (Iri.to_uri (Server_fs.iri root_path)) webid
    else
      Lwt.fail_with (Printf.sprintf "Directory %s not created" root_dir)

  