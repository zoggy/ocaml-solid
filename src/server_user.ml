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

let vars_of_pem pem =
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "Extracing pubkey from %s" pem)
  in
  let pubkey = Filename.temp_file "oss" "pubkey" in
  let command = Printf.sprintf
    "openssl x509 -pubkey -noout -in %s > %s"
    (Filename.quote pem) (Filename.quote pubkey)
  in
  match Sys.command command with
    n when n <> 0 -> Lwt.fail_with ("Command failed: "^command)
  | _ ->
      let%lwt () = Server_log._debug_lwt
        (fun m -> m "Extracing modulus from %s" pubkey)
      in
      let command = ("",
         [| "openssl" ; "rsa" ; "-in" ; pubkey ; "-pubin" ;
          "-inform" ; "PEM" ; "-modulus" ; "-noout" |])
      in
      let%lwt str = Lwt_process.pread command in
      let re = Re.(compile (seq
          [
            str "Modulus=" ;
            group (rep1 (alt [alpha;digit])) ;
            eol ;
          ]))
      in
      let groups = Re.exec re str in
      let modulus = Re.Group.get groups 1 in

      let%lwt () = Server_log._debug_lwt
        (fun m -> m "Extracing exponent from %s" pubkey)
      in
      let command = ("",
         [| "openssl" ; "rsa" ; "-in" ; pubkey ; "-pubin" ;
          "-inform" ; "PEM" ; "-text" ; "-noout" |])
      in
      let%lwt str = Lwt_process.pread command in
      let re = Re.(compile (seq
          [
            str "Exponent: " ;
            group (rep1 digit) ;
            char ' ' ;
          ]))
      in
      let groups = Re.exec re str in
      let exponent = Re.Group.get groups 1 in
      let%lwt () = Server_fs.safe_unlink pubkey in

      let%lwt () = Server_log._debug_lwt
        (fun m -> m "Extracing Subject Alternative Name from %s" pem)
      in
      let command =
        ("", [| "openssl" ; "x509" ; "-noout" ; "-in" ; pem ; "-text" |])
      in
      let%lwt str = Lwt_process.pread command in
      let re = Re.(compile (seq
          [
            str "X509v3 Subject Alternative Name:";
            rep (set " \t\n\r");
            str "URI:" ;
            group (rep1 notnl) ;
          ]))
      in
      let webid =
        try
          let groups = Re.exec re str in
          Some (Re.Group.get groups 1)
        with
          _ -> None
      in
      let%lwt () = Server_log._debug_lwt
        (fun m -> m "modulus=%s\nexponent=%s\nwebid=%s"
           modulus exponent
             (match webid with None -> "NONE" | Some s -> s))
      in
      Lwt.return
        (webid,
         [
           re_var "cert-modulus", modulus ;
           re_var "cert-exponent", exponent ;
         ]
        )

let templates iri =
  [
    iri^"Applications/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Applications/,acl"] ;

    iri^"Inbox/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Inbox/,acl"] ;

    iri^"settings/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/settings/,acl"] ;

    iri^"settings/preferences.ttl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/settings/preferences.ttl"] ;

    iri^"Private/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Private/,acl"] ;

    iri^"Public/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Public/,acl"] ;

    iri^"Shared/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Shared/,acl"] ;

    iri^"Work/,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/Work/,acl"] ;
  ]

let profile_templates iri =
  [
    iri^"profile/card", Some Ldp_http.mime_turtle,
    [%blob "user_templates/profile/card"] ;

    iri^"profile/card,acl", Some Ldp_http.mime_turtle,
    [%blob "user_templates/profile/card,acl"] ;
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

let mk_templates root_path ?(name="Your name")
  ?(cert_label="Cert label") ~vars ~profile webid =
  let templates = templates root_path in
  let templates =
    if profile then templates @ profile_templates root_path else templates
  in
  let vars = vars @
    [
      re_var "webid", Iri.to_string webid ;
      re_var "name", name ;
      re_var "cert-label", cert_label ;
    ]
  in
  let replace_var str (re, by) = Re.replace_string re ~all:true ~by str in
  let mk (uri, mime, template) =
    let content = List.fold_left replace_var template vars in
    let%lwt path = Server_fs.path_of_uri (Uri.of_string uri) in
    let%lwt () =
      match mime with
      Some s when s = Ldp_http.mime_turtle ->
          begin
            (* test graph syntax *)
            let iri = Server_fs.iri path in
            let g = open_graph iri in
            try Lwt.return (Rdf_ttl.from_string g content)
            with e -> Server_log._err_lwt
                (fun f -> f "%s: %s" (Iri.to_string iri) (Printexc.to_string e))
          end
      | _ -> Lwt.return_unit
    in
    match%lwt Server_fs.put_file path ?mime
      (fun oc -> Lwt_io.write oc content)
    with
      false -> Server_log._err_lwt
        (fun f -> f "Could not create %s" (Server_fs.path_to_filename path))
    | true -> Lwt.return_unit
  in
  Lwt_list.iter_s mk templates

let add ?webid ?name ?cert_label ?pem ~profile login =
  let%lwt (webid, vars) =
    match pem with
      None -> Lwt.return (webid, [])
    | Some file -> vars_of_pem file
  in
  let%lwt webid =
    match webid with
      None -> Lwt.fail_with "missing webid"
    | Some str -> Lwt.return (Iri.of_string str)
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
      mk_templates (Iri.to_uri (Server_fs.iri root_path))
        ?name ?cert_label ~vars ~profile webid
    else
      Lwt.fail_with (Printf.sprintf "Directory %s not created" root_dir)

  