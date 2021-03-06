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

open Rdf_term
open Rdf_graph
open Rdf_pim.Open
open Rdf_acl.Open

open Server_openssl

let namespaces = [
    Rdf_pim.pim, "pim" ;
    Rdf_acl.acl, "acl" ;
  ]

let iri_dot = Iri.of_string "./"
let iri_empty = Iri.of_string ""
let iri_sharp str = Iri.of_string ("#"^str)

let re_var var = Re.(compile (str ("{"^var^"}")))

let vars_of_pem pem =
  let%lwt p = Server_openssl.info_from_pem pem in
  Lwt.return
    (p.pem_webid,
     [
       re_var "cert-modulus", p.pem_modulus ;
       re_var "cert-exponent", p.pem_exponent ;
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

let options =
  try Server_fs.options_of_json (`Assoc [])
  with Ocf.Error e ->
    prerr_endline (Ocf.string_of_error e);
    exit 1
module Options = (val options)
module Unix = Server_fs.Make_unix(Options)
module Fs = Server_fs.Make_fs(Unix)(Options)

let mk_root_acl root_path webid =
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "preparing root acl graph for %s" (Fs.path_to_filename root_path))
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
    (fun m -> m "creating acl file %s" (Fs.path_to_filename acl_path))
  in
  if%lwt Fs.store_path_graph acl_path g then
    Lwt.return_unit
  else
    Server_log._err_lwt
      (fun f -> f "Could not create %s" (Fs.path_to_filename acl_path))

let mk_templates root_path ~name ~cert_label ~vars ~profile webid =
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
  let%lwt () = Server_log._debug_lwt (fun f -> f "%s" "before replace_var") in
  let replace_var str (re, by) = Re.replace_string re ~all:true ~by str in
  let%lwt () = Server_log._debug_lwt (fun f -> f "%s" "after replace_var") in
  let mk (uri, mime, template) =
    let content = List.fold_left replace_var template vars in
    let%lwt (path,(module T)) = Server_fs.path_of_uri (Uri.of_string uri) in
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
    match%lwt Fs.put_file path ?mime
      (fun oc -> Lwt_io.write oc content)
    with
      false -> Server_log._err_lwt
        (fun f -> f "Could not create %s" (Fs.path_to_filename path))
    | true -> Lwt.return_unit
  in
  Lwt_list.iter_s mk templates

let create_cert ~webid ~cert_label prefix =
  let csrfile = prefix^".csr" in
  let pemfile = prefix^".pem" in
  let keyfile = prefix^".key" in
  let cfxfile = prefix^".cfx" in
  let%lwt () = Server_openssl.make_csr_and_key
    ~cn:cert_label ~webid ~csrfile ~keyfile
  in
  let%lwt () = Server_openssl.make_pem ~webid ~csrfile ~keyfile ~pemfile in
  let%lwt () = make_pkcs12 ~keyfile ~pemfile ~outfile:cfxfile in
  let%lwt () = Server_fs.safe_unlink csrfile in
  let%lwt () =
    Lwt_io.(write_line stdout
     (Printf.sprintf
      "Files created:\n\
        %s: self-signed user certificate\n\
        %s: private key\n\
        %s: pkcs12 certificate to import in the browser"
        pemfile keyfile cfxfile)
    )
  in
  Lwt.return pemfile

let add ?webid ?(name="User name") ?(cert_label=name) ?cert ~profile root_uri =
  let%lwt webid =
    match webid with
      None ->
        let iri = Iri.of_string root_uri in
        let iri = Iri.append_path iri ["profile"; "card"] in
        let iri = Iri.with_fragment iri (Some "me") in
        let%lwt () = Server_log._info_lwt
          (fun f -> f "No webid provided, using %s" (Iri.to_string iri))
        in
        Lwt.return iri
    | Some iri -> Lwt.return iri
  in
  let%lwt (webid, vars) =
    match cert with
    | None -> Lwt.return (webid, [])
    | Some x ->
        let%lwt (pem_webid, vars) =
          match x with
          | `Create prefix ->
              let%lwt pemfile = create_cert ~webid ~cert_label prefix in
              vars_of_pem pemfile
          | `Exists pemfile -> vars_of_pem pemfile
        in
        match pem_webid with
          None -> Lwt.fail_with "No webid in PEM file"
        | Some id -> Lwt.return (id, vars)
  in
  let%lwt (root_path,(module T)) = Server_fs.path_of_uri (Uri.of_string root_uri) in
  let root_dir = Fs.path_to_filename root_path in
  if Sys.file_exists root_dir then
    Lwt.fail_with (Printf.sprintf "Directory %s exists" root_dir)
  else
    let g = open_graph Server_fs.(iri (meta_path root_path)) in
    if%lwt Fs.post_mkdir root_path g then
      let%lwt () = mk_root_acl root_path webid in
      mk_templates (Iri.to_uri (Server_fs.iri root_path))
        ~name ~cert_label ~vars ~profile webid
    else
      Lwt.fail_with (Printf.sprintf "Directory %s not created" root_dir)

  