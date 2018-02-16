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

open Lwt
open Cohttp
open Cohttp_tls

let iri_subject_alt_names cert =
  List.fold_left
    (fun acc -> function
       | `URI str -> (Iri.of_string str) :: acc
       | _ -> acc
    ) [] (X509.Extension.subject_alt_names cert)

let get_cert_info cert =
  Printf.sprintf "Subject Alternative Name (IRI): %s"
    (String.concat ", " (List.map Iri.to_string (iri_subject_alt_names cert)))
(*
  let sub = X509.subject cert in
  let issuer = X509.issuer cert in
  Printf.sprintf "Subject: %s\nIssuer: %s\nSubject_alt_names(URI): %s"
    (X509.distinguished_name_to_string sub)
    (X509.distinguished_name_to_string issuer)
    (String.concat ", " (List.map Iri.to_string (iri_subject_alt_names cert)))
*)

(* FIXME: add user authentication with cookies here *)

let server conf http_handler =
  let open Server_conf in
  let%lwt cert =
    X509_lwt.private_of_pems
      ~cert:conf.server_cert
      ~priv_key:conf.server_key
  in
  let%lwt authenticator =
    X509_lwt.authenticator
     (match conf.server_ca with
         None -> `No_authentication_I'M_STUPID
       | Some file -> `Ca_file file)
  in
  let tls_server = Tls.Config.server
    ~reneg:true
    ~certificates: (`Single cert)
    ~authenticator ()
  in
  let callback _conn req body =
    let (tls_session,_) = _conn in
    let%lwt user =
      match Tls_lwt.Unix.epoch tls_session with
        `Error -> Lwt.return_none
      | `Ok epoch_data ->
          let open Tls.Core in
          match epoch_data.peer_certificate with
            None -> Lwt.return_none
          | Some c ->
              let%lwt () =
                Server_log._debug_lwt
                  (fun m -> m "Info conn: %s"
                     (Printf.sprintf "TLS epoch: %s" (get_cert_info c))
                  )
              in
              Server_auth.user_of_cert c
    in
    let uri = req |> Request.uri in
    let uri_s = uri |> Uri.to_string in
    let%lwt () = Server_log._debug_lwt
      (fun m -> m "New query: %s %s"
         (req |> Request.meth |> Code.string_of_method) uri_s
      )
    in
    http_handler ?user req body
    (*match Uri.path uri with
        "/private" ->
          let t = Tls_lwt.reneg tls_session
    >>= fun () ->*)
    (*
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nInfo_conn: %s\nHeaders: %s\nBody: %s"
         uri_s meth info_conn headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
    *)
  in
(*  let mode = `TLS (
     `Crt_file_path "server-certificates/server.pem",
     `Key_file_path "server-certificates/server.key",
     `No_password,
     `Port 9999)
  in*)
  Server.create (*~mode*)
    ~port: conf.port
    tls_server (Server.make ~callback())

