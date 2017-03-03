open Lwt
open Cohttp
open Cohttp_tls

let server_cert = "./server-certificates/server.pem"
let server_key = "./server-certificates/server.key"

let uid_asn_oid = "0.9.2342.19200300.100.1.1"

let get_cert_info cert =
  let sub = X509.subject cert in
  let issuer = X509.issuer cert in
  Printf.sprintf "Subject: %s\nIssuer: %s\nHostnames: %s"
    (X509.distinguished_name_to_string sub)
    (X509.distinguished_name_to_string issuer)
    (String.concat ", " (X509.hostnames cert))

let server =
  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun cert ->
    X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
  let tls_server = Tls.Config.server
    ~reneg:true
    ~certificates: (`Single cert)
    ~authenticator ()
  in
  let callback _conn req body =
    let (tls_session,_) = _conn in
    let%lwt info_conn =
      match Tls_lwt.Unix.epoch tls_session with
        `Error -> Lwt.return "TLS epoch: `Error"
      | `Ok epoch_data ->
          let open Tls.Core in
          match epoch_data.peer_certificate with
            None -> Lwt.return "TLS epoch: no peer certificate"
          | Some c -> Lwt.return
            (Printf.sprintf "TLS epoch: %s" (get_cert_info c))
    in
    let uri = req |> Request.uri in
    let uri_s = uri |> Uri.to_string in
    (*match Uri.path uri with
        "/private" ->
          let t = Tls_lwt.reneg tls_session
    >>= fun () ->*)
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nInfo_conn: %s\nHeaders: %s\nBody: %s"
         uri_s meth info_conn headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
(*  let mode = `TLS (
     `Crt_file_path "server-certificates/server.pem",
     `Key_file_path "server-certificates/server.key",
     `No_password,
     `Port 9999)
  in*)
  Server.create (*~mode*) ~port: 9999 tls_server (Server.make ~callback())

