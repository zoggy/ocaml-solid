open Lwt.Infix

let usage = Printf.sprintf "Usage: %s [options] [args]\nwhere options are:" Sys.argv.(0)

let ldp_http_curl ~cert ~priv_key =
  let module P =
  struct
    let dbg = Lwt_io.write_line Lwt_io.stderr
    let cert = cert
    let key = priv_key
  end
  in
  let module H = Ldp_http.Http (Ldp_curl.Make(P)) in
  Lwt.return (module H : Ldp_http.Http)

let ldp_http_tls ~cert ~priv_key ~cert_dir =
  let%lwt authenticator = X509_lwt.authenticator `No_authentication_I'M_STUPID in
(*  let%lwt authenticator = X509_lwt.authenticator (`Ca_dir cert_dir) in*)
  let%lwt certificates = X509_lwt.private_of_pems ~cert ~priv_key >>=
    fun c -> Lwt.return (`Single c)
  in
  let module P =
  struct
    let dbg = Lwt_io.write_line Lwt_io.stderr
    let authenticator = authenticator
    let certificates = certificates
  end
  in
  let module H = Ldp_http.Http (Ldp_tls.Make(P)) in
  Lwt.return (module H : Ldp_http.Http)

let parse ?(options=[]) ?(usage=usage) () =
  let priv_key = ref "client.key" in
  let cert_pem = ref "client.pem" in
  let server_cert_dir = ref "certificates" in
  let curl = ref false in
  let tls_options =
    [ "--privkey", Arg.Set_string priv_key,
      Printf.sprintf " <file> read private client key from <file> (default is %s)"
        !priv_key;

      "--cert", Arg.Set_string cert_pem,
      Printf.sprintf " <file> read client certificate from pem <file> (default is %s)"
        !cert_pem ;

      "--certificates", Arg.Set_string server_cert_dir,
      Printf.sprintf
        " <dir> use certificates in <dir> to authenticate server (default is %s)"
        !server_cert_dir ;

      "--curl", Arg.Set curl,
        " use curl instead of cohttp+tls to connect" ;
    ]
  in
  let args = ref [] in
  Arg.parse (tls_options @ options) (fun s -> args := s :: !args) usage ;
  let%lwt http =
    if !curl then
      ldp_http_curl ~cert: !cert_pem ~priv_key: !priv_key
    else
      ldp_http_tls ~cert: !cert_pem ~priv_key: !priv_key ~cert_dir: !server_cert_dir
  in
  Lwt.return (List.rev !args, http)

let print_alert where alert =
  let msg = Printf.sprintf "TLS ALERT (%s): %s"
    where (Tls.Packet.alert_type_to_string alert)
  in
  Lwt_io.(write_line stderr msg)

let print_fail where fail =
  let msg = Printf.sprintf "TLS FAIL (%s): %s"
    where (Tls.Engine.string_of_failure fail)
  in
  Lwt_io.(write_line stderr msg)

let main ?options ?usage f =
  let main =
    try%lwt
      let%lwt (args, http) = parse ?options ?usage () in
      f args http
    with
    | Tls_lwt.Tls_alert alert ->
        print_alert "remote end" alert >>= fun () -> exit 1
    | Tls_lwt.Tls_failure alert ->
        print_fail "our end" alert >>= fun () -> exit 1
  in
  Lwt_main.run main
