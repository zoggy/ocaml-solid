open Lwt.Infix

let usage = Printf.sprintf "Usage: %s [options] [args]\nwhere options are:" Sys.argv.(0)

let ldp_http ~cert ~priv_key ~cert_dir =
  let%lwt authenticator = X509_lwt.authenticator (`Ca_dir cert_dir) in
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
    ]
  in
  let args = ref [] in
  Arg.parse (tls_options @ options) (fun s -> args := s :: !args) usage ;
  let%lwt http = ldp_http ~cert: !cert_pem ~priv_key: !priv_key ~cert_dir: !server_cert_dir in
  Lwt.return (List.rev !args, http)


  