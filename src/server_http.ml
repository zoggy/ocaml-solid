(** *)

module S = Cohttp_lwt_unix.Server

let reform_pem str =
  match Server_misc.split_string str [' '] with
  | beg::cert::q ->
      begin
        match List.rev q with
          cert2 :: en :: q ->
            let l = (beg ^ " " ^cert) ::
              (List.rev q) @
                [en ^ " " ^cert2]
            in
            String.concat "\n" l
        | _ -> str
      end
  | _ -> str

let server conf http_handler =
  let open Server_conf in
  let callback _conn req body =
    let headers = Cohttp.Request.headers req in
    let%lwt user =
      match Cohttp.Header.get headers conf.client_cert_header with
        None ->
          let%lwt () = Server_log._debug_lwt
            (fun m -> m "No %s header" conf.client_cert_header)
          in
          Lwt.return_none
      | Some str ->
          let str = reform_pem str in
          let%lwt () = Server_log._debug_lwt (fun m -> m "PEM: %s" str) in
          let cs = Cstruct.of_string str in
          try
            let c = X509.Encoding.Pem.Certificate.of_pem_cstruct1 cs in
            Server_auth.user_of_cert c
          with e ->
              Server_log._debug
                (fun m -> m "Error while reading client PEM: %s\n%s"
                   (Printexc.to_string e) (Cohttp.Header.to_string headers));
              Lwt.return_none
    in
    http_handler ?user req body
  in
  let conn_closed (_,id) = () in
  let config = S.make ~callback ~conn_closed () in
  let%lwt ctx = Conduit_lwt_unix.init ~src:conf.host() in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  let mode = `TCP (`Port conf.port) in
  S.create ~ctx ~mode config
