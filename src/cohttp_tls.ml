(* from cohttp_lwt_unix_io *)

open Lwt.Infix

module IO =
  struct
    module CD = Cohttp_lwt_unix_debug
    let () =
      if Sys.os_type <> "Win32" then
        Sys.(set_signal sigpipe Signal_ignore);

    type 'a t = 'a Lwt.t
    let (>>=) = Lwt.bind
    let return = Lwt.return

    type ic = Lwt_io.input_channel
    type oc = Lwt_io.output_channel
    type conn = Tls_lwt.Unix.t

    let src = Logs.Src.create "cohttp.lwt" ~doc:"Cohttp Lwt IO module"
    module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

    let read_line ic =
      if CD.debug_active () then
        Lwt_io.read_line_opt ic >>= function
        | None ->
            Log.debug (fun f -> f  "<<< EOF")
              >>= fun () ->  Lwt.return_none
                | Some l as x ->
                  Log.debug (fun f -> f  "<<< %s" l)
                  >>= fun () ->  Lwt.return x
      else
        Lwt_io.read_line_opt ic
    
    let read ic count =
      let count = min count Sys.max_string_length in
      if CD.debug_active () then
        Lwt_io.read ~count ic
          >>= fun buf ->
            Log.debug (fun f -> f  "<<<[%d] %s" count buf)
              >>= fun () -> return buf
      else
        Lwt_io.read ~count ic
    
    let write oc buf =
      if CD.debug_active () then (
         Log.debug (fun f -> f  ">>> %s" (String.trim buf)) >>= fun () ->
           Lwt_io.write oc buf
        )
      else (
         ( Lwt_io.write oc buf )
        )

    let flush oc =
      Lwt_io.flush oc
    
end

module Request = struct
  include Cohttp.Request
  include (Make(IO)
           : module type of Make(IO) with type t := t)
end

module Response = struct
  include Cohttp.Response
  include (Make(IO)
           : module type of Make(IO) with type t := t)
end

module Server_core = Cohttp_lwt.Make_server (IO)

module Server = struct
  include Server_core
        (* from conduit_lwt_tls, but Lwt.return (t, ic, oc)
          instead of Lwt.return (fd, ic, oc) *)
      let init' ?backlog ?stop ?timeout tls sa callback =
        sa
          |> Conduit_lwt_server.listen ?backlog
          |> Conduit_lwt_server.init ?stop (fun (fd, _) ->
             Lwt.try_bind
               (fun () -> Tls_lwt.Unix.server_of_fd tls fd)
               (fun t ->
                  let (ic, oc) = Tls_lwt.of_t t in
                  Lwt.return (t, ic, oc))
               (fun exn -> Lwt_unix.close fd >>= fun () -> Lwt.fail exn)
               >>= Conduit_lwt_server.process_accept ?timeout callback)

        let init ?backlog ~certfile ~keyfile ?stop ?timeout sa callback =
          X509_lwt.private_of_pems ~cert:certfile ~priv_key:keyfile
            >>= fun certificate ->
          let config = Tls.Config.server ~certificates:(`Single certificate) () in
          init' ?backlog ?stop ?timeout config sa callback

    (* /conduit_lwt_tls *)

    (* from conduit_lwt_unix.ml *)

    let sockaddr_on_tcp_port sa port =
      let open Unix in
      match sa with
      | Some (ADDR_UNIX _) -> failwith "Cant listen to TCP on a domain socket"
      | Some (ADDR_INET (a,_)) -> ADDR_INET (a,port), Ipaddr_unix.of_inet_addr a
      | None -> ADDR_INET (inet_addr_any,port), Ipaddr.(V4 V4.any)

    let create ?timeout ?stop ?on_exn ?sockaddr ?(port=9999) tls http_server =
      let sa, ip = sockaddr_on_tcp_port sockaddr port in
      init' ?stop ?timeout tls sa (callback http_server)
  end

