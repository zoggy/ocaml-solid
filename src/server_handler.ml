open Lwt.Infix

let log = Server_webmachine.log

let http_handler ?user request body =
  let uri = Cohttp.Request.uri request in
  (* since webmachine does not support PATCH yet, map it
    to PUT but pass the real method in paramter
    of our resource *)
  let met = Cohttp.Request.meth request in
  let request =
    match met with
      `PATCH -> { request with Cohttp.Request.meth = `PUT }
    | _ -> request
  in
  let uri = match Uri.scheme uri with
      None -> Uri.with_scheme uri (Some "https")
    | Some _ -> uri
  in
  let uri = Uri.canonicalize uri in
  let%lwt () = log (fun f ->
       f "HTTP query: %s" (Uri.to_string uri))
  in
  let%lwt routes =
    match%lwt Server_fs.path_of_uri uri with
    | exception e ->
        let%lwt () = log (fun f -> f "%s" (Printexc.to_string e)) in
        Lwt.return []
    | (path, (module T)) ->
        let%lwt () =
          log (fun f ->
             f "Iri: %s\nFilename: %s"
               (Iri.to_string (Server_fs.iri path))
               (T.Fs.path_to_filename path))
        in
        let module Acl = Server_acl.Make(T.Fs_acl) in
        let module W = Server_webmachine.Make(T.Fs)(Acl) in
        Lwt.return [ "*", fun () -> 
            new W.r met ~read_only: T.Options.read_only user path ]
  in
  let open Cohttp in
  Server_webmachine.Wm.dispatch' routes ~body ~request
  >|= begin function
    | None        -> (`Not_found, Header.init (), `String "Not found", [])
    | Some result -> result
       end
       >>= fun (status, headers, body, path) ->
       let%lwt () = log (fun f ->
           f "HTTP decision path: %s" (String.concat " -> " path))
      in
      Cohttp_tls.Server.respond ~headers ~body ~status ()
