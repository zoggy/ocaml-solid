(** *)
open Lwt.Infix

module type Wm = sig
  module Rd = Webmachine.Rd
  include Webmachine.S with type 'a io = 'a Lwt.t
  end

module Wm : Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_tls.IO)
end

let log f = Server_log._debug_lwt f

class r user =
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    method allowed_methods rd = Wm.continue [`GET ; `POST] rd
    method content_types_provided rd =
      let%lwt () = log (fun f -> f "content_types_provided") in
      Wm.continue [
        ("application/xhtml+xml", self#to_html);
        ("application/json", self#to_json);
      ] rd

    method content_types_accepted rd =
      Wm.continue [] rd

    method private to_html rd =
      let%lwt () = log (fun f -> f "to_html") in
      Wm.continue (`String "<html><body>hello</body></html>") rd

    method private to_json rd =
      let%lwt () = log (fun f -> f "to_json") in
      let json = {| { error : "json interface not implemented yet" } |} in
      let body = `String json in
      Wm.continue body { rd with Wm.Rd.resp_body = body }
  end

let http_handler (user, request) body =
  let req_uri = Cohttp.Request.uri request in
  let%lwt () = log (fun f ->
       f "HTTP query: %s" (Uri.to_string req_uri))
  in
  let open Cohttp in
  let routes = [ "", fun () -> new r user ] in
  Wm.dispatch' routes ~body ~request
  >|= begin function
    | None        -> (`Not_found, Header.init (), `String "Not found", [])
    | Some result -> result
       end
       >>= fun (status, headers, body, path) ->
       let%lwt () = log (fun f ->
           f "HTTP decision path: %s" (String.concat " -> " path))
      in
      Cohttp_tls.Server.respond ~headers ~body ~status ()
