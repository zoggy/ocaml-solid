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

class r (user:Iri.t option) path =
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    method allowed_methods rd = Wm.continue [`GET ; `POST] rd

    method is_authorized rd = Wm.continue `Authorized rd
(*    type auth =
    [ `Authorized
    | `Basic of string
    | `Redirect of Uri.t
]*)

    method forbidden rd =
      (* FIXME: handle permissions here, according to user,
         path and method (GET, ...) *)
      let%lwt rights = Server_perm.rights_for_path user path in
      let%lwt () = Server_log._debug_lwt
        (fun m -> m "rights %d to user %s on %s"
          rights
          (match user with None -> "NONE" | Some iri -> Iri.to_string iri)
          (Iri.to_string (Server_fs.iri path)))
      in
      let ok =
        match rd.Wm.Rd.meth with
          `GET | `OPTIONS | `HEAD -> Server_perm.has_read rights
        | `POST -> Server_perm.has_write rights || Server_perm.has_append rights
        | `PUT | `DELETE | `PATCH -> Server_perm.has_write rights
        | _ -> true
      in
      Wm.continue (not ok) rd

    method moved_temporarily rd =
      Wm.continue None rd

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

let http_handler ?user request body =
  let uri = Cohttp.Request.uri request in
  let uri = match Uri.scheme uri with
      None -> Uri.with_scheme uri (Some "https")
    | Some _ -> uri
  in
  let uri = Uri.canonicalize uri in
  let%lwt () = log (fun f ->
       f "HTTP query: %s" (Uri.to_string uri))
  in
  let%lwt path = Server_fs.path_of_uri uri in
  let%lwt () =
    log (fun f ->
       f "Iri: %s\nFilename: %s"
         (Iri.to_string (Server_fs.iri path))
         (Server_fs.path_to_filename path))
  in
  let open Cohttp in
  let routes =
    [ "*", fun () -> new r user path ]
  in
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
