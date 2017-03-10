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

let mime_xhtml = "application/xhtml+xml"
let mime_xhtml_charset = "application/xhtml+xml;charset=utf-8"
let mime_html = "text/html"

let error_page title message =
  let module Xh = Xtmpl_xhtml in
  let module X = Xtmpl_rewrite in
  let xml =
    Xh.html
      ~atts: (X.atts_one ("","xmlns") [X.cdata "http://www.w3.org/1999/xhtml"])
      [
        Xh.header
          [
            Xh.title [X.cdata title] ;
            Xh.meta ~atts:(X.atts_of_list
             [ ("","http-equiv"), [X.cdata "Content-Type"] ;
               ("","content"), [X.cdata mime_xhtml_charset] ;
             ]) [] ;
          ];
        Xh.body [X.cdata message] ;
      ]
  in
  X.to_string [xml]

let error_rd rd title message =
  let body = error_page title message in
  let rd = { rd with Wm.Rd.resp_body = `String body } in
  let rd = Wm.Rd.with_resp_headers
    (fun h -> Cohttp.Header.add h "content-type" mime_xhtml_charset)
      rd
  in
  rd

let request_uri_path_ends_with_slash rd =
  let req_path = Uri.path rd.Wm.Rd.uri in
  let len = String.length req_path in
  len > 0 && String.get req_path (len-1) = '/'

(* FIXME: check if meta of resource R has an acl resource
   or shares the one of R *)
let rd_add_acl_meta path rd =
  match Server_fs.kind path with
  | Some (`Dir | `File | `Acl) ->
      let acl = Server_fs.acl_path path in
      let meta = Server_fs.meta_path path in
      let acl_iri = Iri.to_string (Server_fs.iri acl) in
      let meta_iri = Iri.to_string (Server_fs.iri meta) in
      Wm.Rd.with_resp_headers (fun h ->
         let h = Cohttp.Header.add_multi h
           "link" [
             Printf.sprintf "%s; rel=\"acl\"" acl_iri ;
             Printf.sprintf "%s; rel=\"meta\"" meta_iri ;
           ]
         in
         h)
        rd
  | Some `Meta | None -> rd

class r (user:Iri.t option) path =
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    method resource_exists rd =
      match Server_fs.kind path with
      | None ->
          let rd = error_rd rd "Not found" "Not found" in
          Wm.continue false rd
      | Some `Dir ->
          (* if request uri does not end with / for a directory (container),
             then send a moved_temporarily:
             - declare that no such resource exists
             - previously_existed returns true for a `Dir
             - moved_temporarily returns new uri
          *)
          Wm.continue (request_uri_path_ends_with_slash rd) rd
      | Some _ -> Wm.continue true rd

    method previously_existed rd =
      match Server_fs.kind path with
        Some `Dir -> Wm.continue true rd
      | _ -> Wm.continue false rd

    method allowed_methods rd =
      Wm.continue [
        `GET ; `OPTIONS ; `HEAD ;
        `POST ;`PUT ; `DELETE ; `PATCH
      ] rd

    (*method is_authorized rd = Wm.continue `Authorized rd*)

    method forbidden rd =
      let%lwt rights = Server_perm.rights_for_path user path in
      let%lwt () = log
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
      let%lwt () = log (fun m -> m "moved temporarily ?") in
      match Server_fs.kind path with
      | Some `Dir ->
          begin
            if request_uri_path_ends_with_slash rd then
              Wm.continue None rd
            else
              let uri = Iri.to_uri (Server_fs.iri path) in
              Wm.continue (Some (Uri.of_string uri)) rd
          end
      | _ -> Wm.continue None rd

    method content_types_provided rd =
      let%lwt () = log (fun f -> f "content_types_provided") in
      match Server_fs.kind path with
        Some `Dir ->
          Wm.continue [
            Ldp_http.mime_turtle, self#to_container_ttl ;
            Ldp_http.mime_xmlrdf, self#to_container_xmlrdf ;
          ] rd
          (* FIXME: provide also simple HTML page *)
      | Some `Acl ->
          Wm.continue [
            Ldp_http.mime_turtle, self#to_acl_ttl ;
            Ldp_http.mime_xmlrdf, self#to_acl_xmlrdf ;
          ] rd
      | Some `Meta ->
          Wm.continue [
            Ldp_http.mime_turtle, self#to_meta_ttl ;
            Ldp_http.mime_xmlrdf, self#to_meta_xmlrdf ;
          ] rd
      | Some `File ->
          begin
            match%lwt Server_fs.path_mime path with
              None -> Wm.continue ["*/*", self#to_raw] rd
            | Some mime ->
                (* FIXME: offer some more formats depending on mime,
                   like rdf conversions *)
                Wm.continue [mime, self#to_raw] rd
          end
      | None ->
          Wm.continue ["*/*", self#to_raw] rd

    method content_types_accepted rd =
      Wm.continue [] rd

    method private to_container_ttl rd =
      let%lwt g = Server_fs.create_container_graph path in
      let body = `String (Rdf_ttl.to_string g) in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta path rd in
      Wm.continue body rd

    method private to_container_xmlrdf rd =
      let%lwt g = Server_fs.create_container_graph path in
      let body = `String (Rdf_xml.to_string g) in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta path rd in
      Wm.continue body rd

    method private to_meta_ttl rd =
      let file = Server_fs.path_to_filename path in
      let%lwt str =
        match%lwt Server_fs.string_of_file file with
        | None -> Lwt.return ""
        | Some str -> Lwt.return str
      in
      let body = `String str in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta path rd in
      Wm.continue body rd

    method private to_meta_xmlrdf rd =
      let%lwt body =
        match%lwt Server_fs.read_path_graph path with
        | None -> Lwt.return ""
        | Some g ->Lwt.return (Rdf_xml.to_string g)
      in
      let body = `String body in
      let rd =  { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta path rd in
      Wm.continue body rd

    method private to_acl_ttl rd =
      let file = Server_fs.path_to_filename path in
      let%lwt str =
        match%lwt Server_fs.string_of_file file with
        | None -> Lwt.return ""
        | Some str -> Lwt.return str
      in
      let body = `String str in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta path rd in
      Wm.continue body rd

    method private to_acl_xmlrdf rd =
      let%lwt body =
        match%lwt Server_fs.read_path_graph path with
        | None -> Lwt.return ""
        | Some g ->Lwt.return (Rdf_xml.to_string g)
      in
      let body = `String body in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta path rd in
      Wm.continue body rd

    method private to_raw rd =
      (* FIXME: improve this with a stream body *)
      let file = Server_fs.path_to_filename path in
      let%lwt str =
        match%lwt Server_fs.string_of_file file with
        | None -> Lwt.return ""
        | Some str -> Lwt.return str
      in
      let body = `String str in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta path rd in
      Wm.continue body rd
      
    method finish_request rd =
      let rd = Wm.Rd.with_resp_headers (fun h ->
         Cohttp.Header.add h "Access-Control-Allow-Origin" "*")
        rd
      in
      Wm.continue () rd
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
