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

let mime_xhtml = Server_page.mime_xhtml
let mime_xhtml_charset = Server_page.mime_xhtml_charset
let mime_html = "text/html"

let error_rd rd title message =
  let body = Server_page.page title [Xtmpl_rewrite.cdata message] in
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
let rd_add_acl_meta rd path =
  match Server_fs.kind path with
  | `Dir | `File | `Acl _ ->
      let acl = Server_fs.acl_path path in
      let meta = Server_fs.meta_path path in
      let acl_iri = Iri.to_string (Server_fs.iri acl) in
      let meta_iri = Iri.to_string (Server_fs.iri meta) in
      Wm.Rd.with_resp_headers (fun h ->
         let h = Cohttp.Header.add_multi h
           "link" [
             Printf.sprintf "<%s>; rel=\"acl\"" acl_iri ;
             Printf.sprintf "<%s>; rel=\"describedby\"" meta_iri ;
           ]
         in
         h)
        rd
  | `Meta _ | `Unknown -> rd

let content_type_of_rd rd =
  let h = rd.Wm.Rd.req_headers in
  match Cohttp.Header.get h "content-type" with
  | None -> Ldp_http.mime_turtle
  | Some str -> str

let link_type_of_rd rd =
  let h = rd.Wm.Rd.req_headers in
  match Cohttp.Header.get h "link" with
    None -> None
  | Some str ->
      let links = Iri.parse_http_link str in
      try Some (List.assoc "type" links)
      with Not_found -> None

let fix_slug_field =
  let safe_slug b () _i = function
    `Malformed str -> ()
  | `Uchar codepoint ->
      if Iri_types.host_safe_char codepoint then
        Uutf.Buffer.add_utf_8 b codepoint
      else
        ()
  in
  fun rd ->
    let h = rd.Wm.Rd.req_headers in
    match Cohttp.Header.get h "slug" with
    | None -> rd
    | Some str ->
        let str = Iri_types.pct_decode str in
        let str = Uunf_string.normalize_utf_8 `NFKC str in
        let b = Buffer.create (String.length str) in
        Uutf.String.fold_utf_8 (safe_slug b) () str;
        let field = Buffer.contents b in
        Wm.Rd.with_req_headers
          (fun h -> Cohttp.Header.replace h "slug" field)
          rd

let rd_set_location rd path =
  Wm.Rd.with_resp_headers
    (fun h -> Cohttp.Header.replace h "location"
       (Iri.to_uri (Server_fs.iri path))
    )
    rd

let graph_of_request rd path =
  match content_type_of_rd rd with
    str when str = Ldp_http.mime_turtle ->
      begin
        let%lwt str = Cohttp_lwt_body.to_string rd.Wm.Rd.req_body in
        try
          let g = Rdf_graph.open_graph (Server_fs.iri path) in
          Rdf_ttl.from_string g str ;
          Lwt.return_some g
        with
          e ->
            let%lwt () = Server_log._err_lwt
              (fun f -> f "Invalid turtle: %s" (Printexc.to_string e))
            in
            Lwt.return_none
      end
  | str when str = Ldp_http.mime_xmlrdf ->
      begin
        let%lwt str = Cohttp_lwt_body.to_string rd.Wm.Rd.req_body in
        try
          let g = Rdf_graph.open_graph (Server_fs.iri path) in
          Rdf_xml.from_string g str ;
          Lwt.return_some g
        with
          e ->
            let%lwt () = Server_log._err_lwt
              (fun f -> f "Invalid xml/rdf: %s" (Printexc.to_string e))
            in
            Lwt.return_none
      end
  | _ -> Lwt.return_none

class r (user:Iri.t option) path =
  let write_body rd oc =
    Cohttp_lwt_body.write_body
      (Lwt_io.write oc) rd.Wm.Rd.req_body
  in
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    method generate_etag rd =
      let%lwt opt = Server_fs.path_etag path in
      Wm.continue opt rd

    method last_modified rd =
      let%lwt opt = Server_fs.path_last_modified path in
      let%lwt () = Server_log._debug_lwt (fun m ->
           m "Last modified for %s: %s"
             (Iri.to_string (Server_fs.iri path))
             (match opt with None -> "" | Some s -> s)
        )
      in
      Wm.continue opt rd

    method resource_exists rd =
      let (exists, rd) =
        match Server_fs.kind path with
        | `Acl `Unknown | `Meta `Unknown | `Unknown ->
            let rd =
              match rd.Wm.Rd.meth with
                `GET | `HEAD | `OPTIONS ->
                  error_rd rd "Not found" "Ressources does not exist"
              | _ -> rd
            in
            (false, rd)
        | `Dir ->
            (* if request uri does not end with / for a directory (container),
               then send a moved_temporarily:
               - declare that no such resource exists
               - previously_existed returns true for a `Dir
               - moved_temporarily returns new uri
               *)
            (request_uri_path_ends_with_slash rd, rd)
        | _ -> (true, rd)
      in
      Wm.continue exists rd

    method previously_existed rd =
      match Server_fs.kind path with
      | `Dir when rd.Wm.Rd.meth = `POST -> Wm.continue false rd
      | `Dir -> Wm.continue true rd
      | _ -> Wm.continue false rd

    (* method options rd : ((string * string) list, 'body) op
        see also https://github.com/solid/solid/issues/45 *)

    method allowed_methods rd =
      Wm.continue [
        `GET ; `OPTIONS ; `HEAD ;
        `POST ;`PUT ; `DELETE ; `PATCH
      ] rd

    (*method is_authorized rd = Wm.continue `Authorized rd*)

    method malformed_request rd =
      (* if request contains a slug field, make sure it contains
         only valid characters so wa can use it blindly later *)
      let rd = fix_slug_field rd in
      Wm.continue false rd

    method forbidden rd =
      let%lwt rights = Server_acl.rights_for_path user path in
      let%lwt () = log
        (fun m -> m "rights %d to user %s on %s"
          rights
          (match user with None -> "NONE" | Some iri -> Iri.to_string iri)
          (Iri.to_string (Server_fs.iri path)))
      in
      let%lwt ok =
        match rd.Wm.Rd.meth with
          `GET | `OPTIONS | `HEAD ->
            Lwt.return (Server_acl.has_read rights)
        | `POST ->
            Server_fs.path_is_container path >|=
              (&&) (Server_acl.has_write rights || Server_acl.has_append rights)
        | `DELETE ->
            if Server_acl.has_write rights then
             Server_fs.path_can_be_deleted path
            else
              Lwt.return_false
        | `PUT | `PATCH ->
            Lwt.return (Server_acl.has_write rights)
        | _ -> Lwt.return_true
      in
      Wm.continue (not ok) rd

    (** [`POST] requests will call this method. Returning true indicates the
         POST succeeded. *)
    method process_post rd =
      (* we can POST only into containers *)
      match%lwt Server_fs.path_is_container path with
      | false -> Wm.continue false rd
      | true ->
          let slug = Cohttp.Header.get rd.Wm.Rd.req_headers "slug" in
          let create_container =
            match link_type_of_rd rd with
              Some iri -> Ldp_http.type_is_container iri
            | _ -> false
          in
          let avail =
            if create_container then
              Server_fs.available_dir
            else
              Server_fs.available_file
          in
          match%lwt avail ?slug path with
          | None -> Wm.continue false rd
          | Some newpath ->
              let%lwt created =
                if create_container then
                  match%lwt graph_of_request rd newpath with
                    None -> Lwt.return (Error (`Bad_request, rd))
                  | Some g ->
                      let%lwt b = Server_fs.post_mkdir newpath g in
                      Lwt.return (Ok b)
                else
                  let mime = content_type_of_rd rd in
                  let%lwt b =
                    Server_fs.post_file newpath ~mime (write_body rd)
                  in
                  Lwt.return (Ok b)
              in
              match created with
              | Error (status, rd) ->
                  Wm.respond (Cohttp.Code.code_of_status status) rd
              | Ok false -> Wm.continue false rd
              | Ok true ->
                  let rd = rd_set_location rd newpath in
                  let rd = rd_add_acl_meta rd newpath in
                  Wm.continue true rd

    method moved_temporarily rd =
      let%lwt () = log (fun m -> m "moved temporarily ?") in
      match Server_fs.kind path with
      | `Dir ->
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
        `Dir ->
          let as_graph = [
              Ldp_http.mime_turtle, self#to_container_ttl ;
              Ldp_http.mime_xmlrdf, self#to_container_xmlrdf ;
            ]
          in
          let%lwt as_other =
            Server_acl.available_container_listings user path
            >|= List.map (fun (mime, f) -> (mime, self#to_mime f))
          in
          Wm.continue (as_graph @ as_other) rd

      | `Acl _ ->
          Wm.continue [
            Ldp_http.mime_turtle, self#to_acl_ttl ;
            Ldp_http.mime_xmlrdf, self#to_acl_xmlrdf ;
          ] rd
      | `Meta _ ->
          Wm.continue [
            Ldp_http.mime_turtle, self#to_meta_ttl ;
            Ldp_http.mime_xmlrdf, self#to_meta_xmlrdf ;
          ] rd
      | `File ->
          begin
            match%lwt Server_fs.path_mime path with
              None -> Wm.continue ["*/*", self#to_raw] rd
            | Some mime ->
                (* FIXME: offer some more formats depending on mime,
                   like rdf conversions *)
                Wm.continue [mime, self#to_raw] rd
          end
      | `Unknown ->
          Wm.continue ["*/*", self#to_raw] rd

    method content_types_accepted rd =
      match rd.Wm.Rd.meth, Server_fs.kind path with
      | `PUT, `Dir -> Wm.continue [] rd
      | `PUT, _ ->
          if Server_fs.is_graph_path path then
            Wm.continue [
              Ldp_http.mime_turtle, self#put_graph ;
              Ldp_http.mime_xmlrdf, self#put_graph ;
            ] rd
          else
            Wm.continue ["*/*", self#accept_put] rd
      | `POST, `Dir ->
          begin
            match link_type_of_rd rd with
              Some iri when Ldp_http.type_is_container iri ->
                Wm.continue [
                  Ldp_http.mime_turtle, self#dummy_accept ;
                  Ldp_http.mime_xmlrdf, self#dummy_accept ;
                ] rd
            | _ -> Wm.continue ["*/*", self#dummy_accept] rd
          end
      | _ -> Wm.continue [] rd

    method private dummy_accept rd = Wm.continue true rd
    method private accept_put rd =
      let%lwt existed =
        Lwt_unix.file_exists (Server_fs.path_to_filename path)
      in
      let mime = content_type_of_rd rd in
      let%lwt ok = Server_fs.put_file path ~mime (write_body rd) in
      let rd = rd_add_acl_meta rd path in
      let rd = if existed then rd else rd_set_location rd path in
      Wm.continue ok rd

    method private put_graph rd =
      let%lwt existed =
        Lwt_unix.file_exists (Server_fs.path_to_filename path)
      in
      match%lwt graph_of_request rd path with
      | None -> Wm.respond (Cohttp.Code.code_of_status `Bad_request) rd
      | Some g ->
          let%lwt ok = Server_fs.put_file path
            (fun oc -> Lwt_io.write oc (Rdf_ttl.to_string ~compact: true g))
          in
          let rd = if existed then rd else rd_set_location rd path in
          Wm.continue ok rd

    method delete_resource rd =
      let%lwt deleted = Server_fs.delete_path path in
      Wm.continue deleted rd

    method private to_container_ttl rd =
      let%lwt g = Server_fs.create_container_graph path in
      let body = `String (Rdf_ttl.to_string ~compact: true g) in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta rd path in
      Wm.continue body rd

    method private to_container_xmlrdf rd =
      let%lwt g = Server_fs.create_container_graph path in
      let body = `String (Rdf_xml.to_string g) in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta rd path in
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
      let rd = rd_add_acl_meta rd path in
      Wm.continue body rd

    method private to_meta_xmlrdf rd =
      let%lwt body =
        match%lwt Server_fs.read_path_graph path with
        | None -> Lwt.return ""
        | Some g ->Lwt.return (Rdf_xml.to_string g)
      in
      let body = `String body in
      let rd =  { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta rd path in
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
      let rd = rd_add_acl_meta rd path in
      Wm.continue body rd

    method private to_acl_xmlrdf rd =
      let%lwt body =
        match%lwt Server_fs.read_path_graph path with
        | None -> Lwt.return ""
        | Some g ->Lwt.return (Rdf_xml.to_string g)
      in
      let body = `String body in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta rd path in
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
      let rd = rd_add_acl_meta rd path in
      Wm.continue body rd

    method private to_mime f rd =
      let%lwt body = f () in
      let body =  `String body in
      let rd = { rd with Wm.Rd.resp_body = body } in
      let rd = rd_add_acl_meta rd path in
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
