(*********************************************************************************)
(*                OCaml-Solid                                                    *)
(*                                                                               *)
(*    Copyright (C) 2016-2017 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

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

let mime_xhtml = Ldp_http.mime_xhtml
let mime_xhtml_charset = Server_page.mime_xhtml_charset
let mime_html = "text/html"

module SSet = Set.Make(String)

let handled_rdf_mimes = SSet.of_list
  [ Ldp_http.mime_xmlrdf ;
    Ldp_http.mime_turtle ;
  ]
let is_handled_rdf_mime str =
  let mime = Ldp_types.mime_of_content_type str in
  SSet.mem mime handled_rdf_mimes

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
  | `Meta _ | `Unknown | `UnknownDir -> rd

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


let rd_set_content_type rd mime =
  Wm.Rd.with_resp_headers
    (fun h -> Cohttp.Header.replace h "content_type" mime)
    rd

let graph_of_request rd path =
  match content_type_of_rd rd with
    str when str = Ldp_http.mime_turtle ->
      begin
        let%lwt str = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
        try
          let g = Rdf_graph.open_graph (Server_fs.iri path) in
          Rdf_ttl.from_string g str ;
          let%lwt () = Server_log._debug_lwt
            (fun f -> f "graph_of_request (%s): %s"
               (Iri.to_string (g.Rdf_graph.name ()))
                 (Rdf_ttl.to_string g)
            )
          in
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
        let%lwt str = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
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


module Make (Fs:Server_fs.Fs) (Acl:Server_acl.Acl) =
  struct
    let path_to_patch path =
      match Server_fs.kind path with
        `Acl _ | `Meta _ -> Lwt.return path
      | `Dir | `Unknown | `UnknownDir -> Lwt.return (Server_fs.meta_path path)
      | `File ->
          match%lwt Fs.path_mime path with
          | mime when is_handled_rdf_mime mime -> Lwt.return path
          | _ -> Lwt.return (Server_fs.meta_path path)

    let apply_patch path str =
      let query =  Rdf_sparql.query_from_string str in
      let%lwt graph_path =
        match Server_fs.kind path with
          `Dir | `Unknown | `UnknownDir -> Lwt.return (Server_fs.meta_path path)
        | `File ->
            begin
              match%lwt Fs.path_mime path with
              | mime when is_handled_rdf_mime mime -> Lwt.return path
              | _ -> Lwt.return (Server_fs.meta_path path)
            end
        | _ -> Lwt.return path
      in
      let%lwt graph =
        match%lwt Fs.read_path_graph graph_path with
          None -> Lwt.return (Rdf_graph.open_graph (Server_fs.iri graph_path))
        | Some g -> Lwt.return g
      in
      match Rdf_sparql.execute_update ~graph query with
        Rdf_sparql.Bool b ->
          let%lwt written = Fs.store_path_graph graph_path graph in
          Lwt.return (b && written)
      | _ -> assert false

    class r real_meth ?(read_only=false)
      ?(rights_for_path=Acl.rights_for_path)
        (user:Iri.t option) path =
      let write_body rd oc =
        Cohttp_lwt.Body.write_body
          (Lwt_io.write oc) rd.Wm.Rd.req_body
      in
      object(self)
        inherit [Cohttp_lwt.Body.t] Wm.resource

        method generate_etag rd =
        let accept = Cohttp.Header.get rd.Wm.Rd.req_headers "accept" in
        let%lwt opt = Fs.path_etag ?accept path in
        Wm.continue opt rd

      method last_modified rd =
        let%lwt opt = Fs.path_last_modified path in
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
          | `Acl `Unknown | `Meta `Unknown | `Unknown
          | `Acl `UnknownDir | `Meta `UnknownDir | `UnknownDir ->
              let rd =
                match real_meth with
                  `GET | `HEAD | `OPTIONS ->
                    error_rd rd "Not found" "Ressource does not exist"
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
        | `Dir when real_meth = `POST -> Wm.continue false rd
        | `Dir -> Wm.continue true rd
        | _ -> Wm.continue false rd

            (* method options rd : ((string * string) list, 'body) op
               see also https://github.com/solid/solid/issues/45 *)

      val mutable allowed_methods = []

      method known_methods rd =
        Wm.continue
          [ `GET ; `OPTIONS ; `HEAD; `POST ;`PUT ; `DELETE ; `PATCH ]
          rd

      method allowed_methods rd =
        let%lwt mets =
          match allowed_methods with
            [] ->
              let mets = [ `GET ; `OPTIONS ; `HEAD ] in
              let mets =
                if read_only
                then mets
                else mets @ [`POST ;`PUT ; `DELETE ; `PATCH ]
              in
              let%lwt () = Server_log._debug_lwt
                (fun f -> f "Allowed methods: %s"
                   (String.concat ", "
                    (List.map Cohttp.Code.string_of_method mets)))
              in
              allowed_methods <- mets ;
              Lwt.return mets
          | l -> Lwt.return l
        in
        Wm.continue mets rd

          (*method is_authorized rd = Wm.continue `Authorized rd*)

      method malformed_request rd =
        (* if request contains a slug field, make sure it contains
           only valid characters so wa can use it blindly later *)
        let rd = fix_slug_field rd in
        Wm.continue false rd

      method forbidden rd =
        let%lwt rights =
          let%lwt p =
            (* when patching containers or non (xml/rdf or turtle),
               compute the right on ,meta path instead of path *)
            match real_meth with
            | `PATCH -> path_to_patch path
            | _ -> Lwt.return path
          in
          rights_for_path user p
        in
        let%lwt () = log
          (fun m -> m "rights %s to user %s on %s"
             (Rdf_webacl.rights_to_string rights)
               (match user with None -> "NONE" | Some iri -> Iri.to_string iri)
               (Iri.to_string (Server_fs.iri path)))
        in
        let%lwt ok =
          match real_meth with
          | `OPTIONS ->
              (* browser's pre-flight requests do not send credentials,
                 so a 403 would make the browser block the request *)
              Lwt.return_true
          | `GET (* | `OPTIONS *) | `HEAD ->
              Lwt.return (Rdf_webacl.has_read rights)
          | `POST ->
              let%lwt is_container = Fs.path_is_container path in
              let%lwt () = log
                (fun m -> m "is_container %s: %b"
                   (Iri.to_string (Server_fs.iri path)) is_container)
              in
              Lwt.return
                (is_container &&
                 (Rdf_webacl.has_write rights || Rdf_webacl.has_append rights)
                )
          | `DELETE ->
              if Rdf_webacl.has_write rights then
                Fs.path_can_be_deleted path
              else
                Lwt.return_false
          | `PUT | `PATCH ->
              Lwt.return (Rdf_webacl.has_write rights)
          | _ -> Lwt.return_true
        in
        Wm.continue (not ok) rd

          (** [`POST] requests will call this method. Returning true indicates the
             POST succeeded. *)
      method process_post rd =
        (* we can POST only into containers *)
        match%lwt Fs.path_is_container path with
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
                Fs.available_dir
              else
                Fs.available_file
            in
            let%lwt() = Server_log._debug_lwt
              (fun f -> f "content-length: %d"
                 ((function None -> -1 | Some n -> int_of_string n)
                  (Cohttp.Header.get rd.Wm.Rd.req_headers "content-length")))
            in
            match%lwt avail ?slug path with
            | None -> Wm.continue false rd
            | Some newpath ->
                let%lwt created =
                  if create_container then
                    match%lwt graph_of_request rd newpath with
                      None -> Lwt.return (Error (`Bad_request, rd))
                    | Some g ->
                        let%lwt b = Fs.post_mkdir newpath g in
                        Lwt.return (Ok b)
                  else
                    let mime = content_type_of_rd rd in
                    let%lwt b =
                      Fs.post_file newpath ~mime (write_body rd)
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
              Acl.available_container_listings user path
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
              match%lwt Fs.path_mime path with
                "" -> Wm.continue ["*/*", self#to_raw] rd
              | mime ->
                  (* FIXME: offer some more formats depending on mime,
                     like rdf conversions *)
                  Wm.continue [mime, self#to_raw] rd
            end
        | `Unknown ->
            Wm.continue ["*/*", self#to_raw] rd
        | `UnknownDir ->
            Wm.continue [] rd

      method content_types_accepted rd =
        match real_meth, Server_fs.kind path with
        | `PATCH, _ ->
            Wm.continue
              [Ldp_http.mime_sparql_update, self#process_patch]
              rd
        | `PUT, `Dir -> Wm.continue [] rd
        | `PUT, _ ->
            if Server_fs.is_graph_path path then
              Wm.continue [
                Ldp_http.mime_turtle, self#put_graph ;
                Ldp_http.mime_xmlrdf, self#put_graph ;
              ] rd
            else
                (* accept the content type provided *)
                let ct = content_type_of_rd rd in
                Wm.continue [ct, self#accept_put] rd
        | `POST, `Dir ->
            begin
              match link_type_of_rd rd with
                Some iri when Ldp_http.type_is_container iri ->
                  Wm.continue [
                    Ldp_http.mime_turtle, self#dummy_accept ;
                    Ldp_http.mime_xmlrdf, self#dummy_accept ;
                  ] rd
              | _ ->
                    (* accept the content type provided *)
                    let ct = content_type_of_rd rd in
                    Wm.continue [ct, self#dummy_accept] rd
            end
        | _ -> Wm.continue [] rd

      method private process_patch rd =
        let%lwt () = Server_log._debug_lwt
          (fun f -> f "processing PATCH")
        in
        let%lwt str = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
        try
          let%lwt b = apply_patch path str in
          if not b then
            Wm.respond (Cohttp.Code.code_of_status `Bad_request) rd
          else
            Wm.continue true rd
        with
          e ->
            let (status, msg) =
              match e with
                Rdf_sparql.Error (Rdf_sparql.Not_implemented str) ->
                  (`Not_implemented, str)
              | Rdf_sparql.Error e ->
                  (`Bad_request, Rdf_sparql.string_of_error e)
              | e ->
                  (`Internal_server_error, Printexc.to_string e)
            in
            let%lwt() = Server_log._debug_lwt
              (fun f -> f "PATCH: %s" msg)
            in
            let body = `String msg in
            let rd = { rd with Wm.Rd.resp_body = body } in
            let rd = rd_set_content_type rd Ldp_http.mime_text in
            Wm.respond (Cohttp.Code.code_of_status status) rd

      method private dummy_accept rd = Wm.continue true rd
      method private accept_put rd =
        let%lwt existed = Fs.path_exists path in
        let mime = content_type_of_rd rd in
        let%lwt ok = Fs.put_file path ~mime (write_body rd) in
        let rd = rd_add_acl_meta rd path in
        let rd = if existed then rd else rd_set_location rd path in
        Wm.continue ok rd

      method private put_graph rd =
        let%lwt existed = Fs.path_exists path in
        match%lwt graph_of_request rd path with
        | None -> Wm.respond (Cohttp.Code.code_of_status `Bad_request) rd
        | Some g ->
            let%lwt ok = Fs.put_file path
              (fun oc -> Lwt_io.write oc (Rdf_ttl.to_string ~compact: true g))
            in
            let rd = if existed then rd else rd_set_location rd path in
            Wm.continue ok rd

      method delete_resource rd =
        let%lwt deleted = Fs.delete_path path in
        Wm.continue deleted rd

      method private to_container body rd =
        let rd = { rd with Wm.Rd.resp_body = body } in
        let rd = rd_add_acl_meta rd path in
        let rd = Wm.Rd.with_resp_headers (fun h ->
             Cohttp.Header.add h "link"
               (Printf.sprintf "<%s>; rel=\"type\""
                (Iri.to_string Rdf_ldp.c_Container))
          )
          rd
        in
        Wm.continue body rd

      method private to_container_ttl rd =
        let can_read p = rights_for_path user p >|= Rdf_webacl.has_read in
        let%lwt g = Fs.create_container_graph path can_read in
        let body = `String (Rdf_ttl.to_string ~compact: true g) in
        self#to_container body rd

      method private to_container_xmlrdf rd =
        let can_read p = rights_for_path user p >|= Rdf_webacl.has_read in
        let%lwt g = Fs.create_container_graph path can_read in
        let body = `String (Rdf_xml.to_string g) in
        self#to_container body rd

      method private to_meta body rd =
        let rd = rd_add_acl_meta rd path in
        let rd =  { rd with Wm.Rd.resp_body = body } in
        Wm.continue body rd

      method private to_meta_ttl rd =
        let%lwt str = Fs.string_of_path path in
        let body = `String str in
        self#to_meta body rd

      method private to_meta_xmlrdf rd =
        let%lwt body =
          match%lwt Fs.read_path_graph path with
          | None -> Lwt.return ""
          | Some g ->Lwt.return (Rdf_xml.to_string g)
        in
        let body = `String body in
        self#to_meta body rd

      method private to_acl body rd =
        let rd = { rd with Wm.Rd.resp_body = body } in
        let rd = rd_add_acl_meta rd path in
        Wm.continue body rd

      method private to_acl_ttl rd =
        let%lwt str = Fs.string_of_path path in
        let body = `String str in
        self#to_acl body rd

      method private to_acl_xmlrdf rd =
        let%lwt body =
          match%lwt Fs.read_path_graph path with
          | None -> Lwt.return ""
          | Some g ->Lwt.return (Rdf_xml.to_string g)
        in
        let body = `String body in
        self#to_acl body rd

      method private to_raw rd =
        (* FIXME: improve this with a stream body *)
        let%lwt str = Fs.string_of_path path in
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
        let module H = Cohttp.Header in
        let rd =
          Wm.Rd.with_resp_headers (fun h ->
             let origin =
               match Cohttp.Header.get rd.Wm.Rd.req_headers "origin" with
                 None -> "*"
               | Some str -> str
             in
             let allowed_mets =
               String.concat ","
                 (List.map Cohttp.Code.string_of_method allowed_methods)
             in
             let h = H.add h "Access-Control-Allow-Origin" origin in
             let h = H.add h "Access-Control-Allow-Methods" allowed_mets in
             let h =
               match H.get rd.Wm.Rd.req_headers "access-control-request-headers" with
                 None -> h
               | Some s -> H.add h "Access-Control-Allow-Headers" s
             in
             let h = H.add h "Access-Control-Allow-Credentials" "true" in
             let h = H.add h
               "Access-Control-Expose-Headers"
                 "User, Location, Link, Vary, Last-Modified, Content-Length, \
                 Accept-Patch, Accept-Post, Allow"
             in
             let h =
               match Server_fs.kind path with
                 `Dir -> H.add h "Accept-Post" "*/*"
               | _ -> h
             in
             let h = H.add h "Accept-Patch" Ldp_http.mime_sparql_update in
             let h = H.add_unless_exists h "Allow" allowed_mets in
             let h =
               match user with
                 None -> h
               | Some iri  -> H.add h "user" (Iri.to_string iri)
             in
             h
          )
            rd
        in
        Wm.continue () rd
    end
end
