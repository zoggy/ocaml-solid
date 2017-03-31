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

module Ldp = Rdf_ldp
open Ldp_types

open Cohttp
open Lwt.Infix;;

type Ldp_types.error +=
  | Post_error of int * Iri.t
  | Get_error of int * Iri.t
  | Put_error of int * Iri.t
  | Patch_error of int * Iri.t
  | Delete_error of int * Iri.t

let error e = Ldp_types.fail e

let string_of_error fallback = function
| Post_error (code, iri) ->
    Printf.sprintf "POST error (%d, %s)"
      code (Iri.to_string iri)
| Get_error (-1, iri) ->
    Printf.sprintf "GET error (empty body and content-type, %s)"
      (Iri.to_string iri)
| Get_error (code, iri) ->
    Printf.sprintf "GET error (%d, %s)"
      code (Iri.to_string iri)
| Put_error (code, iri) ->
    Printf.sprintf "PUT error (%d, %s)"
      code (Iri.to_string iri)
| Patch_error (code, iri) ->
    Printf.sprintf "PATCH error (%d, %s)"
      code (Iri.to_string iri)
| Delete_error (code, iri) ->
    Printf.sprintf "DELETE error (%d, %s)"
      code (Iri.to_string iri)
| e -> fallback e

let () = Ldp_types.register_string_of_error string_of_error

let map_opt f = function None -> None | Some x -> Some (f x)
let do_opt f = function None -> () | Some x -> f x

let mime_turtle = "text/turtle"
let mime_xmlrdf = "text/xml+rdf"
let mime_sparql_update = "application/sparql-update"
let mime_text = "text/plain"

let get_link links rel =
  try Some (List.assoc rel links)
  with Not_found -> None

let container_types =
  Iri.Set.of_list
    [ Rdf_ldp.c_BasicContainer ;
      Rdf_ldp.c_Container ;
    ]

let type_is_container =
  fun iri -> Iri.Set.mem iri container_types

let is_container ?iri g =
  let iri =
    match iri with
      None -> g.Rdf_graph.name ()
    | Some iri -> iri
  in
  let sub = Rdf_term.Iri iri in
  let e i =
    g.Rdf_graph.exists ~sub ~pred: Rdf_rdf.type_ ~obj:(Rdf_term.Iri i) ()
  in
  Iri.Set.exists e container_types

let string_of_metadata m =
  let b = Buffer.create 256 in
  let p = Printf.bprintf b in
  p "meta.iri=%s\n" (Iri.to_string m.iri) ;
  do_opt (p "meta.acl=%s\n") (map_opt Iri.to_string m.acl);
  do_opt (p "meta.meta=%s\n") (map_opt Iri.to_string m.meta);
  do_opt (p "meta.user=%s\n") m.user;
  do_opt (p "meta.websocket=%s\n") (map_opt Iri.to_string m.websocket);
  Buffer.contents b

let response_metadata iri ((resp, body): Response.t * Cohttp_lwt_body.t) =
  let header = resp.Response.headers in
  let links =
    match Header.get header "Link" with
      None -> []
    | Some str -> Iri.parse_http_link str
  in
  let iri =
    match Header.get header "Location" with
      None -> iri
    | Some str -> Iri.of_string str
  in
  let acl = get_link links "acl" in
  let meta =
    match get_link links "describedby" with
      None -> get_link links "meta"
    | x -> x
  in
  let user = Header.get header "User" in
  let websocket =
    match Header.get header "Updates-via" with
    | None -> None
    | Some s ->
      try Some (Iri.of_string s)
      with _ -> None
  in
  let exists = Code.code_of_status resp.Response.status = 200 in
  let editable =
    match Header.get header "Allow" with
      None -> []
    | Some str -> Ldp_types.methods_of_string str
  in
  { iri ; acl ; meta ; user ;
    websocket ; editable ; exists ; info = (resp, body) ;
  }

module type Requests =
  sig
    val dbg : string -> unit Lwt.t
    val call : ?body: Cohttp_lwt_body.t -> ?chunked: bool ->
      ?headers: Header.t -> Code.meth -> Iri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t
  end

module type Http =
  sig
    val dbg : string -> unit Lwt.t
    val head : Iri.t -> Ldp_types.meta Lwt.t
    val get_non_rdf : ?accept:string -> Iri.t -> (string * string) Lwt.t
    val get_rdf : ?g:Rdf_graph.graph ->
      ?accept:string -> ?parse:bool -> Iri.t ->
        (string * (Rdf_graph.graph, Ldp_types.error) result option) Lwt.t
    val get_rdf_graph : ?g:Rdf_graph.graph ->
      ?accept:string -> Iri.t -> Rdf_graph.graph Lwt.t
    val get_container : ?g:Rdf_graph.graph ->
      ?accept:string -> Iri.t -> Rdf_graph.graph Lwt.t
    val get : ?accept:string -> ?parse:bool -> Iri.t -> Ldp_types.resource Lwt.t
    val post :
      ?data:string ->
      ?mime:string ->
      ?slug:string -> typ:Iri.t -> Iri.t -> Ldp_types.meta Lwt.t
    val post_container : ?data: Rdf_graph.graph ->
      ?slug:string -> Iri.t -> Ldp_types.meta Lwt.t
    val post_direct_container : ?data: Rdf_graph.graph ->
      ?slug:string -> ?membershipResource: Iri.t ->
        relation: [< `HasMember of Iri.t | `IsMemberOf of Iri.t ] ->
          Iri.t -> Ldp_types.meta Lwt.t
    val post_indirect_container : ?data: Rdf_graph.graph ->
      ?slug:string -> ?membershipResource: Iri.t ->
        relation: [< `HasMember of Iri.t | `IsMemberOf of Iri.t ] ->
          insertedContent: Iri.t -> Iri.t -> Ldp_types.meta Lwt.t
    val post_rdf :
      ?data:Rdf_graph.graph ->
      ?slug:string -> ?typ: Iri.t -> Iri.t -> Ldp_types.meta Lwt.t
    val put : ?data:string -> ?mime:string -> ?typ: Iri.t -> Iri.t -> Ldp_types.meta Lwt.t
    val post_non_rdf :
      ?data:string -> ?mime:string -> Iri.t -> Ldp_types.meta Lwt.t
    val patch_with_query : Iri.t -> string -> unit Lwt.t
    val patch :
      ?del:Rdf_graph.graph -> ?ins:Rdf_graph.graph -> Iri.t -> unit Lwt.t
    val delete : Iri.t -> unit Lwt.t
    val login : Iri.t -> string option Lwt.t
    val fold_get :
      ?onerror:[ `Fail | `Ignore | `Report of exn -> unit Lwt.t ] ->
        ?accept:string ->
        ?parse:bool ->
        ('a -> Ldp_types.resource -> 'a Lwt.t) ->
        'a -> Iri.t list -> 'a Lwt.t
    val fold_get_graph :
      ?onerror:[ `Fail | `Ignore | `Report of exn -> unit Lwt.t ] ->
      Rdf_graph.graph -> Iri.t list -> unit Lwt.t
  end

module type Cache =
  sig
    val clear : unit -> unit Lwt.t
    val get :
      (?headers: Header.t -> Iri.t ->
       (Response.t * Cohttp_lwt_body.t) Lwt.t) ->
        ?headers:Header.t -> Iri.t -> (Response.t * string) Lwt.t
  end

module type Cache_impl =
  sig
    type key
    val clear : unit -> unit Lwt.t
    val key : Header.t -> Iri.t -> key option
    val store : key -> string -> unit Lwt.t
    val find : key -> string option Lwt.t
  end

module Make_cache (I:Cache_impl) : Cache =
  struct
    let empty_headers = Header.init ()

    let clear = I.clear
    let get (call_get : ?headers: Header.t -> Iri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t)
      ?(headers=empty_headers) iri =
      let headers_nocookie = Header.remove headers "cookie" in
      match I.key headers_nocookie iri with
        None ->
          let%lwt (resp, body) = call_get ~headers iri in
          let%lwt body = Cohttp_lwt_body.to_string body in
          Lwt.return (resp, body)
      | Some key ->
          match%lwt I.find key with
            Some marshalled ->
              Lwt.return (Marshal.from_string marshalled 0)
          | None ->
              let%lwt (resp, body) = call_get ~headers iri in
              let%lwt body = Cohttp_lwt_body.to_string body in
              match (Code.code_of_status resp.Response.status) / 100 with
              | 2 ->
                  let marshalled = Marshal.to_string (resp, body) [] in
                  let%lwt () = I.store key marshalled in
                  Lwt.return (resp, body)
              | _ ->
                  Lwt.return (resp, body)
  end

module No_cache = Make_cache
  (struct
    type key = unit
    let clear () = Lwt.return_unit
    let key _ _ = None
    let store _ _ = assert false
    let find _ = assert false
   end)

module Cached_http (C:Cache) (P:Requests) =
  struct
    let dbg = P.dbg

    let head iri = P.call `HEAD iri >|= response_metadata iri

    let cached_get =
      let call_get ?headers iri = P.call ?headers `GET iri in
      C.get call_get

    let get_non_rdf ?accept iri =
      let headers =
        match accept with
          None -> None
        | Some str -> Some (Header.init_with "Accept" str)
      in
      cached_get ?headers iri >>= fun (resp, body) ->
      let content_type =
        match Header.get resp.Response.headers "Content-type" with
          None -> ""
        | Some str -> str
      in
      match body, content_type with
        "", "" -> error (Get_error (-1, iri))
      | _ -> Lwt.return (content_type, body)

    let parse_graph ?g iri mime_type str =
      let g =
        match g with
          None -> Rdf_graph.open_graph (Iri.with_fragment iri None)
        | Some g -> g
      in
      match mime_type with
      | s when s = mime_turtle ->
          begin
            try Rdf_ttl.from_string g str; Ok g
            with e ->
                Pervasives.Error (Ldp_types.Parse_error (iri, e))
          end
      | s when s = mime_xmlrdf ->
          begin
            try Rdf_xml.from_string g str; Ok g
            with e ->
                Pervasives.Error (Ldp_types.Parse_error (iri, e))
          end
      | _ -> Pervasives.Error (Ldp_types.Unsupported_format (iri, mime_type))

    let get_rdf ?g ?(accept=mime_turtle) ?(parse=(g<>None)) iri =
      get_non_rdf ~accept iri >>=
      fun (content_type, str) ->
          let mime_type = Ldp_types.mime_of_content_type content_type in
          P.dbg str >>=
            fun () ->
              if parse then
                let res = parse_graph ?g iri mime_type str in
                Lwt.return (str, Some res)
              else
                Lwt.return (str, None)

    let get_rdf_graph ?g ?accept iri =
      match%lwt get_rdf ?g ?accept ~parse: true iri with
        | _, Some (Error e) -> Ldp_types.fail e
        | _, Some (Ok g) -> Lwt.return g
        | _ -> assert false

    let get_container = get_rdf_graph

    let get ?(accept=Printf.sprintf "%s, */*" mime_turtle) ?(parse=true) iri =
      let headers = Header.init_with "Accept" accept in
      cached_get ~headers iri >>= fun (resp, body) ->
      let header = resp.Response.headers in
      let ct =
        match Header.get header "Content-type" with
          None -> ""
        | Some s -> s
      in
      match Ldp_types.mime_of_content_type ct with
      | mime when parse &&
          (mime = mime_turtle || mime = mime_xmlrdf) ->
          let meta = response_metadata iri
            (resp, Cohttp_lwt_body.of_string body)
          in
          let src = (ct, body) in
          let g = Rdf_graph.open_graph iri in
          begin
            match parse_graph ~g iri mime body with
              Pervasives.Error e -> Ldp_types.fail e
            | Ok g ->
                let resource = { meta ; graph = g ; src } in
                if is_container g then
                  Lwt.return (Container resource)
                else
                  Lwt.return (Rdf resource)
          end
      | _ ->
          Lwt.return (Non_rdf (ct, Some body))

    let post ?data ?(mime=mime_turtle) ?slug ~typ iri =
      let headers =
        let h = Header.init_with "Content-type" mime in
        let h = Header.add h
          "Link" (Printf.sprintf "<%s>; rel=\"type\"" (Iri.to_string typ))
        in
        let h =
          match slug with
            None | Some "" -> h
          | Some str -> Header.add h "Slug" str
        in
        h
      in
      (*prerr_endline
        (let s =
          Cohttp.Header.pp_hum Format.str_formatter headers ;
          Format.flush_str_formatter ()
         in
         Printf.sprintf "request header=%s" s);*)
      let body = map_opt (fun s -> `String s) data in
      P.call ~headers ?body `POST iri
        >>= fun (resp, body) ->
        match Code.code_of_status resp.Response.status with
        | 200 | 201 -> Lwt.return (response_metadata iri (resp, body))
        | n -> error (Post_error (n, iri))

    let post_rdf ?data ?slug ?(typ=Rdf_ldp.c_Resource) iri =
      let data = map_opt Rdf_ttl.to_string data in
      post ?data ?slug ~typ iri

    let empty_iri = Iri.of_string ""

    let mk_container ?(data=Rdf_graph.open_graph empty_iri)
     ?membershipResource ?relation ?insertedContent typ =
      let open Rdf_graph in
      let open Rdf_term in
      let add = data.add_triple ~sub: (Iri empty_iri) in
      add ~pred: Rdf_dc.type_ ~obj: (Iri Rdf_ldp.c_Container);
      add ~pred: Rdf_dc.type_ ~obj: (Iri typ);
      (
       match membershipResource with
         None -> ()
       | Some iri ->
           add ~pred: Rdf_ldp.membershipResource
             ~obj: (Iri iri)
      );
      (
       match relation with
       | None -> ()
       | Some (`HasMember iri) ->
           add ~pred: Rdf_ldp.hasMemberRelation
             ~obj: (Iri iri)
       | Some (`IsMemberOf iri) ->
           add ~pred: Rdf_ldp.isMemberOfRelation
             ~obj: (Iri iri)
      );
      (match insertedContent with
         None -> ()
       | Some iri -> add ~pred: Rdf_ldp.insertedContentRelation ~obj: (Iri iri)
      );
      data

    let post_container ?data ?slug iri =
      let data = mk_container ?data Rdf_ldp.c_BasicContainer in
      post_rdf ~data ?slug ~typ: Rdf_ldp.c_BasicContainer iri

    let post_direct_container ?data ?slug ?membershipResource ~relation iri =
      let membershipResource =
        match membershipResource with
          None -> empty_iri
        | Some iri -> iri
      in
      let data = mk_container ?data
        ~membershipResource ~relation
          Rdf_ldp.c_DirectContainer
      in
      post_rdf ~data ?slug ~typ: Rdf_ldp.c_DirectContainer iri

    let post_indirect_container ?data
      ?slug ?membershipResource ~relation ~insertedContent iri =
      let membershipResource =
        match membershipResource with
          None -> empty_iri
        | Some iri -> iri
      in
      let data = mk_container ?data
        ~membershipResource ~relation ~insertedContent
          Rdf_ldp.c_IndirectContainer
      in
      post_rdf ~data ?slug ~typ: Rdf_ldp.c_IndirectContainer iri

    let put ?data ?(mime=mime_turtle) ?(typ=Rdf_ldp.c_NonRDFSource) iri =
      let body = map_opt (fun s -> `String s) data in
      let headers = Header.init_with "Content-type" mime in
      let headers =
        Header.add headers
          "Link" (Printf.sprintf "<%s>; rel=\"type\"" (Iri.to_string typ))
      in
      P.call ~headers ?body `PUT iri
      >>= fun (resp, body) ->
        match Code.code_of_status resp.Response.status with
        | 200 | 201 -> Lwt.return (response_metadata iri (resp, body))
        | n -> error (Put_error (n, iri))

    let post_non_rdf ?data ?mime iri =
      put ?data ?mime ~typ: Rdf_ldp.c_NonRDFSource iri

    let patch_with_query iri query =
      let body = `String query in
      let headers = Header.init_with "Content-type" mime_sparql_update in
      P.call ~headers ~body `PATCH iri
        >>= fun (resp, body) ->
        match Code.code_of_status resp.Response.status with
        | 200 | 201 -> Lwt.return_unit
        | n -> error (Patch_error (n, iri))

    let patch ?del ?ins iri =
      let b = Buffer.create 256 in
      (match del with
         None -> ()
       | Some g ->
           Printf.bprintf b "DELETE DATA { %s }\n"
             (Rdf_ttl.to_string g)
      );
      (match ins with
         None -> ()
       | Some g ->
           Printf.bprintf b "%sINSERT DATA { %s }\n"
             (match del with None -> "" | Some _ -> ";\n")
             (Rdf_ttl.to_string g)
      );
      match Buffer.contents b with
        "" -> Lwt.return_unit
      | query -> patch_with_query iri query

    let delete iri =
      P.call `DELETE iri
      >>= fun (resp, _) ->
        match Code.code_of_status resp.Response.status with
        | 200 | 201 -> Lwt.return_unit
        | n -> error (Delete_error (n, iri))


    let login iri =
      P.dbg (Printf.sprintf "login, iri=%s" (Iri.to_string iri)) >>= fun () ->
        head iri >>= fun meta -> Lwt.return meta.user

    let fold_get ?(onerror=`Fail) ?accept ?parse f acc iris =
      let g acc iri =
        match%lwt get ?accept ?parse iri with
        | r -> f acc r
        | exception e ->
           begin
             match onerror with
               `Fail -> Lwt.fail e
             | `Ignore -> Lwt.return acc
             | `Report rep -> let%lwt () = rep e in Lwt.return acc
           end
      in
      Lwt_list.fold_left_s g acc iris

    let fold_get_graph ?onerror g iris =
      let f () = function
      | Container r | Rdf r -> Rdf_graph.merge g r.graph; Lwt.return_unit
      | Non_rdf _ ->  Lwt.return_unit
      in
      fold_get ?onerror ~parse: true f () iris
  end

module Http (P:Requests) = Cached_http (No_cache) (P)
