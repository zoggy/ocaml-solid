
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

let get_link links rel =
  try Some (List.assoc rel links)
  with Not_found -> None

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
    match get_link links "meta" with
      None -> get_link links "describedBy"
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
    val get_rdf : ?g:Rdf_graph.graph -> Iri.t -> Rdf_graph.graph Lwt.t
    val get_container :
     ?g:Rdf_graph.graph -> Iri.t -> Rdf_graph.graph Lwt.t
    val is_container : Rdf_graph.graph -> bool
    val get : Iri.t -> Ldp_types.resource Lwt.t
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
    val patch :
      ?del:Rdf_graph.graph -> ?ins:Rdf_graph.graph -> Iri.t -> unit Lwt.t
    val delete : Iri.t -> unit Lwt.t
    val login : Iri.t -> string option Lwt.t
  end

module Http (P:Requests) =
  struct
    let dbg = P.dbg

    let head iri = P.call `HEAD iri >|= response_metadata iri

    let get_non_rdf ?accept iri =
      let headers =
        match accept with
          None -> None
        | Some str -> Some (Header.init_with "Accept" str)
      in
      P.call ?headers `GET iri >>= fun (resp, body) ->
      let content_type =
        match Header.get resp.Response.headers "Content-type" with
          None -> ""
        | Some str -> str
      in
      let%lwt body = Cohttp_lwt_body.to_string body in
      match body, content_type with
        "", "" -> error (Get_error (-1, iri))
      | _ -> Lwt.return (content_type, body)

    let get_rdf ?g iri =
      let g =
        match g with
          None -> Rdf_graph.open_graph iri
        | Some g -> g
      in
      get_non_rdf ~accept: mime_turtle iri >>=
        fun (_, str) ->
          P.dbg str >>=
            fun () ->
              Rdf_ttl.from_string g str;
              Lwt.return g

    let get_container ?g iri = get_rdf ?g iri

    let is_container g =
      let sub = Rdf_term.Iri (g.Rdf_graph.name ()) in
      let e = g.Rdf_graph.exists ~sub ~pred: Rdf_rdf.type_ in
      e ~obj: (Rdf_term.Iri Ldp.c_BasicContainer) () ||
        e ~obj: (Rdf_term.Iri Ldp.c_Container) ()

    let get iri =
      let headers = Header.init_with
        "Accept" (Printf.sprintf "%s, *" mime_turtle)
      in
      P.call ~headers `GET iri
      >>= fun (resp, body) ->
      let header = resp.Response.headers in
      let ct =
        match Header.get header "Content-type" with
          None -> ""
        | Some s -> s
      in
      let%lwt body_s = Cohttp_lwt_body.to_string body in
      match ct with
      | str when str = mime_turtle ->
          begin
            try%lwt
              let g = Rdf_graph.open_graph iri in
              Rdf_ttl.from_string g body_s;
              let resource = { meta = response_metadata iri (resp, body) ; graph = g } in
              if is_container g then
                Lwt.return (Container resource)
              else
                Lwt.return (Rdf resource)
            with (Rdf_ttl.Error err) as e ->
                P.dbg (Rdf_ttl.string_of_error err) >>= fun () ->
                  Lwt.fail e
          end
      | _ ->
          Lwt.return (Non_rdf (ct, Some body_s))

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
           Printf.bprintf b "INSERT DATA { %s }\n"
             (Rdf_ttl.to_string g)
      );
      match Buffer.contents b with
        "" -> Lwt.return_unit
      | query ->
          let body = `String query in
          let headers = Header.init_with "Content-type" "application/sparql-update" in
          P.call ~headers ~body `PATCH iri
          >>= fun (resp, body) ->
              match Code.code_of_status resp.Response.status with
              | 200 | 201 -> Lwt.return_unit
              | n -> error (Patch_error (n, iri))

    let delete iri =
      P.call `DELETE iri
      >>= fun (resp, _) ->
        match Code.code_of_status resp.Response.status with
        | 200 | 201 -> Lwt.return_unit
        | n -> error (Delete_error (n, iri))


    let login iri =
      P.dbg (Printf.sprintf "login, iri=%s" (Iri.to_string iri)) >>= fun () ->
        head iri >>= fun meta -> Lwt.return meta.user

  end
