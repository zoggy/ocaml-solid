module Ldp = Rdf_ldp
open Cohttp

type error =
  | Post_error of int * Iri.t
  | Get_error of int * Iri.t
  | Put_error of int * Iri.t
  | Patch_error of int * Iri.t
  | Delete_error of int * Iri.t

exception Error of error

val string_of_error : error -> string

val map_opt : ('a -> 'b) -> 'a option -> 'b option
val do_opt : ('a -> unit) -> 'a option -> unit

val get_link : ('a * 'b) list -> 'a -> 'b option

val string_of_metadata : Ldp_types.meta -> string

module type Requests =
  sig
    val dbg : string -> unit Lwt.t
    val call: ?body: Cohttp_lwt_body.t -> ?chunked: bool ->
      ?headers: Header.t -> Code.meth -> Iri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t
  end

val response_metadata : Iri.t -> (Response.t * Cohttp_lwt_body.t) -> Ldp_types.meta

module type Http =
  sig
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
    val post_container : ?slug:string -> Iri.t -> Ldp_types.meta Lwt.t
    val post_rdf :
      ?data:Rdf_graph.graph ->
      ?slug:string -> Iri.t -> Ldp_types.meta Lwt.t
    val put : ?data:string -> ?mime:string -> Iri.t -> Ldp_types.meta Lwt.t
    val post_non_rdf :
      ?data:string -> ?mime:string -> Iri.t -> Ldp_types.meta Lwt.t
    val patch :
      ?del:Rdf_graph.graph -> ?ins:Rdf_graph.graph -> Iri.t -> unit Lwt.t
    val delete : Iri.t -> unit Lwt.t
    val login : Iri.t -> string option Lwt.t
  end

module Http : Requests -> Http