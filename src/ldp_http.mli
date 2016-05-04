module Ldp = Rdf_ldp
module Xhr = XmlHttpRequest

type error =
  | Post_error of int * Iri.t
  | Get_error of int * Iri.t
  | Put_error of int * Iri.t
  | Patch_error of int * Iri.t

exception Error of error

val string_of_error : error -> string

val map_opt : ('a -> 'b) -> 'a option -> 'b option
val do_opt : ('a -> unit) -> 'a option -> unit

val get_link : ('a * 'b) list -> 'a -> 'b option

val dbg : string -> unit
val dbg_js : 'jsoo_ad7fbbdd -> unit
val dbg_ : ('a, unit, string, unit) format4 -> 'a
val opt_s : string option -> string
val dbg_meta : Ldp_types.meta -> unit

val response_metadata : string Xhr.generic_http_frame -> Ldp_types.meta

val head : string -> Ldp_types.meta Lwt.t

val get_container : ?g:Rdf_graph.graph -> Iri.t -> Rdf_graph.graph Lwt.t
val get_rdf : ?g:Rdf_graph.graph -> Iri.t -> Rdf_graph.graph Lwt.t
val get_non_rdf : ?accept:string -> Iri.t -> (string * string) Lwt.t
val get : Iri.t -> Ldp_types.resource Lwt.t

val post_container : ?slug:string -> Iri.t -> Ldp_types.meta Lwt.t
val post_rdf : ?data:Rdf_graph.graph -> ?slug:string -> Iri.t -> Ldp_types.meta Lwt.t
val post_non_rdf : ?data:string -> ?mime:string -> Iri.t -> Ldp_types.meta Lwt.t

val put : ?data:string -> ?mime:string -> Iri.t -> Ldp_types.meta Lwt.t

val patch : ?del: Rdf_graph.graph -> ?ins: Rdf_graph.graph -> Iri.t -> unit Lwt.t
val delete : Iri.t -> unit Lwt.t

val login : ?url: Iri.t -> unit -> string option Lwt.t
