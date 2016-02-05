module Ldp = Rdf_ldp
module Xhr = XmlHttpRequest


type error =
  | Post_error of int * Iri.t
  | Get_error of int * Iri.t
  | Put_error of int * Iri.t

exception Error of error

val string_of_error : error -> string

val map_opt : ('a -> 'b) -> 'a option -> 'b option
val do_opt : ('a -> unit) -> 'a option -> unit

val get_link : ('a * 'b) list -> 'a -> 'b option

type meta = {
  url : Iri.t;
  acl : Iri.t option;
  meta : Iri.t option;
  user : string option;
  websocket : string option;
  editable : Ldp_types.meth list;
  exists : bool;
  xhr : string Xhr.generic_http_frame;
}

val dbg : string -> unit
val dbg_js : 'jsoo_ad7fbbdd -> unit
val dbg_ : ('a, unit, string, unit) format4 -> 'a
val opt_s : string option -> string
val dbg_meta : meta -> unit

val response_metadata : string Xhr.generic_http_frame -> meta

val head : string -> meta Lwt.t
val get : ?accept:string -> Iri.t -> (string * string) Lwt.t
val get_graph : ?g:Rdf_graph.graph -> Iri.t -> Rdf_graph.graph Lwt.t

val post_container : ?slug:string -> Iri.t -> meta Lwt.t
val post_resource : ?data:Rdf_graph.graph -> ?slug:string -> Iri.t -> meta Lwt.t
val post_non_rdf : ?data:string -> ?mime:string -> Iri.t -> meta Lwt.t

val put : ?data:string -> ?mime:string -> Iri.t -> meta Lwt.t

val patch : ?del: Rdf_graph.graph -> ?ins: Rdf_graph.graph -> Iri.t -> unit Lwt.t
val delete : Iri.t -> unit Lwt.t

val login : ?url: Iri.t -> unit -> string option Lwt.t
