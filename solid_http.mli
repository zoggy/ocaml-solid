module Ldp = Rdf_ldp
module Xhr = XmlHttpRequest
val map_opt : ('a -> 'b) -> 'a option -> 'b option
val do_opt : ('a -> unit) -> 'a option -> unit
val split_string : ?keep_empty:bool -> string -> char list -> string list
val get_link : ('a * 'b) list -> 'a -> 'b option
type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]
type meta = {
  url : string;
  acl : Iri.t option;
  meta : Iri.t option;
  user : string option;
  websocket : string option;
  editable : meth list;
  exists : bool;
  xhr : string Xhr.generic_http_frame;
}
val meth_of_string :
  ([> `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ] as 'a) list ->
  string -> 'a list
val string_of_meth :
  [< `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ] -> string
val dbg : string -> unit
val dbg_js : 'jsoo_ad7fbbdd -> unit
val dbg_ : ('a, unit, string, unit) format4 -> 'a
val opt_s : string option -> string
val dbg_meta : meta -> unit
val methods_of_string :
  string ->
  [> `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ] list
val response_metadata : string Xhr.generic_http_frame -> meta

val head : string -> meta Lwt.t
val get : ?accept:string -> string -> (string * string) Lwt.t
val get_graph : ?g:Rdf_graph.graph -> string -> Rdf_graph.graph Lwt.t
val post : ?data:'a -> ?slug:'b -> ?container:bool -> parent:'c -> 'd
val login : ?url: string -> unit -> string option Lwt.t
