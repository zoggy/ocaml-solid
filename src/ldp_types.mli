module Xhr = XmlHttpRequest

type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

type meta =
  { iri : Iri.t ;
    acl : Iri.t option ;
    meta: Iri.t option ;
    user: string option ;
    websocket: Iri.t option ;
    editable : meth list ;
    exists: bool ;
    xhr: string Xhr.generic_http_frame ;
  }


type rdf_resource =
  { meta : meta ;
    graph: Rdf_graph.graph ;
  }

type resource =
| Container of rdf_resource
| Rdf of rdf_resource
| Non_rdf of string * string option (* mime type * content *)

val container_children : Rdf_graph.graph -> Iri.t list

type error = ..
exception Error of error
val error : error -> 'a
val fail : error -> 'a Lwt.t
val ref_string_of_error : (error -> string) ref
val string_of_error : error -> string
val register_string_of_error : ((error -> string) -> error -> string) -> unit
type error +=
| Invalid_method of string
| Missing_pred of Iri.t * Iri.t

val meth_of_string : string -> meth
val string_of_meth : meth -> string
val split_string : ?keep_empty:bool -> string -> char list -> string list
val methods_of_string : string -> meth list
