type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]
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
