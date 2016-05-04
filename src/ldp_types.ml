
(** *)

module Xhr = XmlHttpRequest

type meth = [
  | `DELETE
  | `GET
  | `HEAD
  | `OPTIONS
  | `PATCH
  | `POST
  | `PUT ]

type meta = {
  iri : Iri.t;
  acl : Iri.t option;
  meta : Iri.t option;
  user : string option;
  websocket : Iri.t option;
  editable : meth list;
  exists : bool;
  xhr : string Xhr.generic_http_frame;
}

type rdf_resource =
  { meta : meta ;
    graph: Rdf_graph.graph ;
  }

type resource =
| Container of rdf_resource
| Rdf of rdf_resource
| Non_rdf of string * string option (* mime type * content *)

let container_children g =
  let sub = Rdf_term.Iri (g.Rdf_graph.name()) in
  let pred = Rdf_ldp.contains in
  Rdf_graph.iri_objects_of ~sub ~pred g

type error = ..
exception Error of error
let error e = raise (Error e)
let fail e = Lwt.fail (Error e)

let ref_string_of_error = ref (function _ -> "Unknown error")
let string_of_error e = !ref_string_of_error e
let register_string_of_error f =
  let g = !ref_string_of_error in
  ref_string_of_error := f g

type error +=
  | Invalid_method of string
  | Missing_pred of Iri.t * Iri.t

let () = register_string_of_error
  (fun fallback -> function
     | Invalid_method str -> Printf.sprintf "Invalid method %S" str
     | Missing_pred (sub, pred) ->
         Printf.sprintf "%s has no relation with predicate %s"
           (Iri.to_string sub) (Iri.to_string pred)
     | e -> fallback e
  )

let meth_of_string = function
  "DELETE" -> `DELETE
| "GET" -> `GET
| "HEAD" -> `HEAD
| "OPTIONS" -> `OPTIONS
| "PATCH" -> `PATCH
| "POST" -> `POST
| "PUT" -> `PUT
| str -> error (Invalid_method str)

let string_of_meth = function
  `DELETE -> "DELETE"
| `GET -> "GET"
| `HEAD -> "HEAD"
| `OPTIONS -> "OPTIONS"
| `PATCH -> "PATCH"
| `POST -> "POST"
| `PUT -> "PUT"

(*c==v=[String.split_string]=1.2====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> if keep_empty then [""] else []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.2====*)

let methods_of_string =
  let f acc m =
    try (meth_of_string m) :: acc
    with _ -> acc
  in
  fun str ->
    List.fold_left f [] (split_string str [',';' ';'\t'])
