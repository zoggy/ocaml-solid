(** *)
open Ldp_types
open Ldp_http

let iri = Iri.of_string "https://zoggy.databox.me"

type tree =
  | NRDF of Iri.t * string
  | RDF of Iri.t
  | CONT of Iri.t * tree list

let containers () =
  let rec iter iri =
    match%lwt Ldp_http.get iri with
      Container r ->
        let children = Ldp_types.container_children r.graph in
        let%lwt children = Lwt_list.map_p iter children in
        Lwt.return (CONT (iri, children))
    | Rdf r -> Lwt.return (RDF iri)
    | Non_rdf (ct, _) -> Lwt.return (NRDF (iri, ct))
  in
  iter iri

open D3

let children t _ =
  match t with
    CONT (iri, children) -> children
  | RDF iri -> []
  | NRDF (iri, ct) -> []
let rec view () =
  nest (selectAll "div" <.> data children)
    [ enter |. append "div" |. data children ;
      update
      |. (text (fun _ t _ ->
      match t with
          CONT (iri, children) -> Iri.to_string iri
        | RDF iri -> Iri.to_string iri
        | NRDF (iri, ct) -> Printf.sprintf "%s [%s]" (Iri.to_string iri) ct
    ))
   ]

let run =
  let%lwt containers = containers () in
  let node = Dom_html.getElementById "containers" in
  D3.run ~node (view ()) containers ;
  Lwt.return_unit
