(** *)
open Ldp_types
open Ldp_http
open Lwt.Infix

module H = Ldp_http.Http(Ldp_js.Dbg)

let iri = Iri.of_string "https://zoggy.databox.me"

type tree =
  | NRDF of Iri.t * string
  | RDF of Iri.t
  | CONT of Iri.t * tree list

let containers () =
  let rec iter iri =
    match%lwt H.get iri with
      Container r ->
        let children = Ldp_types.container_children r.graph in
        let%lwt children = Lwt_list.map_p iter children in
        Lwt.return (CONT (iri, children))
    | Rdf r -> Lwt.return (RDF iri)
    | Non_rdf (ct, _) -> Lwt.return (NRDF (iri, ct))
  in
  iter iri

let node_text t =
  match t with
    CONT (iri, _) -> Iri.to_string iri
  | RDF iri -> Iri.to_string iri
  | NRDF (iri, ct) -> Printf.sprintf "%s [%s]" (Iri.to_string iri) ct

let node_children t =
  match t with
    CONT (iri, l) -> l
  | RDF iri -> []
  | NRDF (iri, ct) -> []

let insert =
  let doc = Dom_html.document in
  let rec iter ?(first=false) node t =
    let li =
      if first then
        node
      else
        (
         let li = doc##createElement (Js.string "li") in
         Dom.appendChild node li;
         li
        )
    in
    Dom.appendChild li (doc##createTextNode (Js.string (node_text t)));
    match node_children t with
      [] -> ()
    | l ->
        let ul = doc##createElement (Js.string "ul") in
        Dom.appendChild li ul ;
        List.iter (iter ul) l
  in
  iter ~first:true

let run () =
  try%lwt
    let%lwt containers = containers () in
    let node = Dom_html.getElementById "containers" in
    insert node containers;
    Lwt.return_unit
  with
    e -> H.dbg (Printexc.to_string e)

let _ = run ()