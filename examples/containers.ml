(** *)
open Ldp_types
open Ldp_http
open Lwt.Infix

module H = Ldp_http.Http(Ldp_js.Dbg)

module C = Ldp_containers.Make(H)
open Ldp_containers

let iri = Iri.of_string "https://zoggy.databox.me"

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
    let%lwt containers = C.containers iri in
    let node = Dom_html.getElementById "containers" in
    insert node containers;
    Lwt.return_unit
  with
    e -> H.dbg (Printexc.to_string e)

let _ = run ()