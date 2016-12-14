
type tree =
  | NRDF of Iri.t * string
  | RDF of Iri.t
  | CONT of Iri.t * tree list

let node_text t =
  match t with
    CONT (iri, _) -> Iri.to_string iri
  | RDF iri -> Iri.to_string iri
  | NRDF (iri, ct) -> Printf.sprintf "%s [%s]" (Iri.to_string iri) ct

module type S =
  sig
    val containers : Iri.t -> tree
  end

module Make (P:Ldp_http.Http) =
  struct
    let containers iri =
      let rec iter iri =
        match%lwt P.get iri with
          Ldp_types.Container r ->
            let children = Ldp_types.container_children r.Ldp_types.graph in
            let%lwt children = Lwt_list.map_p iter children in
            Lwt.return (CONT (iri, children))
        | Ldp_types.Rdf r -> Lwt.return (RDF iri)
        | Ldp_types.Non_rdf (ct, _) -> Lwt.return (NRDF (iri, ct))
      in
      iter iri
  end
