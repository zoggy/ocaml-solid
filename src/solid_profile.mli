
type profile = Rdf_graph.graph
type workspace =
  { ws_title : string;
    ws_iri : Iri.t;
    ws_triples : Rdf_term.triple list;
  }
val get_profile : Iri.t -> profile Lwt.t
val get_workspaces : ?profile: profile -> Iri.t -> workspace list Lwt.t
