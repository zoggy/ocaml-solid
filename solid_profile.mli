
type profile = Rdf_graph.graph
type workspace = { ws_title : string; ws_iri : Iri.t; ws_graph : Rdf_graph.graph; }
val get_profile : Iri.t -> profile Lwt.t
