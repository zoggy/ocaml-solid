
type profile = Rdf_graph.graph
type workspace =
  { ws_title : string;
    ws_iri : Iri.t;
    ws_triples : Rdf_term.triple list;
  }


module type S =
  sig
    val get_profile : Iri.t -> profile Lwt.t
    val inbox : profile -> Iri.t option
    val workspaces : profile -> workspace list
    val storages : profile -> Iri.t list
    val name : profile -> string
    val pim : profile -> Rdf_pim.from
  end

module Make : Ldp_http.Http -> S
