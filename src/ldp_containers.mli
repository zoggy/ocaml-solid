type tree = NRDF of Iri.t * string | RDF of Iri.t | CONT of Iri.t * tree list
val node_text : tree -> string
module type S = sig val containers : Iri.t -> tree end
module Make :
  functor (P : Ldp_http.Http) -> sig val containers : Iri.t -> tree Lwt.t end
