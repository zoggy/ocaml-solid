
module H = Ldp_http
module Ldp = Rdf_ldp
module Foaf = Rdf_foaf
module G = Rdf_graph
open Lwt.Infix

type profile = G.graph

type workspace = {
  ws_title : string ;
  ws_iri : Iri.t ;
  ws_graph : G.graph ;
}

let get_profile iri =
  H.get_graph iri >>= fun g ->
  let iri = Iri.with_fragment iri None in
  match g.G.objects_of
    ~sub: (Rdf_term.Iri iri)
      ~pred: Foaf.primaryTopic
  with
    [] ->
      Ldp_types.fail (Ldp_types.Missing_pred (iri, Foaf.primaryTopic))
  | webid :: _ ->
      let f acc pred =
        let objs = G.iri_objects_of g ~sub: (Rdf_term.Iri iri) ~pred in
        objs @ acc
      in
      let to_load = List.fold_left f []
        [ Rdf_owl.sameAs ; Rdf_rdfs.seeAlso ; Rdf_pim.preferencesFile ]
      in
      let load iri =
        let g = Rdf_graph.open_graph iri in
        try%lwt H.get_graph ~g iri
        with Ldp_types.Error e ->
          H.dbg (Ldp_types.string_of_error e);
          Lwt.return g
      in
      let%lwt graphs = Lwt_list.map_p load to_load in
      List.iter (Rdf_graph.merge g) graphs ;
      Lwt.return g

      