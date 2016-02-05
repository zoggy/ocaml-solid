
module H = Ldp_http
module Ldp = Rdf_ldp
module Foaf = Rdf_foaf
module G = Rdf_graph
open Lwt.Infix

type profile = G.graph

type workspace = {
  ws_title : string ;
  ws_iri : Iri.t ;
  ws_triples : Rdf_term.triple list ;
}

let get_profile iri =
  H.get_graph iri >>= fun g ->
(*  let iri = Iri.with_fragment iri None in*)
  match g.G.objects_of
    ~sub: (Rdf_term.Iri iri) ~pred: Foaf.primaryTopic
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

let get_workspaces ?profile webid =
  let%lwt profile = match profile with
      None -> get_profile webid
    | Some p -> Lwt.return p
  in
  let ws = G.iri_objects_of profile
    ~sub:(Rdf_term.Iri webid) ~pred: Rdf_pim.workspace
  in
  let f acc ws =
    match profile.G.objects_of
      ~sub: (Rdf_term.Iri ws) ~pred: Rdf_dc.title
    with
      (Rdf_term.Literal lit) :: _ ->
        { ws_title = lit.Rdf_term.lit_value ;
          ws_iri = ws ;
          ws_triples = profile.G.find ~sub: (Rdf_term.Iri ws) () ;
        } :: acc
    | _ ->
       acc
  in
  Lwt.return (List.fold_left f [] ws)
