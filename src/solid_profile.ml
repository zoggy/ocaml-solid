
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

module type S =
  sig
    val get_profile : Iri.t -> profile Lwt.t
    val inbox : profile -> Iri.t option
    val workspaces : profile -> workspace list
    val storages : profile -> Iri.t list
    val name : profile -> string
    val pim : profile -> Rdf_pim.from
  end

module Make (H: Ldp_http.Http) =
  struct
    let primary_topic profile =
      let iri = profile.G.name() in
      match profile.G.objects_of
        ~sub: (Rdf_term.Iri iri)
        ~pred: Foaf.primaryTopic
      with
        [] ->
          Ldp_types.error
            (Ldp_types.Missing_pred (iri, Foaf.primaryTopic))
      | webid :: _ -> webid

    let get_profile iri =
      H.get_rdf iri >>= fun g ->
      let%lwt webid =
        try Lwt.return (primary_topic g)
        with e -> Lwt.fail e
      in
      let f sub acc pred =
        let objs = G.iri_objects_of g ~sub ~pred in
        objs @ acc
      in
      let to_load_preds =
        [ Rdf_owl.sameAs ; Rdf_rdfs.seeAlso ; Rdf_pim.preferencesFile ]      
      in
      let to_load = List.fold_left (f (Rdf_term.Iri iri)) [] to_load_preds in
      let to_load = List.fold_left (f webid) to_load to_load_preds in
      let load iri =
        let g = Rdf_graph.open_graph iri in
        try%lwt H.get_rdf ~g iri
        with Ldp_types.Error e ->
            H.dbg (Ldp_types.string_of_error e) >>= fun () ->
              Lwt.return g
      in
      let%lwt graphs = Lwt_list.map_p load to_load in
      List.iter (Rdf_graph.merge g) graphs ;
      Lwt.return g

    let workspaces profile =
      let webid = primary_topic profile in
      let ws = G.iri_objects_of profile
        ~sub:webid ~pred: Rdf_pim.workspace
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
      List.fold_left f [] ws

    let inbox profile =
      let sub = primary_topic profile in
      match Rdf_graph.iri_objects_of profile
        ~sub ~pred: Rdf_solid.inbox
      with
        [] -> None
      | iri :: _ -> Some iri

    let storages profile =
      let sub = primary_topic profile in
      Rdf_graph.iri_objects_of profile
        ~sub ~pred: Rdf_pim.storage

    let name profile =
      let sub = primary_topic profile in
      let l = profile.Rdf_graph.objects_of
        ~sub ~pred: Rdf_foaf.name
      in
      let rec iter = function
        [] -> Iri.to_string (profile.Rdf_graph.name())
      | (Rdf_term.Literal l) :: _ -> l.Rdf_term.lit_value
      | _ :: q -> iter q
      in
      iter l

    let pim profile =
      match primary_topic profile with
        Rdf_term.Iri sub -> new Rdf_pim.from ~sub profile
      | _ -> new Rdf_pim.from profile

  end
