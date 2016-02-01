

let id_iri = "https://zoggy.databox.me/profile/card#me"
(*let id_iri = "https://databox.me/"*)

open Solid_http

let t =
  let%lwt user = login ~url: id_iri () in
  dbg (Printf.sprintf "User=%s" (match user with None -> "" | Some u -> u));
  let%lwt g = get_graph "https://zoggy.databox.me/Preferences/prefs.ttl" in
  dbg (Rdf_ttl.to_string g) ;
  Lwt.return_unit
