

let id_iri = Iri.of_string "https://zoggy.databox.me/profile/card#me"
(*let id_iri = "https://databox.me/"*)

open Solid_http

let blabla = {|
<> a <http://rdfs.org/sioc/ns#Post> ;
    <http://purl.org/dc/terms/title> "First post" ;
    <http://rdfs.org/sioc/ns#content> "Hello world! This is my first post" .
    |}

let t =
  let%lwt user = login ~url: id_iri () in
  dbg (Printf.sprintf "User=%s" (match user with None -> "" | Some u -> u));
  let%lwt g = get_graph
    (Iri.of_string "https://zoggy.databox.me/Preferences/prefs.ttl")
  in
  dbg (Rdf_ttl.to_string g) ;
  let%lwt meta = post ~data: blabla
    (Iri.of_string "https://zoggy.databox.me/Public/")
  in
  dbg (Printf.sprintf "Post meta.url=%s" meta.url);

  Lwt.return_unit
