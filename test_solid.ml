

let id_iri = Iri.of_string "https://zoggy.databox.me/profile/card#me"
(*let id_iri = "https://databox.me/"*)

open Solid_http

let doc_meta url =
  let g = Rdf_graph.open_graph url in
  Rdf_ttl.from_string g {|
<> a <http://rdfs.org/sioc/ns#Post> ;
    <http://purl.org/dc/terms/title> "Hello HTML doc!" .
    |};
  Rdf_ttl.to_string g

let doc_css = {|
  h1 { background-color: black;
       color: green;
     } |}

let doc_html = {|
  <html>
    <head>
      <title>Hello HTML document !</title>
    </head>
    <body>
    <h1>Hello HTML document!</h1>
    <p>bla bla bla</p>
    <p>bla bla bla</p>
    </body>
  </html>
|}

let t =
  try%lwt
    let%lwt user = login ~url: id_iri () in
    dbg (Printf.sprintf "User=%s" (match user with None -> "" | Some u -> u));
    let%lwt g = get_graph
      (Iri.of_string "https://zoggy.databox.me/Preferences/prefs.ttl")
    in
    dbg (Rdf_ttl.to_string g) ;
    let%lwt meta = post_non_rdf ~data: doc_html ~mime: "text/html"
      (Iri.of_string "https://zoggy.databox.me/Public/doc.html")
    in
    dbg (Printf.sprintf "Post meta.url=%s" meta.url);
    match meta.meta with
      None -> dbg "No meta URL"; Lwt.return_unit
    | Some iri_meta ->
        dbg (Printf.sprintf "Meta IRI=%s" (Iri.to_string iri_meta));
        let iri = Iri.of_string meta.url in
        let%lwt meta = put ~data: (doc_meta iri) iri_meta in
        dbg (Printf.sprintf "Put ok");
        let%lwt meta = head (Iri.to_uri iri) in
        dbg_meta meta ;
        let ins = Rdf_graph.open_graph iri in
        ins.Rdf_graph.add_triple
          ~sub: (Rdf_term.Iri iri) ~pred: Rdf_dc.dc_title
          ~obj: (Rdf_term.term_of_literal_string "Hello title 2") ;
        let%lwt () = patch ~ins iri_meta in
        Lwt.return_unit
  with
    Solid_http.Error e ->
      dbg (Printf.sprintf "Error: %s" (string_of_error e));
      Lwt.return_unit
  | Iri.Error e ->
      dbg (Printf.sprintf "Error: %s" (Iri.string_of_error e));
      Lwt.return_unit
  | Rdf_ttl.Error e ->
      dbg (Printf.sprintf "Error: %s" (Rdf_ttl.string_of_error e));
      Lwt.return_unit
  | e ->
      dbg (Printexc.to_string e);
      Lwt.return_unit



