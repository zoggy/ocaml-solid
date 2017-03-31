
let main =
  let%lwt h = Ldp_tls.make Lwt_io.(write_line stderr) in
  let module H = (val h : Ldp_http.Http) in

  let iri = Iri.of_string Sys.argv.(1) in
  let%lwt () = H.patch_with_query iri
    ("PREFIX dc: <http://purl.org/dc/elements/1.1/>\n
   PREFIX cont: <"^(Iri.to_string iri)^">\n
   DELETE WHERE { cont: dc:title ?title };\n
   INSERT DATA { cont: dc:title \"New title\" }")
  in

  let iri_image = Iri.append_path iri ["image.png"] in
  let%lwt () = H.patch_with_query iri_image
    ("PREFIX dc: <http://purl.org/dc/elements/1.1/>\n
   PREFIX image: <"^(Iri.to_string iri_image)^">\n
   INSERT DATA { image: dc:title \"Image title\" }")
  in

  let iri_graph = Iri.append_path iri ["graph.ttl"] in
  let%lwt () = H.patch_with_query iri_graph
    ("PREFIX dc: <http://purl.org/dc/elements/1.1/>\n
   PREFIX g: <"^(Iri.to_string iri_graph)^">\n
   DELETE WHERE { g: dc:title ?title };\n
   INSERT DATA {
     g: dc:title \"New Graph title\";
        dc:title \"Nouveau title du graphe\"@fr
   }")
  in
  Lwt.return_unit

let () = Lwt_main.run main