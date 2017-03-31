
let main =
  let%lwt h = Ldp_tls.make Lwt_io.(write_line stderr) in
  let module H = (val h : Ldp_http.Http) in

  let iri = Iri.of_string Sys.argv.(1) in
  let%lwt () = H.patch_with_query iri
    ("PREFIX dc: <http://purl.org/dc/elements/1.1/>\n
   PREFIX cont: "^(Iri.to_string iri)^"\n
   DELETE { cont: dc:title ?title }
   INSERT { cont: dc:title \"New title\" }
   WHERE { cont: dc:title ?title }")
  in
  Lwt.return_unit

let () = Lwt_main.run main