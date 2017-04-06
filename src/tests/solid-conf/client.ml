
module C = Solid_conf

let hostname = C.string "localhost"
let port = C.int 8080
let login = C.(option_ Wrapper.string None)
let files = C.(list Wrapper.string ["tutu" ; "toto"])

let group =
  let g = C.add C.group ["server" ; "hostname"] hostname in
  let g = C.add g ["server"; "port"] port in
  let guser = C.add C.group ["login"] login in
  let guser = C.add guser ["files"] files in
  let g = C.add_group g ["user"] guser in
  g

let main =
  let%lwt h = Ldp_tls.make Lwt_io.(write_line stderr) in
  let module H = (val h : Ldp_http.Http) in

  let iri = Iri.of_string Sys.argv.(1) in
  let iri = Iri.append_path iri ["options.ttl"] in
  let g = C.to_graph group iri in

  let%lwt _meta = H.put_rdf ~data:g iri in
  let dot = Rdf_dot.dot_of_graph g in
  let%lwt () = Lwt_io.(with_file ~mode:Output "/tmp/g.dot"
    (fun oc -> write oc dot))
  in
  Lwt.return_unit

let () = Lwt_main.run main