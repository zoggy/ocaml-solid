
module C = Solid_conf

let base_hostname = "localhost"
let base_port = 8080
let base_login = None
let base_files = ["tutu"; "toto"]
let base_boolean = false

let hostname = C.string base_hostname
let port = C.int base_port
let login = C.(option_ Wrapper.string base_login)
let files = C.(list Wrapper.string base_files)
let boolean = C.bool base_boolean

let check_val name to_s vref v =
  let v = C.get v in
  if v <> vref then
    failwith (Printf.sprintf
     "Incorrect field: %s = %S, should be %S"
       name (to_s v) (to_s vref))

let check () =
  check_val "hostname" (fun s -> s) base_hostname hostname ;
  check_val "port" string_of_int base_port port ;
  check_val "login" (function None -> "NONE" | Some s -> s) base_login login ;
  check_val "files" (String.concat ", ") base_files files ;
  check_val "boolean" string_of_bool base_boolean boolean

let group =
  let g = C.add C.group [`S "server" ; `S "hostname"] hostname in
  let g = C.add g [`S "server"; `S "port"] port in
  let guser = C.add C.group [`I Rdf_foaf.nick] login in
  let guser = C.add guser [`S "files"] files in
  let g = C.add_group g [`I Rdf_foaf.account] guser in
  let g = C.add g  [`I (Rdf_rdf.rdf_"hello")] boolean in
  g

let main =
  let%lwt h = Ldp_tls.make Lwt_io.(write_line stderr) in
  let module H = (val h : Ldp_http.Http) in

  let iri = Iri.of_string Sys.argv.(1) in
  let iri = Iri.normalize (Iri.append_path iri ["options.ttl"]) in
  let g = C.to_graph group iri in

  let%lwt _meta = H.put_rdf ~data:g iri in

  let dot = Rdf_dot.dot_of_graph g in
  let%lwt () = Lwt_io.(with_file ~mode:Output "/tmp/g.dot"
    (fun oc -> write oc dot))
  in
  C.set hostname "blabla" ;
  C.set port 0 ;
  C.set login (Some "blabla");
  C.set files [] ;
  C.set boolean true ;
  let%lwt g = H.get_rdf_graph iri in
  C.from_graph group g ;
  let g = C.to_graph group iri in
  let dot = Rdf_dot.dot_of_graph g in
  let%lwt () = Lwt_io.(with_file ~mode:Output "/tmp/g2.dot"
    (fun oc -> write oc dot))
  in
  let%lwt () = try check(); Lwt.return_unit with e -> Lwt.fail e in
  Lwt.return_unit

let () = Lwt_main.run main