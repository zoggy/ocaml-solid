
let main =
  let%lwt h = Ldp_tls.make
    ?cache:None
    ~cert: ("tutu.pem","tutu.key")
    ~dbg:Lwt_io.(write_line stderr) in
  let module H = (val h : Ldp_http.Http) in

  let iri = Iri.of_string Sys.argv.(1) in
  let%lwt meta = H.post_container ~slug:"my-directory" iri in
  let%lwt () = Lwt_unix.sleep 1. in
  let%lwt _meta = H.post_container ~slug:"container" meta.Ldp_types.iri in
  Lwt.return_unit

let () = Lwt_main.run main