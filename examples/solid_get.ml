open Lwt.Infix

let print_type = ref false
let raw = ref false
let options =
  [ "--print-type", Arg.Set print_type, " print content-type" ;
    "--raw", Arg.Set raw,
    " print graph source instead of printing from parsed graph" ;
  ]

let f args http =
  match args with
    [] -> Lwt.return_unit
  | iri :: _ ->
      let module H = (val http : Ldp_http.Http) in
      let iri = Iri.of_string iri in
      let%lwt (ct,s) =
        match%lwt H.get ~parse: (not !raw) iri with
        | Ldp_types.Container r
        | Ldp_types.Rdf r ->
            if !raw then
              Lwt.return r.Ldp_types.src
            else
              Lwt.return (Ldp_http.mime_turtle, Rdf_ttl.to_string r.Ldp_types.graph)
        | Ldp_types.Non_rdf (ct, Some contents) -> Lwt.return (ct, contents)
        | Ldp_types.Non_rdf (ct, None) -> Lwt.return (ct, "")
      in
      (if !print_type then Lwt_io.write_line Lwt_io.stdout ct else Lwt.return_unit)
      >>= fun () -> Lwt_io.write Lwt_io.stdout s

let () = Solid_common.main ~options f
