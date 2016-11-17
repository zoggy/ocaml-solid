open Lwt.Infix

let string_of_file name =
  Lwt_io.with_file ~mode: Lwt_io.Input name Lwt_io.read

let usage = Printf.sprintf
  "%s [options] <uri> [<file>]\nwhere options are:"
  Sys.argv.(0)

let mk_container = ref false
let mime_type = ref Ldp_http.mime_turtle
let options = [
    "--mime", Arg.Set_string mime_type, " set mime-type" ;
    "-c", Arg.Set mk_container, " create a container" ;
  ]

let f args http =
  let module H = (val http : Ldp_http.Http) in
  try%lwt
    match args with
    | [iri] when !mk_container ->
        let iri = Iri.of_string iri in
        let%lwt mt = H.post_container iri in
        Lwt_io.write Lwt_io.stdout (Ldp_http.string_of_metadata mt)

    | iri :: file :: _ ->
        let iri = Iri.of_string iri in
        let%lwt data = string_of_file file in
        let typ =
          if !mime_type = Ldp_http.mime_turtle then
            Rdf_ldp.c_RDFSource
          else
            Rdf_ldp.c_NonRDFSource
        in
        let%lwt mt = H.put ~data ~mime: !mime_type ~typ iri in
        Lwt_io.write Lwt_io.stdout (Ldp_http.string_of_metadata mt)

  | [] | [_] -> Lwt.fail_with usage
  with
  | Ldp_types.Error e ->
      let msg = Ldp_types.string_of_error e in
      Lwt_io.write Lwt_io.stderr msg

let () = Solid_common.main ~options f
