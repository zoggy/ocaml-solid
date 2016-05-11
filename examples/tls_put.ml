open Lwt.Infix

let string_of_file name =
  Lwt_io.with_file ~mode: Lwt_io.Input name Lwt_io.read

let usage = Printf.sprintf
  "%s [options] <uri> <file>\nwhere options are:"
  Sys.argv.(0)

let mime_type = ref Ldp_http.mime_turtle
let options = ["--mime", Arg.Set_string mime_type, " set mime-type"]

let f args http =
  match args with
    [] | [_] -> Lwt.fail_with usage
  | iri :: file :: _ ->
      let module H = (val http : Ldp_http.Http) in
      let iri = Iri.of_string iri in
      let%lwt data = string_of_file file in
      try%lwt
        let%lwt mt = H.put ~data ~mime: !mime_type iri in
        Lwt_io.write Lwt_io.stdout (Ldp_http.string_of_metadata mt)
      with
        Ldp_types.Error e ->
          let msg = Ldp_types.string_of_error e in
          Lwt_io.write Lwt_io.stderr msg

let () = Tls_common.main ~options f
