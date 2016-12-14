open Lwt.Infix

let options = []

let f args http =
  match args with
    [] -> Lwt.return_unit
  | iris ->
      let module H = (val http : Ldp_http.Http) in
      let f iri =
        try H.delete (Iri.of_string iri)
        with Ldp_types.Error e ->
            let msg = Printf.sprintf "%s: %s" iri
              (Ldp_types.string_of_error e)
            in
            Lwt_io.write_line Lwt_io.stdout msg
      in
      Lwt_list.iter_p f iris

let () = Solid_tool_common.main ~options f
