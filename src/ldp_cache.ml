
exception Not_a_directory of string

let of_dir dirname =
  let dirname =
    if Filename.is_relative dirname then
      Filename.concat (Sys.getcwd()) dirname
    else
      dirname
  in
  let module I : Ldp_http.Cache_impl =
  struct
    type key = string

    let key headers iri =
      let str = Printf.sprintf "%s\n%s"
        (Iri.to_string iri)
          (Cohttp.Header.to_string headers)
      in
      Some (Digest.to_hex (Digest.string str))

    let store key str =
      let filename = Filename.concat dirname key in
      (*let%lwt () = Lwt_io.(write_line stdout (Printf.sprintf "output to %s" filename)) in*)
      Lwt_io.(with_file Output filename
       (fun oc -> write oc str))

    let find key =
      let filename = Filename.concat dirname key in
      try%lwt
        let%lwt str = Lwt_io.(with_file Input filename read) in
        Lwt.return_some str
      with _ -> Lwt.return_none

    let clear () =
      Lwt_stream.iter_p
        (fun f -> try%lwt Lwt_unix.unlink f with _ -> Lwt.return_unit)
        (Lwt_unix.files_of_directory dirname)
  end
  in
  let%lwt () =
    match Sys.is_directory dirname with
    | true -> Lwt.return_unit
    | false -> Lwt.fail (Not_a_directory dirname)
    | exception _ -> Lwt_unix.mkdir dirname 0o750
  in
  let module C = Ldp_http.Make_cache (I) in
  Lwt.return (module C : Ldp_http.Cache)

