open Lwt.Infix

let mkdir dir =
  let com = Printf.sprintf "mkdir -p %s" (Filename.quote dir) in
  match%lwt Lwt_process.(exec (shell com)) with
    Unix.WEXITED 0 -> Lwt.return_unit
  | _ -> Lwt.fail_with (Printf.sprintf "Command failed: %s" com)

let iri_base ?basename iri =
  let l =
    match Iri.path iri with
      Iri.Absolute l | Iri.Relative l -> l
  in
  match List.rev l with
    "" :: h :: _ -> h
  | h :: _ -> h
  | _ ->
      match basename with
      | None -> failwith
          (Printf.sprintf "No basename and empty path (%s)" (Iri.to_string iri))
      | Some b -> b

let is_prefix ~s ~pref =
  let len_s = String.length s in
  let len_pref = String.length pref in
  (len_pref <= len_s) &&
    (String.sub s 0 len_pref) = pref

let rec rec_get http ~dir ?basename iri =
  let module H = (val http : Ldp_http.Http) in
  let open Ldp_types in
  let rec iter dir ?(parse=true) iri =
    let%lwt () = Lwt_io.(write_line stderr (Printf.sprintf "get %s" (Iri.to_string iri))) in
    match%lwt H.get ~parse iri with
    | exception (Ldp_types.Error e) ->
        begin
          let msg = Ldp_types.string_of_error e in
          match e with
          | Ldp_types.Parse_error _
          | Ldp_types.Unsupported_format _ ->
              let%lwt () = Lwt_io.(write_line stderr msg) in
              if parse then iter dir ~parse: false iri else Lwt.return_unit
          | _ -> Lwt.return_unit
        end

    | Ldp_types.Container r ->
        let dir = Filename.concat dir (iri_base ?basename r.meta.iri) in
        let%lwt () = mkdir dir in
        let%lwt children =
          let l = Ldp_types.container_children r.graph in
          let s_iri = Iri.to_string r.meta.iri in
          let l = List.filter
            (fun iri -> is_prefix ~s:(Iri.to_string iri) ~pref:s_iri)
              l
          in
          Lwt.return l
        in
        Lwt.join
          [
            get_meta dir r ;
            Lwt_list.iter_p (iter dir) children ;
          ]

    | Ldp_types.Rdf r ->
        let filename = Filename.concat dir (iri_base ?basename r.meta.iri) in
        Lwt_io.(with_file ~mode: Output filename
         (fun oc -> write oc (snd r.src))
        )
    | Ldp_types.Non_rdf (_, c) ->
        let s = match c with None -> "" | Some s -> s in
        let filename = Filename.concat dir (iri_base ?basename iri) in
        Lwt_io.(with_file ~mode: Output filename
         (fun oc -> write oc s)
        )
  and iter_opt dir = function
    None -> Lwt.return_unit
  | Some iri -> iter dir iri
  and get_meta dir r =
    Lwt_list.iter_p (iter_opt dir) [ r.meta.acl ; r.meta.meta ]
  in
  iter dir iri

let get_and_print http ~print_type ~raw ?output iri =
  let module H = (val http : Ldp_http.Http) in
  let%lwt (ct,s) =
    match%lwt H.get ~parse: (not raw) iri with
    | Ldp_types.Container r
    | Ldp_types.Rdf r ->
        if raw then
          Lwt.return r.Ldp_types.src
        else
          Lwt.return (Ldp_http.mime_turtle, Rdf_ttl.to_string r.Ldp_types.graph)
    | Ldp_types.Non_rdf (ct, Some contents) -> Lwt.return (ct, contents)
    | Ldp_types.Non_rdf (ct, None) -> Lwt.return (ct, "")
  in
  match output with
    None ->
      Lwt_io.(
       let%lwt () =
         if print_type then write_line stdout ct else Lwt.return_unit
       in
       write stdout s)
  | Some fname ->
      Lwt_io.(
       with_file ~mode: Output fname (fun oc -> write oc s)
      )

let print_type = ref false
let raw = ref false
let recursive = ref false
let output = ref None
let basename = ref None
let options =
  [ "--print-type", Arg.Set print_type, " print content-type" ;
    "--raw", Arg.Set raw,
    " print graph source instead of printing from parsed graph" ;

    "-r", Arg.Set recursive,
    " recursively download containers and documents from the given iri";

    "-o", Arg.String (fun s -> output := Some s),
    "name output to given file or directory" ;

    "-b", Arg.String (fun s -> basename := Some s),
    "name basename to use when downloading an iri with an empty path" ;
  ]
let f args http =
  match args with
    [] -> Lwt.return_unit
  | iri :: _ ->
      let iri = Iri.of_string iri in
      if !recursive then
        let dir = match !output with
          | None -> Filename.current_dir_name
          | Some s -> s
        in
        rec_get http ~dir ?basename: !basename iri
      else
        get_and_print http
          ~print_type: !print_type ~raw: !raw ?output: !output iri

let () = Solid_tool_common.main ~options f
