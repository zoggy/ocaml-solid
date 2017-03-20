open Lwt.Infix


type profile =
  { id: string [@ocf Ocf.Wrapper.string, "me"] ;
    cert: (string * string) option [@ocf Ocf.Wrapper.(option (pair string string)), None] ;
    certificates: string option [@ocf Ocf.Wrapper.(option string), None] ;
    cache: string option [@ocf Ocf.Wrapper.(option string), None] ;
    debug: bool [@ocf Ocf.Wrapper.bool, false] ;
  } [@@ocf];;

let usage = Printf.sprintf "Usage: %s [options] [args]\nwhere options are:" Sys.argv.(0)

let ldp_http_curl profile =
  let dbg =
    if profile.debug then
      Lwt_io.write_line Lwt_io.stderr
    else
      (fun _ -> Lwt.return_unit)
  in
  Ldp_curl.make ?cache:profile.cache ?cert:profile.cert ~dbg

let ldp_http_tls profile =
  let dbg =
    if profile.debug then
      Lwt_io.write_line Lwt_io.stderr
    else
      (fun _ -> Lwt.return_unit)
  in
  Ldp_tls.make ?cache:profile.cache ?cert:profile.cert ~dbg

let profiles = Ocf.list profile_wrapper []

let map_filename ?(dir=Sys.getcwd()) fn =
  if Filename.is_implicit fn then
    Filename.concat dir fn
  else
    if Filename.is_relative fn then
      Filename.concat (Sys.getcwd()) fn
    else
      fn

let find_profile id =
  let rc_dir =
    Filename.concat
      (try Sys.getenv "HOME"
       with _ -> "/")
       ".solid"
  in
  let rc_file = Filename.concat rc_dir "profiles.json" in
  Ocf.from_file (Ocf.as_group profiles) rc_file ;
  let map_opt f = function
  | None -> None
  | Some s -> Some (f s)
  in
  try
    let p = List.find (fun p -> p.id = id) (Ocf.get profiles) in
    { p with
      cert = map_opt (fun (s1, s2) ->
         map_filename ~dir:rc_dir s1, map_filename ~dir:rc_dir s2)
        p.cert ;
      certificates = map_opt (map_filename ~dir:rc_dir) p.certificates ;
      cache = map_opt (map_filename ~dir:rc_dir) p.cache ;
    }
  with Not_found -> failwith (Printf.sprintf "No profile %S" id)

let parse ?(options=[]) ?(usage=usage) () =
  let profile = ref default_profile in
  let curl = ref false in
  let identity id = profile := (find_profile id) in
  let privkey s =
    match !profile.cert with
      None -> profile := { !profile with cert = Some ("", map_filename s) }
    | Some (t,_) ->
        profile := { !profile with cert = Some (t, map_filename s) }
  in
  let cert s =
    match !profile.cert with
      None -> profile := { !profile with cert = Some (map_filename s, "") }
    | Some (_,t) ->
        profile := { !profile with cert = Some (map_filename s, t) }
  in
  let certificates s =
    profile := { !profile with certificates = Some (map_filename s) }
  in
  let cache s = profile := { !profile with cache = Some (map_filename s) } in
  let nocache s = profile := { !profile with cache = None } in
  let debug s = profile := { !profile with debug = true } in
  let nodebug s = profile := { !profile with debug = false } in
  let base_options =
    [ "-p", Arg.String identity,
      "id use profile with corresponding id" ;

      "--privkey", Arg.String privkey,
      Printf.sprintf " <file> read private client key from <file>" ;

      "--cert", Arg.String cert,
      Printf.sprintf " <file> read client certificate from pem <file>" ;

      "--certificates", Arg.String certificates,
      " <dir> use certificates in <dir> to authenticate server";

      "--curl", Arg.Set curl,
      " use curl instead of cohttp+tls to connect" ;

      "--cache", Arg.String cache,
      " <dir> use <dir> as cache directory" ;

      "--nocache", Arg.Unit nocache,
      " <dir> do not use cache" ;

      "--debug", Arg.Unit debug,
      " debug mode on" ;

      "--nodebug", Arg.Unit nodebug,
      " debug mode off" ;
    ]
  in
  let args = ref [] in
  Arg.parse (base_options @ options) (fun s -> args := s :: !args) usage ;
  let%lwt http =
    if !curl then
      ldp_http_curl !profile
    else
      ldp_http_tls !profile
  in
  Lwt.return (List.rev !args, http)

let print_alert where alert =
  let msg = Printf.sprintf "TLS ALERT (%s): %s"
    where (Tls.Packet.alert_type_to_string alert)
  in
  Lwt_io.(write_line stderr msg)

let print_fail where fail =
  let msg = Printf.sprintf "TLS FAIL (%s): %s"
    where (Tls.Engine.string_of_failure fail)
  in
  Lwt_io.(write_line stderr msg)

let main ?options ?usage f =
  try
    let main =
      try%lwt
        let%lwt (args, http) = parse ?options ?usage () in
        f args http
      with
      | Ldp_types.Error e ->
          Lwt_io.(write_line stderr (Ldp_types.string_of_error e))
      | Tls_lwt.Tls_alert alert ->
          print_alert "remote end" alert >>= fun () -> exit 1
            | Tls_lwt.Tls_failure alert ->
              print_fail "our end" alert >>= fun () -> exit 1
    in
    Lwt_main.run main
  with e ->
      let msg =
        match e with
        | Unix.Unix_error (e,s1,s2) ->
          Printf.sprintf "%s: %s %s" s1 (Unix.error_message e) s2
        | Failure s | Sys_error s -> s
        | _ -> Printexc.to_string e
      in
      prerr_endline msg;
      exit 1

