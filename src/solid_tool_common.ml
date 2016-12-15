open Lwt.Infix


type profile =
  { id: string [@ocf Ocf.Wrapper.string, "me"] ;
    privkey: string [@ocf Ocf.Wrapper.string, "client.key"] ;
    cert: string [@ocf Ocf.Wrapper.string, "client.pem"] ;
    certificates: string option [@ocf Ocf.Wrapper.(option string), None] ;
    cache: string option [@ocf Ocf.Wrapper.(option string), None] ;
  } [@@ocf];;

let usage = Printf.sprintf "Usage: %s [options] [args]\nwhere options are:" Sys.argv.(0)

let ldp_http_curl profile =
  let module P =
  struct
    let dbg = Lwt_io.write_line Lwt_io.stderr
    let cert = profile.cert
    let key = profile.privkey
  end
  in
  let%lwt cache =
    match profile.cache with
      None -> Lwt.return (module Ldp_http.No_cache : Ldp_http.Cache)
    | Some dir -> Ldp_cache.of_dir dir
  in
  let module C = (val cache: Ldp_http.Cache) in
  let module H = Ldp_http.Cached_http (C) (Ldp_curl.Make(P)) in
  Lwt.return (module H : Ldp_http.Http)

let ldp_http_tls profile =
  let%lwt authenticator = X509_lwt.authenticator `No_authentication_I'M_STUPID in
(*  let%lwt authenticator = X509_lwt.authenticator (`Ca_dir cert_dir) in*)
  let%lwt certificates = X509_lwt.private_of_pems
    ~cert:profile.cert ~priv_key: profile.privkey >>=
    fun c -> Lwt.return (`Single c)
  in
  let module P =
  struct
    let dbg = Lwt_io.write_line Lwt_io.stderr
    let authenticator = authenticator
    let certificates = certificates
  end
  in
  let%lwt cache =
    match profile.cache with
      None -> Lwt.return (module Ldp_http.No_cache : Ldp_http.Cache)
    | Some dir -> Ldp_cache.of_dir dir
  in
  let module C = (val cache: Ldp_http.Cache) in
  let module H = Ldp_http.Cached_http (C) (Ldp_tls.Make(P)) in
  Lwt.return (module H : Ldp_http.Http)

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
  let map = map_filename ~dir:rc_dir in
  let map_opt = function
  | None -> None
  | Some s -> Some (map_filename ~dir:rc_dir s)
  in
  try
    let p = List.find (fun p -> p.id = id) (Ocf.get profiles) in
    { p with
      privkey = map p.privkey ;
      cert = map p.cert ;
      certificates = map_opt p.certificates ;
      cache = map_opt p.cache ;
    }
  with Not_found -> failwith (Printf.sprintf "No profile %S" id)

let parse ?(options=[]) ?(usage=usage) () =
  let profile = ref default_profile in
  let curl = ref false in
  let identity id = profile := (find_profile id) in
  let privkey s = profile := { !profile with privkey = map_filename s } in
  let cert s = profile := { !profile with cert = map_filename s } in
  let certificates s =
    profile := { !profile with certificates = Some (map_filename s) }
  in
  let cache s = profile := { !profile with cache = Some (map_filename s) } in
  let nocache s = profile := { !profile with cache = None } in
  let base_options =
    [ "-p", Arg.String identity,
      "id use profile with corresponding id" ;

      "--privkey", Arg.String privkey,
      Printf.sprintf " <file> read private client key from <file> (default is %s)"
        !profile.privkey;

      "--cert", Arg.String cert,
      Printf.sprintf " <file> read client certificate from pem <file> (default is %s)"
        !profile.cert ;

      "--certificates", Arg.String certificates,
      " <dir> use certificates in <dir> to authenticate server";

      "--curl", Arg.Set curl,
      " use curl instead of cohttp+tls to connect" ;

      "--cache", Arg.String cache,
      " <dir> use <dir> as cache directory" ;

      "--nocache", Arg.Unit nocache,
      " <dir> do not use cache" ;
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

