(** Server configuration options. *)

let server_cert = Ocf.string ~doc:".pem file of server certificate"
  "./server-certificates/server.pem"

let server_key = Ocf.string ~doc:".key file of server key"
  "./server-certificates/server.key"

let port = Ocf.int ~doc: "port number to listen to" 9999

let filename_wrapper =
  let to_json ?with_doc fn = `String fn in
  let from_json ?def = function
    `String str ->
      begin
        if Filename.is_relative str then
          Filename.concat (Sys.getcwd()) str
        else
          str
      end
  | json -> Ocf.invalid_value json
  in
  Ocf.Wrapper.make to_json from_json

let storage_root = Ocf.option filename_wrapper
  ~doc:"root directory to store served documents"
  "www"

let () = Logs.set_level ~all: true (Some Logs.Warning)
let global_log_level = Ocf.option
  ~cb: (fun l -> Logs.set_level ~all: true l;
     prerr_endline (Printf.sprintf "level set to %s" (Logs.level_to_string l)))
  Ldp_log.level_wrapper (Logs.level ())

let add_options g =
  let https =
    let g = Ocf.group in
    let g = Ocf.add g ["cert_file"] server_cert in
    let g = Ocf.add g ["key_file"] server_key in
    let g = Ocf.add g ["port"] port in
    g
  in
  let storage =
    let g = Ocf.group in
    let g = Ocf.add g ["root"] storage_root in
    g
  in
  let log =
    let g = Ocf.group in
    let g = Ocf.add g ["global"] global_log_level in
    let g = Ocf.add g ["library"] Ldp_log.log_level in
    let g = Ocf.add g ["server"] Server_log.log_level in
    g
  in
  let g = Ocf.add_group g ["https"] https in
  let g = Ocf.add_group g ["storage"] storage in
  let g = Ocf.add_group g ["log"] log in
  g
