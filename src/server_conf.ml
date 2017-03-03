(** Server configuration options. *)

let server_cert = Ocf.string ~doc:".pem file of server certificate"
  "./server-certificates/server.pem"

let server_key = Ocf.string ~doc:".key file of server key"
  "./server-certificates/server.key"

let port = Ocf.int ~doc: "port number to listen to" 443

let storage_root = Ocf.string
  ~doc:"root directory to store served documents"
  "./documents"

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
  let g = Ocf.add_group g ["https"] https in
  let g = Ocf.add_group g ["storage"] storage in
  (*let g = Ocf.add_group g ["log"] Dmw_log.options in*)
  g
