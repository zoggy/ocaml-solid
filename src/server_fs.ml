(** *)

let homes () =
  Filename.concat (Ocf.get Server_conf.storage_root) "home"

let documents () =
  Filename.concat (Ocf.get Server_conf.storage_root) "documents"

let filepath_of_uri uri =
  let path = Uri.path uri in
  let len = String.length path in
  if len > 0 && String.get path 0 = '~' then
    (homes(), String.sub path 1 (len - 1))
  else
    (documents(), path)