(** *)

let exec com =
  match%lwt Lwt_process.(exec (shell com)) with
    Unix.WEXITED 0 -> Lwt.return_unit
  | _ -> Lwt.fail_with (Printf.sprintf "Command failed: %s" com)

let csr_template = {|
default_bits = 2048
prompt = no
default_md = sha256
#req_extensions = req_ext
x509_extensions = x509_ext
distinguished_name = dn

[ dn ]
CN = ${cn}
UID=${webid}

[ x509_ext ]

subjectKeyIdentifier    = hash
authorityKeyIdentifier  = keyid,issuer
basicConstraints  = CA:FALSE
keyUsage          = digitalSignature, keyEncipherment
subjectAltName    = @alternate_names
nsComment         = "OpenSSL Generated Certificate"

[ alternate_names ]
URI=critical,${webid}
|}

let make_var name content = (Re.(compile (str (Printf.sprintf "${%s}" name))), content)
let make_vars = List.map (fun (n,c) -> make_var n c)
let apply_vars vars template =
  List.fold_left
    (fun acc (re,by) -> Re.replace_string re ~all:true ~by acc)
    template vars

let make_csr_and_key ~cn ~webid ~csrfile ~keyfile =
  let vars = make_vars [ "cn", cn ; "webid", Iri.to_string webid ] in
  let temp = Filename.temp_file "oss" ".csr" in
  let%lwt () = Lwt_io.(with_file ~mode: Output temp
     (fun oc -> write oc (apply_vars vars csr_template)))
  in
  let com = Printf.sprintf
    "openssl req -new -sha256 -nodes -out %s -newkey rsa:2048 -keyout %s -config %s"
    (Filename.quote csrfile) (Filename.quote keyfile)
    (Filename.quote temp)
  in
  let%lwt () = exec com in
  Server_fs.safe_unlink temp

let make_pem ~webid ~csrfile ~keyfile ~pemfile =
  let temp = Filename.temp_file "oss" ".cnf" in
  let com = Printf.sprintf "cat /etc/ssl/openssl.cnf > %s" (Filename.quote temp) in
  let%lwt () = exec com in
  let%lwt () = Lwt_io.(with_file ~flags:[Unix.O_APPEND] ~mode:Output temp
     (fun oc -> write_line oc
        (Printf.sprintf "\n[SAN]\nsubjectAltName=critical,URI:%s" (Iri.to_string webid)))
    )
  in
  let com = Printf.sprintf
    "openssl x509 -req -days 365 -in %s  -signkey %s -out %s -extfile %s -extensions SAN"
    (Filename.quote csrfile) (Filename.quote keyfile)
    (Filename.quote pemfile) (Filename.quote temp)
  in
  let%lwt () = exec com in
  let%lwt () = Server_fs.safe_unlink temp in
  Server_log._debug_lwt
    (fun f ->
      let com = Printf.sprintf "openssl x509 -in %s -text"
        (Filename.quote pemfile)
       in
      let%lwt str = Lwt_process.(pread (shell com)) in
      f "PEM file: %s" str)

let make_pkcs12 ~keyfile ~pemfile ~outfile =
  let com = Printf.sprintf
    "openssl pkcs12 -export -out %s -inkey %s -in %s"
      (Filename.quote outfile) (Filename.quote keyfile) (Filename.quote pemfile)
  in
  exec com

type pem_info =
  { pem_webid : Iri.t option ;
    pem_exponent : string ;
    pem_modulus : string ;
  }
let info_from_pem pemfile =
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "Extracing pubkey from %s" pemfile)
  in
  let pubkey = Filename.temp_file "oss" "pubkey" in
  let command = Printf.sprintf
    "openssl x509 -pubkey -noout -in %s > %s"
    (Filename.quote pemfile) (Filename.quote pubkey)
  in
  let%lwt () = exec command in
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "Extracing modulus from %s" pubkey)
  in
  let command = ("",
     [| "openssl" ; "rsa" ; "-in" ; pubkey ; "-pubin" ;
      "-inform" ; "PEM" ; "-modulus" ; "-noout" |])
  in
  let%lwt str = Lwt_process.pread command in
  let re = Re.(compile (seq
      [
        str "Modulus=" ;
        group (rep1 (alt [alpha;digit])) ;
        eol ;
      ]))
  in
  let groups = Re.exec re str in
  let modulus = Re.Group.get groups 1 in

  let%lwt () = Server_log._debug_lwt
    (fun m -> m "Extracing exponent from %s" pubkey)
  in
  let command = ("",
     [| "openssl" ; "rsa" ; "-in" ; pubkey ; "-pubin" ;
      "-inform" ; "PEM" ; "-text" ; "-noout" |])
  in
  let%lwt str = Lwt_process.pread command in
  let re = Re.(compile (seq
      [
        str "Exponent: " ;
        group (rep1 digit) ;
        char ' ' ;
      ]))
  in
  let groups = Re.exec re str in
  let exponent = Re.Group.get groups 1 in
  let%lwt () = Server_fs.safe_unlink pubkey in

  let%lwt () = Server_log._debug_lwt
    (fun m -> m "Extracing Subject Alternative Name from %s" pemfile)
  in
  let command =
    ("", [| "openssl" ; "x509" ; "-noout" ; "-in" ; pemfile ; "-text" |])
  in
  let%lwt str = Lwt_process.pread command in
  let re = Re.(compile (seq
      [
        str "X509v3 Subject Alternative Name:";
        rep (diff any (set "\n\r")) ;
        rep (set " \t\n\r");
        str "URI:" ;
        group (rep1 notnl) ;
      ]))
  in
  let webid =
    try
      let groups = Re.exec re str in
      let s = Re.Group.get groups 1 in
      try Some (Iri.of_string s)
      with Iri.Error e ->
          Server_log._debug
            (fun f -> f "%s: %s" s (Iri.string_of_error e));
          None 
    with
      _ -> None
  in
  let%lwt () = Server_log._debug_lwt
    (fun m -> m "modulus=%s\nexponent=%s\nwebid=%s"
       modulus exponent
         (match webid with None -> "NONE" | Some i -> Iri.to_string i))
  in
  Lwt.return
    { pem_webid = webid ;
      pem_exponent = exponent ;
      pem_modulus = modulus ;
    }
