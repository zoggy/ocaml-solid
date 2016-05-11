open Lwt.Infix

module IO =
  struct
    type 'a t = 'a Lwt.t
    let (>>=) = Lwt.bind
    let return = Lwt.return

    type ic = Lwt_io.input_channel
    type oc = Lwt_io.output_channel
    type conn = ic * oc

    let read_line ic = Lwt_io.read_line_opt ic
    let read ic count = Lwt_io.read ~count ic
    let write oc buf = Lwt_io.write oc buf
    let flush oc = Lwt_io.flush oc
  end

module Tls_net =
  struct
    module IO = IO
    type ctx = Tls.Config.client [@@deriving sexp_of]
    let default_ctx =
      let authenticator = X509.Authenticator.chain_of_trust [] in
      Tls.Config.(client ~authenticator ())

    let connect_uri ~ctx uri =
      let host = match Uri.host uri with None -> "" | Some s -> s in
      let port = match Uri.port uri with None -> 443 | Some n -> n in
      Tls_lwt.connect_ext
        (*~trace:eprint_sexp*)
        ctx (host, port)
        >>= fun (ic, oc) -> Lwt.return ((ic, oc), ic, oc)

    let close c = Lwt.catch (fun () -> Lwt_io.close c) (fun _ -> Lwt.return_unit)
    let close_in ic = Lwt.ignore_result (close ic)
    let close_out oc = Lwt.ignore_result (close oc)
    let close ic oc = Lwt.ignore_result (close ic >>= fun () -> close oc)
end

module Client = Cohttp_lwt.Make_client (IO) (Tls_net)

module type P =
  sig
    val dbg : string -> unit Lwt.t
    val authenticator : X509.Authenticator.a
    val certificates : Tls.Config.own_cert
  end

let print_cookies dbg (str, cookie) =
  let (a, b) = Cohttp.Cookie.Set_cookie_hdr.serialize cookie in
  let str = Printf.sprintf "cookie: %s => (%s, %s)"
      str a b
  in
  dbg str

type cookie_domain = string * bool (* host * with-subdomains *)
type cookie_path = string list

let is_suffix ~s ~suf =
  let len = String.length s in
  let len_suf = String.length suf in
  len >= len_suf &&
    String.sub s (len - len_suf) len_suf = suf

let is_prefix ~s ~pref =
  let len_s = String.length s in
  let len_pref = String.length pref in
  (len_pref <= len_s) &&
    (String.sub s 0 len_pref) = pref

module Make (P:P) : Ldp_http.Requests =
  struct
    let dbg = P.dbg

    let cookies = ref []

    let remove_cookies () =
      cookies := List.filter
        (fun (_,_,exp,_) ->
           match exp with
             None -> true
           | Some t -> t >= Unix.time()
        ) !cookies

    let add_cookie iri (_, c) =
      let open Cohttp.Cookie.Set_cookie_hdr in
      (*let (k, v) = c.cookie in
      prerr_endline (Printf.sprintf "Set-cookie: %s -> %s" k v);*)
      let domain =
        match c.domain with
        | None -> ((match Iri.host iri with None -> "" | Some s -> s), false)
        | Some s -> (s, true)
      in
      let path =
        match c.path with
        | None -> Iri.path_string iri
        | Some p -> p
      in
      let exp =
        match c.expiration with
          `Session -> None
        | `Max_age n -> Some (Unix.time () +. (Int64.to_float n))
      in
      let c = c.cookie in
      cookies := (domain, path, exp, c) :: !cookies

    let use_cookie host path (c_domain, sub) c_path exp =
      let b =
        (match exp with
           None -> true
         | Some t -> t >= Unix.time ()
        ) &&
          (sub && is_suffix ~s: host ~suf: c_domain) || c_domain = host
          && is_prefix ~s: path ~pref: c_path
      in
      (*prerr_endline (Printf.sprintf
        "use_cookie %s %s (%s, %b) %s _ = %b" host path c_domain sub c_path b
      *)
      b

    let cookies_by_iri iri =
      match Iri.host iri with
        None -> []
      | Some host ->
          let path = Iri.path_string iri in
          List.fold_left
            (fun acc (dom, p, exp, c) ->
               if use_cookie host path dom p exp then c :: acc else acc)
            []
            !cookies

    let call ?body ?chunked ?headers meth iri =
      let headers =
        match headers, cookies_by_iri iri with
            None, [] -> None
          | _, cookies ->
            let h =
              match headers with
                None -> Cohttp.Header.init ()
              | Some h -> h
            in
            (*List.iter
              (fun (k, v) -> prerr_endline (Printf.sprintf "setting cookie: %s => %s" k v))
              cookies;*)
            let (k, v) = Cohttp.Cookie.Cookie_hdr.serialize cookies in
            Some (Cohttp.Header.add h k v)
      in
      let ctx =
        Tls.Config.client
          ~authenticator: P.authenticator ~certificates: P.certificates
          ()
      in
      let%lwt (resp, body) = Client.call ~ctx ?body ?chunked ?headers meth
        (Uri.of_string (Iri.to_uri iri))
      in
      let () =
        let cookies = Cohttp.Cookie.Set_cookie_hdr.extract resp.Cohttp.Response.headers in
        match cookies with
        | [] -> ()
        | _ ->
            remove_cookies () ;
            List.iter (add_cookie iri) cookies ;
      in
      Lwt.return (resp, body)
  end