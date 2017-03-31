(*********************************************************************************)
(*                OCaml-Solid                                                    *)
(*                                                                               *)
(*    Copyright (C) 2016-2017 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

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

module Make (P:P) : Ldp_http.Requests =
  struct
    let dbg = P.dbg

    include Ldp_cookies.Make ()

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

let make ?cache ?cert ~dbg =
  let%lwt authenticator = X509_lwt.authenticator `No_authentication_I'M_STUPID in
(*  let%lwt authenticator = X509_lwt.authenticator (`Ca_dir cert_dir) in*)
  let%lwt certificates = 
    match cert with
    | None -> Lwt.return `None
    | Some (cert, priv_key) ->
        X509_lwt.private_of_pems ~cert ~priv_key >>=
          fun c -> Lwt.return (`Single c)
  in
  let module P =
  struct
    let dbg = dbg
    let authenticator = authenticator
    let certificates = certificates
  end
  in
  let%lwt cache =
    match cache with
      None -> Lwt.return (module Ldp_http.No_cache : Ldp_http.Cache)
    | Some dir -> Ldp_cache.of_dir dir
  in
  let module C = (val cache: Ldp_http.Cache) in
  let module H = Ldp_http.Cached_http (C) (Make(P)) in
  Lwt.return (module H : Ldp_http.Http)
