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

(** *)

(** Only WebId-TLS by now *)

let expiration_delay = Ocf.int (60 * 60) (* 1 hour *)

type entry = {
    graph : Rdf_graph.graph ;
    exp_time : float ;
  }

let entry graph = {
    graph ;
    exp_time = Unix.time () +. float (Ocf.get expiration_delay) ;
  }

type state = Retrieving of Rdf_graph.graph Lwt.t | Present of entry

let (cache : state Iri.Map.t ref) = ref Iri.Map.empty

let http = ref None

let get iri =
  match !http with
    None ->
      let%lwt () = Server_log._err_lwt
        (fun f -> f "Server_auth.http not initialized!")
      in
      Lwt.return (Rdf_graph.open_graph iri)
  | Some http ->
      let module H = (val http : Ldp_http.Http) in
      H.get_rdf_graph iri

let set_retrieving iri =
  let (t, wakener) = Lwt.wait () in
  cache := Iri.Map.add iri (Retrieving t) !cache ;
  let set g =
    let e = entry g in
    cache := Iri.Map.add iri (Present e) !cache ;
    Lwt.wakeup wakener g ;
    t
  in
  match%lwt get iri with
  | exception e ->
      let%lwt () = Server_log._err_lwt
        (fun f -> f "GET %s: %s" (Iri.to_string iri)
           (Printexc.to_string e))
      in
      set (Rdf_graph.open_graph iri)
  | g -> set g

let get iri =
  match Iri.Map.find iri !cache with
  | exception Not_found -> set_retrieving iri
  | Retrieving t -> t
  | Present e when e.exp_time < Unix.time () ->
      set_retrieving iri
  | Present e -> Lwt.return e.graph

let iri_of_cert cert =
  match List.find
    (function `URI _ -> true | _ -> false)
    (X509.Extension.subject_alt_names cert)
  with
  | exception Not_found -> 
      Server_log._debug
        (fun f -> f "No URI in cert");
      None
  | `URI str -> Some (Iri.of_string str)
  | _ -> assert false

let verify_webid ~exp ~modu webid g =
  let exp = Rdf_term.term_of_literal_string
    ~typ: (Rdf_rdf.xsd_"integer")
    (Z.to_string  exp)
  in
  let modu = Rdf_term.term_of_literal_string
    ~typ: (Rdf_rdf.xsd_ "hexBinary")
    (Z.format "0x" modu)
  in
  let q = Printf.sprintf
    "PREFIX : <http://www.w3.org/ns/auth/cert#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
ASK { %s :key [ :modulus ?modu ; :exponent ?exp ; ] .
      FILTER (?exp = %s && ?modu = %s)
    }"
    Rdf_term.(string_of_term (Iri webid))
      (Rdf_term.string_of_term exp)
      (Rdf_term.string_of_term modu)
  in
  Server_log._debug
    (fun f -> f "auth webid sparql = %s" q);
  let q = Rdf_sparql.query_from_string q in
  let dataset = Rdf_ds.simple_dataset g in
  Rdf_sparql.ask ~base:(g.Rdf_graph.name()) dataset q

let user_of_cert cert =
  match iri_of_cert cert with
    None -> Lwt.return_none
  | Some webid ->
      let%lwt () = Server_log._debug_lwt
        (fun f -> f "client claims webid %S" (Iri.to_string webid))
      in
      match X509.public_key cert with
      | `RSA { Nocrypto.Rsa.e ; n} ->
          begin
            let graph_iri = Iri.with_fragment webid None in
            let graph_iri = Iri.with_query graph_iri None in
            let%lwt g = get graph_iri in
            if verify_webid ~exp: e ~modu: n webid g then
              let%lwt () = Server_log._debug_lwt
                (fun f -> f "webid %s verified" (Iri.to_string webid))
              in
              Lwt.return_some webid
            else
              Lwt.return_none
          end
      | _ -> Lwt.return_none

let init_http ~curl =
  let dbg s = Server_log._debug_lwt (fun f -> f "%s" s) in
  let%lwt h = if curl then
      Ldp_curl.make ?cache: None ?cert:None ~dbg
    else
      Ldp_tls.make ?cache: None ?cert:None ~dbg
  in
  http := Some h;
  Lwt.return_unit
