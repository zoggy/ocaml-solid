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

type tree =
  | NRDF of Iri.t * string
  | RDF of Iri.t
  | CONT of Iri.t * tree list

let node_text t =
  match t with
    CONT (iri, _) -> Iri.to_string iri
  | RDF iri -> Iri.to_string iri
  | NRDF (iri, ct) -> Printf.sprintf "%s [%s]" (Iri.to_string iri) ct

module type S =
  sig
    val containers : Iri.t -> tree
  end

module Make (P:Ldp_http.Http) =
  struct
    let containers iri =
      let rec iter iri =
        match%lwt P.get iri with
          Ldp_types.Container r ->
            let children = Ldp_types.container_children r.Ldp_types.graph in
            let%lwt children = Lwt_list.map_p iter children in
            Lwt.return (CONT (iri, children))
        | Ldp_types.Rdf r -> Lwt.return (RDF iri)
        | Ldp_types.Non_rdf (ct, _) -> Lwt.return (NRDF (iri, ct))
      in
      iter iri
  end
