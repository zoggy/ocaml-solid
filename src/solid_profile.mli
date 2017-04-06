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

type profile = Rdf_graph.graph
type workspace =
  { ws_title : string;
    ws_iri : Iri.t;
    ws_triples : Rdf_term.triple list;
  }


module type S =
  sig
    val get_profile : Iri.t -> profile Lwt.t
    val inbox : profile -> Iri.t option
    val workspaces : ?typ:Iri.t -> profile -> workspace list
    val storages : profile -> Iri.t list
    val name : profile -> string
    val pim : profile -> Rdf_pim.from
    val preferences_ws : profile -> workspace list
    val private_ws : profile -> workspace list
    val public_ws : profile -> workspace list
    val shared_ws : profile -> workspace list
    val master_ws : profile -> workspace list
  end

module Make : Ldp_http.Http -> S

