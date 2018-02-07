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

open Cohttp

type meta =
  { iri : Iri.t ;
    acl : Iri.t option ;
    meta: Iri.t option ;
    user: string option ;
    websocket: Iri.t option ;
    editable : Code.meth list ;
    exists: bool ;
    info: Response.t * Cohttp_lwt.Body.t ;
  }

type rdf_resource =
  { meta : meta ;
    graph: Rdf_graph.graph ;
    src: string * string ; (** mime-type * contents *)
  }

type resource =
| Container of rdf_resource
| Rdf of rdf_resource
| Non_rdf of string * string option (* mime type * content *)

val container_children : Rdf_graph.graph -> Iri.t list

type error = ..
exception Error of error
val error : error -> 'a
val fail : error -> 'a Lwt.t
val ref_string_of_error : (error -> string) ref
val string_of_error : error -> string
val register_string_of_error : ((error -> string) -> error -> string) -> unit
type error +=
| Invalid_method of string
| Missing_pred of Iri.t * Iri.t
| Missing_pred_iri of Iri.t * Iri.t
| Request_error of Iri.t * string (* with an exception ?? *)
| Parse_error of Iri.t * exn
| Unsupported_format of Iri.t * string

val split_string : ?keep_empty:bool -> string -> char list -> string list
val methods_of_string : string -> Code.meth list

val mime_of_content_type : string -> string
