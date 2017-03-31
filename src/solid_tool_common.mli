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

type profile = {
  id : string;
  cert : (string * string) option; (** cert * priv key *)
  certificates : string option;
  cache : string option;
  debug: bool ;
}
val default_profile : profile
val profile_wrapper : profile Ocf.Wrapper.t
val usage : string
val ldp_http_curl : profile -> (module Ldp_http.Http) Lwt.t
val ldp_http_tls : profile -> (module Ldp_http.Http) Lwt.t
val profiles : profile list Ocf.conf_option
val map_filename : ?dir:string -> string -> string
val find_profile : string -> profile
val parse :
  ?options:(Arg.key * Arg.spec * Arg.doc) list ->
  ?usage:Arg.usage_msg ->
  unit -> (string list * (module Ldp_http.Http)) Lwt.t
val print_alert : string -> Tls.Packet.alert_type -> unit Lwt.t
val print_fail : string -> Tls.Engine.failure -> unit Lwt.t
val main :
  ?options:(Arg.key * Arg.spec * Arg.doc) list ->
  ?usage:Arg.usage_msg ->
  (string list -> (module Ldp_http.Http) -> unit Lwt.t) -> unit
