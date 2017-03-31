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

(** Logging of the server. *)

val src: Logs.src

val _app: 'a Logs.log
val _err: 'a Logs.log
val _warn: 'a Logs.log
val _info: 'a Logs.log
val _debug: 'a Logs.log

val _app_lwt: 'a Logs_lwt.log
val _err_lwt: 'a Logs_lwt.log
val _warn_lwt: 'a Logs_lwt.log
val _info_lwt: 'a Logs_lwt.log
val _debug_lwt: 'a Logs_lwt.log

val log_level : Logs.level option Ocf.conf_option

val lwt_reporter : unit -> Logs.reporter
