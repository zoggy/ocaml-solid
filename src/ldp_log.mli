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

(** Logging of the library. *)

val src: Logs.src

val __err: 'a Logs.log
val __warn: 'a Logs.log
val __info: 'a Logs.log
val __debug: 'a Logs.log

val __err_lwt: 'a Logs_lwt.log
val __warn_lwt: 'a Logs_lwt.log
val __info_lwt: 'a Logs_lwt.log
val __debug_lwt: 'a Logs_lwt.log

(** @raise Failure if string does not correspond to a valid level. *)
val level_of_string : string -> Logs.level

val level_wrapper: Logs.level option Ocf.wrapper
val log_level : Logs.level option Ocf.conf_option

