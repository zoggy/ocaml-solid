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

let src = Logs.Src.create ~doc:"log of the ocaml-solid library" "solid"

let __err f = Logs.err ~src f
let __warn f = Logs.warn ~src f
let __info f = Logs.info ~src f
let __debug f = Logs.debug ~src f

let __err_lwt f = Logs_lwt.err ~src f
let __warn_lwt f = Logs_lwt.warn ~src f
let __info_lwt f = Logs_lwt.info ~src f
let __debug_lwt f = Logs_lwt.debug ~src f

open Logs

let level_wrapper =
  let to_json ?with_doc = function
    App -> `String "app"
  | Error -> `String "error"
  | Warning -> `String "warning"
  | Info -> `String "info"
  | Debug -> `String "debug"
  in
  let from_json ?(def=App) = function
  `String s ->
      begin
        match String.lowercase_ascii s with
          "app" | "0" -> App
        | "error" | "1" -> Error
        | "warning" | "2" -> Warning
        | "info" | "3" -> Info
        | "debug" | "4" -> Debug
        | _ -> def
      end
  | _ -> prerr_endline "bad value";def
  in
  let w = Ocf.Wrapper.make to_json from_json in
  Ocf.Wrapper.option w

let log_level = Ocf.option
  ~cb: (Logs.Src.set_level src)
  level_wrapper None
