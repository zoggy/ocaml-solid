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

let options = []

let f args http =
  match args with
    [] -> Lwt.return_unit
  | iris ->
      let module H = (val http : Ldp_http.Http) in
      let f iri =
        try H.delete (Iri.of_string iri)
        with Ldp_types.Error e ->
            let msg = Printf.sprintf "%s: %s" iri
              (Ldp_types.string_of_error e)
            in
            Lwt_io.write_line Lwt_io.stdout msg
      in
      Lwt_list.iter_p f iris

let () = Solid_tool_common.main ~options f
