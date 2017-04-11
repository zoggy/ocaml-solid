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

module Client = Cohttp_lwt_xhr.Make_client_async(struct
    let chunked_response = true
    let chunk_size = 128 * 1024
    let convert_body_string = Js.to_bytestring
    let with_credentials = true
  end)

module type P =
  sig
    val dbg : string -> unit Lwt.t
  end

module Make (P:P) =
  struct
    let dbg = P.dbg
    let call ?body ?chunked ?headers meth iri =
      Client.call ?body ?chunked ?headers meth
        (Uri.of_string (Iri.to_string iri))
  end

module Dbg =
  Make (struct
    let dbg s = Firebug.console##log (Js.string s); Lwt.return_unit
  end)

module Nodbg = Make (struct let dbg s = Lwt.return_unit end)

let login_iri () =
  let w = Dom_html.window in
  let loc = w##.location in
  let o = Js.to_string (Dom_html.location_origin loc) in
  let p = Js.to_string (loc##.pathname) in
  Iri.of_string (o ^ p)
