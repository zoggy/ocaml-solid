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

open Ldp_containers

let string_of_tree tree =
  let b = Buffer.create 256 in
  let p fmt = Printf.bprintf b fmt in
  let rec iter margin t =
    p "%s%s\n" margin (Ldp_containers.node_text t);
    match t with
      CONT (_,l) ->
        List.iter (iter (margin^"  ")) l
    | _ -> ()
  in
  iter "" tree ;
  Buffer.contents b

let f args http =
  match args with
    [] -> Lwt.return_unit
  | iris ->
      let module H = (val http : Ldp_http.Http) in
      let module C = Ldp_containers.Make(H) in
      let%lwt trees = Lwt_list.map_p
        (fun iri -> C.containers (Iri.of_string iri)) iris
      in
      Lwt_list.iter_s
        (fun t -> Lwt_io.write Lwt_io.stdout (string_of_tree t))
        trees

let () = Solid_tool_common.main f