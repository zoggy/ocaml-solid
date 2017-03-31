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

type Ldp_types.error +=
  | Rules_not_initialized

let () = Ldp_types.register_string_of_error
  (fun fb -> function
     | Rules_not_initialized ->
         Printf.sprintf "FS routes not initialized"
     | e -> fb e)

module IMap = Map.Make (struct type t = int let compare x y = x - y end)
type groups = {
    id:unit -> int;
    mutable map : string IMap.t ;
  }

let new_groups () =
  let id = let cpt = ref 0 in fun () -> incr cpt; !cpt in
  { id ; map = IMap.empty }

let add_group g str =
  let id = g.id () in
  { g with map = IMap.add id str g.map }

let get_group g id =
  try Some (IMap.find id g.map)
  with Not_found -> None

type rule =
  { host : Re.re option ;
    path : Re.re option ;
    read_only: bool ;
    root : string ;
  }

let rules = ref None

let try_host uri g = function
  None -> Some g
| Some re ->
    let h = match Uri.host uri with None -> "" | Some h -> h in
    match Re.exec_opt re h with
      None -> None
    | Some re_g ->
        let t = Re.Group.all re_g in
        let g = Array.fold_left add_group g
          (Array.sub t 1 ((Array.length t) - 1))
        in
        Some g

let try_path uri g = function
  None -> Some (g, 0)
| Some re ->
    let path = Uri.path uri in
    match Re.exec_opt re path with
      None -> None
    | Some re_g ->
        let t = Re.Group.all re_g in
        let g = Array.fold_left add_group g
          (Array.sub t 1 ((Array.length t) - 1))
        in
        let offset = Re.Group.stop re_g 0 in
        Some (g, offset)

let re_repl_group =
  Re.(compile (seq [char '{'; group (rep1 digit) ; char '}' ]))

let apply_groups g str =
  let f t =
    let n = int_of_string (Re.Group.get t 1) in
    match get_group g n with
      None -> Server_log._warn
        (fun m -> m "FS route: no group %d in %S" n str);
        ""
    | Some s -> s
  in
  Re.replace ~all: true re_repl_group ~f str

let split_string ?(keep_empty=false) s chars =
  let re =
    Re.(
     let re = alt (List.map char chars) in
     let re = if keep_empty then re else rep1 re in
     compile re
    )
  in
  Re.split re s

let split_path =
  let re = Re.(compile (rep1 (char '/'))) in
  fun s ->
    match Re.split re s with
    | "" :: q -> q
    | l -> l

let normalize =
  let rec iter acc = function
    [] -> List.rev acc
  | "." :: q -> iter acc q
  | ".." :: q ->
      begin
        match acc with
          [] -> iter acc q
        | _ :: acc -> iter acc q
      end
  | h :: q -> iter (h :: acc) q
  in
  iter []

let try_rule root uri r =
  let g = new_groups () in
  match try_host uri g r.host with
    None -> None
  | Some g ->
      match try_path uri g r.path with
        None -> None
      | Some (g, offset) ->
          Server_log._debug
            (fun f -> f "root=%s, r.root=%s, is_relative:%b, is_implicit:%b"
              root r.root (Filename.is_relative r.root) (Filename.is_implicit r.root));
          let dir =
            if Filename.is_relative r.root then
              if Filename.is_implicit r.root then
                Filename.concat root r.root
              else
                r.root
            else
              r.root
          in
          let root_dir = apply_groups g dir in
          let p = Uri.path uri in
          let rel =
            let len = String.length p in
            List.map Uri.pct_decode
              (split_path (String.sub p offset (len - offset)))
          in
          let root_iri_path =
            List.map Uri.pct_decode
              (split_path (String.sub p 0 offset))
          in
          Server_log._debug
            (fun f -> f "Route: root_iri_path=%S\nroot_dir=%S\nrel=%S"
               (String.concat "|" root_iri_path) root_dir
                 (String.concat "|" rel));
          Some (root_iri_path, root_dir, rel, r.read_only)

let route root uri =
  match !rules with
    None -> Ldp_types.error Rules_not_initialized
  | Some rules ->
      Server_log._debug (fun f -> f "%s" "applying routing rules");
      let path = Uri.path uri in
      let path = split_path path in
      let path = List.map Uri.pct_decode path in
      let path = normalize path in
      match rules with
      | [] ->  Some ([], root, path, false)
      | rules ->
          let path = List.map Uri.pct_encode path in
          let path = "/"^(String.concat "/" path) in
          let uri = Uri.with_path uri path in
          let rec iter = function
            [] -> None
          | rule :: q ->
              match try_rule root uri rule with
                None -> iter q
              | x -> x
          in
          iter rules

let init l =
  let map f = function None -> None | Some x -> Some (f x) in
  let l = List.map
    (fun r -> {
         host = map (fun x -> Re.compile (Re_pcre.re x)) r.Server_conf.host ;
         path = map (fun x -> Re.compile (Re_pcre.re x)) r.Server_conf.path ;
         root = r.Server_conf.root ;
         read_only = r.Server_conf.read_only ;
       })
    l
  in
  rules := Some l
