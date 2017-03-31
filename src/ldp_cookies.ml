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

type cookie_domain = string * bool (* host * with-subdomains *)
type cookie_path = string list

module Make () =
  struct
    let is_suffix ~s ~suf =
      let len = String.length s in
      let len_suf = String.length suf in
      len >= len_suf &&
        String.sub s (len - len_suf) len_suf = suf

    let is_prefix ~s ~pref =
      let len_s = String.length s in
      let len_pref = String.length pref in
      (len_pref <= len_s) &&
        (String.sub s 0 len_pref) = pref

    let print_cookies dbg (str, cookie) =
      let (a, b) = Cohttp.Cookie.Set_cookie_hdr.serialize cookie in
      let str = Printf.sprintf "cookie: %s => (%s, %s)"
        str a b
      in
      dbg str

    let cookies = ref []

    let remove_cookies () =
      cookies := List.filter
        (fun (_,_,exp,_) ->
           match exp with
             None -> true
           | Some t -> t >= Unix.time()
        ) !cookies

    let add_cookie iri (_, c) =
      let open Cohttp.Cookie.Set_cookie_hdr in
      (*let (k, v) = c.cookie in
      prerr_endline (Printf.sprintf "Set-cookie: %s -> %s" k v);*)
      let domain =
        match c.domain with
        | None -> ((match Iri.host iri with None -> "" | Some s -> s), false)
        | Some s -> (s, true)
      in
      let path =
        match c.path with
        | None -> Iri.path_string iri
        | Some p -> p
      in
      let exp =
        match c.expiration with
          `Session -> None
        | `Max_age n -> Some (Unix.time () +. (Int64.to_float n))
      in
      let c = c.cookie in
      cookies := (domain, path, exp, c) :: !cookies

    let use_cookie host path (c_domain, sub) c_path exp =
      let b =
        (match exp with
           None -> true
         | Some t -> t >= Unix.time ()
        ) &&
          (sub && is_suffix ~s: host ~suf: c_domain) || c_domain = host
          && is_prefix ~s: path ~pref: c_path
      in
      (*prerr_endline (Printf.sprintf
        "use_cookie %s %s (%s, %b) %s _ = %b" host path c_domain sub c_path b
      *)
      b

    let cookies_by_iri iri =
      match Iri.host iri with
        None -> []
      | Some host ->
          let path = Iri.path_string iri in
          List.fold_left
            (fun acc (dom, p, exp, c) ->
               if use_cookie host path dom p exp then c :: acc else acc)
            []
            !cookies
  end
