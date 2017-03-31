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

open Rdf_acl.Open
open Rdf_graph
open Rdf_term

(** See {{:https://github.com/solid/web-access-control-spec#individual-resource-acls} Solid WAC} *)

type rights = int
let no_right = 0

let add_read = (lor) 1
let rem_read = (land) (lnot 1)
let has_read r = r land 1 <> 0
let add_write = (lor) 2
let rem_write = (land) (lnot 2)
let has_write r = r land 2 <> 0
let add_append = (lor) 4
let rem_append = (land) (lnot 4)
let has_append r = r land 4 <> 0
let add_control = (lor) 8
let rem_control = (land) (lnot 8)
let has_control r = r land 8 <> 0

let all_rights = add_read (add_write (add_append (add_control no_right)))

(* FIXME: acl:defautlForNew will be renamed acl:default *)
let auths ~default g iri =
  Server_log._debug (fun m -> m "%s" (Rdf_ttl.to_string g));
  let pred =
    if default then acl_defaultForNew else acl_accessTo
  in
  let filter a =
    (*prerr_endline (Printf.sprintf "filter sub=%s, pred=%s, obj=%s"
      (Iri.to_string a) (Iri.to_string pred) (Iri.to_string iri));*)
    g.exists ~sub: (Iri a) ~pred ~obj:(Iri iri) ()
  in
  List.filter filter
    (iri_subjects_of g ~pred:Rdf_rdf.type_ ~obj: (Iri acl_c_Authorization))

let add_rights_of_modes =
  List.fold_left
    (fun acc mode ->
       if Iri.equal mode acl_c_Read then
         add_read acc
       else if Iri.equal mode acl_c_Write then
          add_write acc
         else if Iri.equal mode acl_c_Append then
             add_append acc
           else if Iri.equal mode acl_c_Control then
               add_control acc
             else acc)

let gather_rights g user acc auth =
  let sub = Iri auth in
  let modes = iri_objects_of g ~sub ~pred:acl_mode in
  if
    g.exists ~sub ~pred:acl_agentClass ~obj:(Iri (Rdf_foaf.c_Agent)) ()
      ||
      match user with
        None -> false
      | Some user -> g.exists ~sub ~pred:acl_agent ~obj:(Iri user) ()
  then
    add_rights_of_modes acc modes
  else
    acc

(** FIXME: we don't handle groups of agent yet *)
(** FIXME: handle control access over ,acl *)
let rights ~default user g iri =
  let auths = auths ~default g iri in
  let%lwt () =
    Server_log._debug_lwt
      (fun m -> m "auths for %s:\n  %s" (Iri.to_string iri)
      (String.concat "\n  " (List.map Iri.to_string auths))
      )
  in
  Lwt.return (List.fold_left (gather_rights g user) no_right auths)

let rights_for_path user p =
  let rec iter ~default p =
    let acl = Server_fs.acl_path p in
    match%lwt Server_fs.read_path_graph acl with
    | Some g ->
        begin
          let r = rights ~default user g (Server_fs.iri p) in
          match Server_fs.kind p with
            `Acl x ->
              (* add control if control right is provided on x *)
              let p2 = Server_fs.noext_path p in
              let%lwt r2 = rights ~default user g (Server_fs.iri p2) in
              if has_control r2 then
                Lwt.return all_rights
              else
                r
          | _ ->
              r
        end
    | None ->
        match%lwt Server_fs.parent p with
          None -> Server_log._err
            (fun m -> m "No root acl in %s" (Server_fs.path_to_filename p));
            Lwt.return no_right
        | Some parent -> iter ~default: true parent
  in
  iter ~default: false p

let fold_listings user path acc basename =
  (* FIXME: not the most efficient computation...
     but let's keep things simple by now *)
  let%lwt p = Server_fs.append_rel path [basename] in
  match Server_fs.kind p with
    `File ->
      begin
        let%lwt rights = rights_for_path user p in
        if has_read rights then
          let absfile = Server_fs.path_to_filename p in
          let%lwt mime =
            match%lwt Server_fs.path_mime p with
            | Some x -> Lwt.return x
            | None -> Lwt.return (Magic_mime.lookup absfile)
          in
          let reader () = Lwt_io.(with_file ~mode:Input absfile read) in
          Lwt.return ((mime, reader)::acc)
        else
          Lwt.return acc
      end
  | _  -> Lwt.return acc

let available_container_listings user path =
  match Server_fs.kind path with
  | `Dir ->
      begin
        match Ocf.get Server_conf.container_listing with
          None -> Lwt.return []
        | Some files ->
            let%lwt l = Lwt_list.fold_left_s
              (fold_listings user path) [] files
            in
            let l = l @ [Server_page.mime_xhtml,
              fun () -> Server_fs.default_container_listing path]
            in
            Lwt.return l
      end
  | _ -> Lwt.return []
