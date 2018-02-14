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

open Lwt.Infix
open Rdf_acl.Open
open Rdf_graph
open Rdf_term

(** See {{:https://github.com/solid/web-access-control-spec#individual-resource-acls} Solid WAC} *)

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
    Rdf_webacl.add_rights_of_modes acc modes
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
  Lwt.return (List.fold_left (gather_rights g user) Rdf_webacl.no_right auths)

module type Acl =
  sig
    val rights_for_path : Iri.t option -> Server_fs.path -> Rdf_webacl.rights Lwt.t
    val available_container_listings : Iri.t option -> Server_fs.path ->
      (string * (unit -> string Lwt.t)) list Lwt.t
  end
module Make (Fs:Server_fs.Fs) : Acl =
  struct
    let rights_for_path user p =
      let rec iter ~default p =
        let acl = Server_fs.acl_path p in
        match%lwt Fs.read_path_graph acl with
        | Some g ->
            begin
              let r = rights ~default user g (Server_fs.iri p) in
              match Server_fs.kind p with
                `Acl x ->
                  (* add control if control right is provided on x *)
                  let p2 = Server_fs.noext_path p in
                  let%lwt r2 = rights ~default user g (Server_fs.iri p2) in
                  if Rdf_webacl.has_control r2 then
                    Lwt.return Rdf_webacl.all_rights
                  else
                    r
              | _ ->
                  r
            end
        | None ->
            match%lwt Fs.parent p with
              None -> Server_log._err
                (fun m -> m "No root acl in %s" (Fs.path_to_filename p));
                Lwt.return Rdf_webacl.no_right
            | Some parent -> iter ~default: true parent
      in
      iter ~default: false p

    let fold_listings user path acc basename =
      let%lwt p = Fs.append_rel path [basename] in
      match Server_fs.kind p with
        `File ->
          begin
            let%lwt rights = rights_for_path user p in
            if Rdf_webacl.has_read rights then
              let%lwt mime = Fs.path_mime p in
              let reader () = Fs.string_of_path p in
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
                let can_read p = rights_for_path user p >|= Rdf_webacl.has_read in
                let l = l @ [Ldp_http.mime_xhtml,
                    fun () -> Fs.default_container_listing path can_read]
            in
                Lwt.return l
          end
      | _ -> Lwt.return []
  end

