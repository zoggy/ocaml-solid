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

(** Server configuration options. *)

type https = {
    (* ".pem file of server certificate" *)
    server_cert : string
      [@ocf Ocf.Wrapper.string, "./server-certificates/server.pem"]
      [@ocf.label "cert_file"] ;

    (* ".key file of server key" *)
    server_key : string
      [@ocf Ocf.Wrapper.string, "./server-certificates/server.key"]
      [@ocf.label "key_file"] ;

    (* "CA file of server" *)
    server_ca : string option
      [@ocf Ocf.Wrapper.(option string), None]
      [@ocf.label "ca_file"] ;

    (* "port number to listen to" *)
    port : int [@ocf Ocf.Wrapper.int, 9999] ;
  } [@@ocf]

let https = Ocf.option (Ocf.Wrapper.option https_wrapper) None

type http = {
    (* "hostname; use '0.0.0.0' to reply to request adressed
       to any hostname" *)
    host : string [@ocf Ocf.Wrapper.string, "localhost"] ;

    (* "port number to listen to" *)
    port : int [@ocf Ocf.Wrapper.int, 9999] ;

    (* "HTTP header used to pass client certificate" *)
    client_cert_header : string
      [@ocf Ocf.Wrapper.string, "X-SSL-Cert"] ;

  } [@@ocf]

let http = Ocf.option (Ocf.Wrapper.option http_wrapper) None

let filename_wrapper =
  let to_json ?with_doc fn = `String fn in
  let from_json ?def = function
    `String str ->
      begin
        if Filename.is_relative str then
          Filename.concat (Sys.getcwd()) str
        else
          str
      end
  | json -> Ocf.invalid_value json
  in
  Ocf.Wrapper.make to_json from_json

let storage_root = Ocf.option filename_wrapper
  ~doc:"root directory to store served documents"
  "www"

let json_wrapper =
  let to_json ?with_doc x = x in
  let from_json ?def x = x in
  Ocf.Wrapper.make to_json from_json

let default_fs_type = "unix"
type fs_map_rule = {
    host : string option [@ocf Ocf.Wrapper.(option string), None] ;
    path : string option [@ocf Ocf.Wrapper.(option string), None] ;
    root : string [@ocf Ocf.Wrapper.string, ""] ;
    options : Yojson.Safe.json [@ocf json_wrapper, `Assoc []] ;
    fs_type : string [@ocf Ocf.Wrapper.string, default_fs_type] ;
  } [@@ocf]

let storage_rules = Ocf.list fs_map_rule_wrapper
  ~doc:"mappings from uri to file system"
  []

let () = Logs.set_level ~all: true (Some Logs.Warning)
let global_log_level = Ocf.option
  ~cb: (fun l -> Logs.set_level ~all: true l)
(*     prerr_endline (Printf.sprintf "level set to %s" (Logs.level_to_string l)))*)
  Ldp_log.level_wrapper (Logs.level ())

let container_listing = Ocf.(option_
   ~doc: "GET text/html on container lists content or use an existing file; \
   null means the server will return 415, else each file will be tried and if \
   none exists the server will build a simple HTML page."
  (Wrapper.list Wrapper.string) (Some ["index.html";"index.xhtml"])
  )

let add_options g =
  let storage =
    let g = Ocf.group in
    let g = Ocf.add g ["root"] storage_root in
    let g = Ocf.add g ["rules"] storage_rules in
    g
  in
  let ldp =
    let g = Ocf.group in
    let g = Ocf.add g ["container_listing"] container_listing in
    g
  in
  let log =
    let g = Ocf.group in
    let g = Ocf.add g ["global"] global_log_level in
    let g = Ocf.add g ["library"] Ldp_log.log_level in
    let g = Ocf.add g ["server"] Server_log.log_level in
    g
  in
  let g = Ocf.add g ["https"] https in
  let g = Ocf.add g ["http"] http in
  let g = Ocf.add_group g ["storage"] storage in
  let g = Ocf.add_group g ["ldp"] ldp in
  let g = Ocf.add_group g ["log"] log in
  g
