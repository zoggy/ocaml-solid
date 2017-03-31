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

(** Main module of SOLID server *)

let conf_options = Server_conf.add_options Ocf.group

let webid = ref None
let pem = ref None
let name = ref None
let cert_label = ref None
let profile = ref false

type mode = Server | Add_user of string
let mode = ref Server

let use_curl = ref false

let options =
  [ "-c", Arg.String (Ocf.from_file conf_options),
    "file load configuration from file" ;

    "--dump-options",
    Arg.Unit (fun () -> print_endline (Ocf.to_string conf_options); exit 0),
    " print current configuration" ;

    "--add-user", Arg.String (fun login -> mode := Add_user login),
    "login add user workspace in home root" ;

    "--webid", Arg.String (fun s -> webid := Some s),
    "iri use iri as webid for new user" ;

    "--pem", Arg.String (fun s -> pem := Some s),
    "file get modulus, exponent and eventually webid from x509 pem file" ;

    "--name", Arg.String (fun s -> name := Some s),
    "name use name in profile if a profile is generated" ;

    "--cert-label", Arg.String (fun s -> cert_label := Some s),
    "label associate label to the key defined in profile, if a profile is generated" ;

    "--profile", Arg.Set profile,
    " create profile/card[,acl]" ;

    "--curl", Arg.Set use_curl,
    " use curl to execute GET requests"
  ]

let usage = Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0)
let main () =
  match Arg.parse (Arg.align options) (fun _ -> ()) usage with
  | exception Arg.Bad msg -> Lwt.fail_with msg
  | exception Ocf.Error e -> Lwt.fail_with (Ocf.string_of_error e)
  | () ->
      Server_fs_route.init (Ocf.get Server_conf.storage_rules) ;
      match !mode with
      | Add_user login ->
          Server_user.add ?name:!name  ?cert_label:!cert_label
            ?pem:!pem ?webid:!webid ~profile:!profile login
      | Server ->
          let%lwt () = Server_auth.init_http ~curl: !use_curl in
          let%lwt () = Server_log._app_lwt
            (fun m -> m "Using documents from %s" (Ocf.get Server_conf.storage_root))
          in
          let%lwt () = Server_log._app_lwt
            (fun m -> m "Starting HTTPS server on port %d"
               (Ocf.get Server_conf.port))
          in
          Server_http_tls.server Server_webmachine.http_handler

let () =
  Logs.set_reporter (Server_log.lwt_reporter ());
  try Lwt_main.run (main ())
  with
    e ->
      let msg =
        match e with
          Failure msg
        | Sys_error msg -> msg
        | Unix.Unix_error (e,s1,s2) ->
            Printf.sprintf "%s %s: %s"
              (Unix.error_message e) s2 s1
        | e ->
            Printexc.to_string e
      in
      prerr_endline msg ;
      exit 1
