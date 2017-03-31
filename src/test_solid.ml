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


let id_iri = Iri.of_string "https://zoggy.databox.me/profile/card#me"
(*let id_iri = "https://databox.me/"*)

open Ldp_types
open Ldp_http

let doc_meta url =
  let g = Rdf_graph.open_graph url in
  Rdf_ttl.from_string g {|
<> a <http://rdfs.org/sioc/ns#Post> ;
    <http://purl.org/dc/terms/title> "Hello HTML doc!" .
    |};
  let str = Rdf_ttl.to_string g in
  dbg_ "doc_meta %s => %s" (Iri.to_string url) str;
  str

let doc_css = {|
  h1 { background-color: black;
       color: green;
     } |}

let doc_html css = Printf.sprintf
  "<html>
    <head>
      <title>Hello HTML document !</title>
      <link rel=\"stylesheet\" type=\"text/css\" href=\"%s\"/>
    </head>
    <body>
    <h1>Hello HTML document!</h1>
    <p>bla bla bla</p>
    <p>bla bla bla</p>
    </body>
  </html>" (Iri.to_uri css)

let t =
  try%lwt
    let%lwt user = login ~url: id_iri () in
    dbg (Printf.sprintf "User=%s" (match user with None -> "" | Some u -> u));
    let%lwt prefs = get
      (Iri.of_string "https://zoggy.databox.me/Preferences/prefs.ttl")
    in
    let%lwt () =
      match prefs with
        Rdf { meta = { websocket = Some upd_iri } ;graph } ->
          dbg (Rdf_ttl.to_string graph) ;
          begin
            try%lwt
              let%lwt upd = Solid_updates.create upd_iri in
              Solid_updates.sub upd (Iri.of_string "https://zoggy.databox.me/Public");
              Solid_updates.on_pub upd
                (fun iri -> dbg_ "receveid update: pub %s" (Iri.to_string iri));
              Lwt.return_unit
            with
              e ->
                dbg (Printexc.to_string e);
                Lwt.return_unit
          end
      | _ ->
          dbg "No updates iri";
          Lwt.return_unit
    in
    let%lwt meta_css = post_non_rdf ~data: doc_css ~mime: "text/css"
      (Iri.of_string "https://zoggy.databox.me/Public/style.css")
    in
    let%lwt meta = post_non_rdf
      ~data: (doc_html meta_css.iri)
      ~mime: "text/html"
      (Iri.of_string "https://zoggy.databox.me/Public/doc.html")
    in
    dbg (Printf.sprintf "Post meta.iri=%s" (Iri.to_string meta.iri));
    match meta.meta with
      None -> dbg "No meta URL"; Lwt.return_unit
    | Some iri_meta ->
        dbg (Printf.sprintf "Meta IRI=%s" (Iri.to_string iri_meta));
        let iri = meta.iri in
        let%lwt meta = put ~data: (doc_meta iri) iri_meta in
        dbg (Printf.sprintf "Put ok");
        let%lwt meta = head (Iri.to_uri iri) in
        dbg_meta meta ;
        let ins = Rdf_graph.open_graph iri in
        ins.Rdf_graph.add_triple
          ~sub: (Rdf_term.Iri iri) ~pred: Rdf_dc.title
          ~obj: (Rdf_term.term_of_literal_string "Hello title 2") ;
        let%lwt () = patch ~ins iri_meta in
        let%lwt ws = Solid_profile.get_workspaces id_iri in
        List.iter
          (fun ws ->
            dbg (Printf.sprintf "workspace: title=%s, iri=%s"
              ws.Solid_profile.ws_title
              (Iri.to_string ws.Solid_profile.ws_iri))
          )
          ws;
        Lwt.return_unit
  with
    Ldp_types.Error e ->
      dbg (Printf.sprintf "Error: %s" (Ldp_types.string_of_error e));
      Lwt.return_unit
  | Iri.Error e ->
      dbg (Printf.sprintf "Error: %s" (Iri.string_of_error e));
      Lwt.return_unit
  | Rdf_ttl.Error e ->
      dbg (Printf.sprintf "Error: %s" (Rdf_ttl.string_of_error e));
      Lwt.return_unit
  | e ->
      dbg (Printexc.to_string e);
      Lwt.return_unit



