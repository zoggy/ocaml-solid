
module Ldp = Rdf_ldp
module Xhr = XmlHttpRequest
open Lwt.Infix;;

type error =
  | Post_error of int * Iri.t
  | Get_error of int * Iri.t
  | Put_error of int * Iri.t

exception Error of error
let error e = Lwt.fail (Error e)

let string_of_error = function
  Post_error (code, iri) ->
    Printf.sprintf "POST error (%d, %s)"
      code (Iri.to_string iri)
| Put_error (code, iri) ->
    Printf.sprintf "PUT error (%d, %s)"
      code (Iri.to_string iri)
| Get_error (code, iri) ->
    Printf.sprintf "GET error (%d, %s)"
      code (Iri.to_string iri)

let map_opt f = function None -> None | Some x -> Some (f x)
let do_opt f = function None -> () | Some x -> f x

let mime_turtle = "text/turtle"

(*c==v=[String.split_string]=1.2====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> if keep_empty then [""] else []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.2====*)

let get_link links rel =
  try Some (List.assoc rel links)
  with Not_found -> None

type meth = [
  | `DELETE
  | `GET
  | `HEAD
  | `OPTIONS
  | `PATCH
  | `POST
  | `PUT ]

let meth_of_string acc = function
  "DELETE" -> `DELETE :: acc
| "GET" -> `GET :: acc
| "HEAD" -> `HEAD :: acc
| "OPTIONS" -> `OPTIONS :: acc
| "PATCH" -> `PATCH :: acc
| "POST" -> `POST :: acc
| "PUT" -> `PUT :: acc
| _ -> acc

let string_of_meth = function
  `DELETE -> "DELETE"
| `GET -> "GET"
| `HEAD -> "HEAD"
| `OPTIONS -> "OPTIONS"
| `PATCH -> "PATCH"
| `POST -> "POST"
| `PUT -> "PUT"

let methods_of_string str =
  List.fold_left meth_of_string [] (split_string str [',';' ';'\t'])

type meta =
  { url : Iri.t ;
    acl : Iri.t option ;
    meta: Iri.t option ;
    user: string option ;
    websocket: string option ;
    editable : meth list ;
    exists: bool ;
    xhr: string Xhr.generic_http_frame ;
  }

let dbg s = Firebug.console##log (Js.string s)
let dbg_js js = Firebug.console##log(js)
let dbg_ fmt = Printf.ksprintf dbg fmt
let opt_s = function None -> "" | Some s -> s

let dbg_meta m =
  dbg_ "meta.url=%s" (Iri.to_string m.url) ;
  do_opt (dbg_ "meta.acl=%s") (map_opt Iri.to_string m.acl);
  do_opt (dbg_ "meta.meta=%s") (map_opt Iri.to_string m.meta);
  do_opt (dbg_ "meta.user=%s") m.user;
  do_opt (dbg_ "meta.websocket=%s") m.websocket

let response_metadata (resp : string Xhr.generic_http_frame) =
  let links = match resp.Xhr.headers "Link" with
      None -> []
    | Some l -> Iri.parse_http_link l
  in
  let url =
    match resp.Xhr.headers "Location" with
      None -> resp.Xhr.url
    | Some str -> str
  in
  let acl = get_link links "acl" in
  let meta =
    match get_link links "meta" with
      None -> get_link links "describedBy"
    | x -> x
  in
  let user = resp.Xhr.headers "User" in
  let websocket = resp.Xhr.headers "Updates-via" in
  let exists = resp.Xhr.code = 200 in
  let editable =
    match resp.Xhr.headers "Allow" with
      None -> []
    | Some str -> methods_of_string str
  in
  { url = Iri.of_string url ;
    acl ; meta ; user ;
    websocket ; editable ; exists ; xhr = resp }

let head url =
  Xhr.perform_raw_url
    ~headers: []
    ~override_method: `HEAD
    ~with_credentials: true
    url >|= response_metadata

let get ?accept iri =
  let hfields =
    match accept with
      None -> []
    | Some str -> ["Accept", str]
  in
  Xhr.perform_raw_url
    ~headers: hfields
    ~with_credentials: true
    (Iri.to_uri iri) >>= fun xhr ->
  let content_type = match xhr.Xhr.headers "Content-type" with
      None -> ""
    | Some str -> str
  in
  Lwt.return (content_type, xhr.Xhr.content)

let get_graph ?g iri =
  let g =
    match g with
      None -> Rdf_graph.open_graph iri
    | Some g -> g
  in
  get ~accept: mime_turtle iri >>= fun (_, str) ->
    dbg str;
    Rdf_ttl.from_string g str;
    Lwt.return g

let post ?data ?(mime=mime_turtle) ?slug ~typ ?(container=false) parent =
  let hfields =
    ["Link", Printf.sprintf "<%s>; rel=\"type\"" (Iri.to_string typ)]
  in
  let hfields =
    match slug with
      None | Some "" -> hfields
    | Some str -> ("Slug", str) :: hfields
  in
  let form_arg = map_opt (fun s -> `RawData (Js.string s)) data in
  Xhr.perform_raw_url
    ~content_type: mime
    ~headers: hfields
    ?form_arg
    ~override_method: `POST
    ~with_credentials: true
    (Iri.to_uri parent) >>= fun xhr ->
    match xhr.Xhr.code with
    | 200 | 201 -> Lwt.return (response_metadata xhr)
    | n -> error (Post_error (n, parent))

let post_container ?slug iri =
  post ?slug ~typ: Rdf_ldp.ldp_BasicContainer iri

let post_resource ?data ?slug iri =
  let data = map_opt Rdf_ttl.to_string data in
  post ?data ?slug ~typ: Rdf_ldp.ldp_Resource iri

let put ?data ?(mime=mime_turtle) iri =
  let form_arg = map_opt (fun s -> `RawData (Js.string s)) data in
  Xhr.perform_raw_url
    ~content_type: mime
    ?form_arg
    ~override_method: `PUT
    ~with_credentials: true
    (Iri.to_uri iri) >>= fun xhr ->
    match xhr.Xhr.code with
    | 200 | 201 -> Lwt.return (response_metadata xhr)
    | n -> error (Put_error (n, iri))


let post_non_rdf ?data ?mime iri =
  put ?data ?mime (*~typ: Rdf_ldp.ldp_NonRDFSource*) iri

let patch ?del ?ins iri =
  let b = Buffer.create 256 in
  (match del with
     None -> ()
   | Some g -> 
       Printf.bprintf b "DELETE DATA { %s }\n"
           (Rdf_ttl.to_string g)
  );
  (match ins with
     None -> ()
   | Some g -> 
       Printf.bprintf b "INSERT DATA { %s }\n"
           (Rdf_ttl.to_string g)
  );
  match Buffer.contents b with
    "" -> Lwt.return_unit
  | query ->
      let form_arg = `RawData (Js.string query) in
      Xhr.perform_raw_url
        ~content_type: "application/sparql-update"
        ~form_arg
        ~override_method: `PATCH
        ~with_credentials: true
        (Iri.to_uri iri) >>= fun xhr ->
          match xhr.Xhr.code with
          | 200 | 201 -> Lwt.return_unit
          | n -> error (Put_error (n, iri))

let delete iri =
  Xhr.perform_raw_url
    ~override_method: `DELETE
    ~with_credentials: true
    (Iri.to_uri iri) >>= fun xhr ->
    match xhr.Xhr.code with
    | 200 | 201 -> Lwt.return_unit
    | n -> error (Put_error (n, iri))

let login ?url () =
  let url =
    match url with
      None ->
        let w = Dom_html.window in
        let loc = w##location in
        let o = Js.to_string (Dom_html.location_origin_safe loc) in
        let p = Js.to_string (loc##pathname) in
        o ^ p
    | Some url -> Iri.to_uri url
  in
  dbg (Printf.sprintf "login, url=%s" url);
  head url >>= fun meta -> Lwt.return meta.user

(*
val perform_raw_url :
  ?headers:(string * string) list ->
  ?content_type:string ->
  ?post_args:(string * Form.form_elt) list ->
  ?get_args:(string * string) list ->
  ?form_arg:Form.form_contents ->
  ?check_headers:(int -> (string -> string option) -> bool) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  ?override_method:[< `DELETE
                    | `GET
                    | `HEAD
                    | `OPTIONS
                    | `PATCH
                    | `POST
                    | `PUT ] ->
  string -> http_frame Lwt.t
*)