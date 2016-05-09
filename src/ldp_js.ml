

module Client = Make_client_async(struct
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
    let dbg s = Firebug.console##log (Js.string s)
  end)

module Nodbg = Make (struct let dbg s = Lwt.return_unit end)

let login_iri () =
  let w = Dom_html.window in
  let loc = w##.location in
  let o = Js.to_string (Dom_html.location_origin_safe loc) in
  let p = Js.to_string (loc##.pathname) in
  o ^ p
