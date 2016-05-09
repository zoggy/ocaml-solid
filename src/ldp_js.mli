
module type P =
  sig
    val dbg : string -> unit Lwt.t
  end

module Make : P -> Ldp_http.Requests

module Dbg : Ldp_http.Requests
module Nodbg : Ldp_http.Requests

val login_iri : unit -> Iri.t
