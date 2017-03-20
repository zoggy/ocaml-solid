

module type P =
  sig
    val dbg : string -> unit Lwt.t
    val authenticator : X509.Authenticator.a
    val certificates : Tls.Config.own_cert
  end

module Make : P -> Ldp_http.Requests

val make : ?cache:string -> ?cert:(string*string) ->
  dbg:(string -> unit Lwt.t) -> (module Ldp_http.Http) Lwt.t