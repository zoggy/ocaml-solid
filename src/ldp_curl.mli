

module type P =
  sig
    val dbg : string -> unit Lwt.t
    val cert : string option
    val key : string option
  end

module Make : P -> Ldp_http.Requests

val make : ?cache:string -> ?cert:(string*string) ->
  dbg:(string -> unit Lwt.t) -> (module Ldp_http.Http) Lwt.t