

module type P =
  sig
    val dbg : string -> unit Lwt.t
    val cert : string
    val key : string
  end

module Make : P -> Ldp_http.Requests
