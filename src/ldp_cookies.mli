
module Make : functor () ->
  sig
    val remove_cookies : unit -> unit
    val add_cookie : Iri.t -> 'a * Cohttp.Cookie.Set_cookie_hdr.t -> unit
    val cookies_by_iri : Iri.t -> Cohttp.Cookie.cookie list
  end