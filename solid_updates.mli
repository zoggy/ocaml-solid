
type updates

val create : Iri.t -> updates Lwt.t
val sub : updates -> Iri.t -> unit
val on_pub : updates -> (Iri.t -> unit) -> unit
