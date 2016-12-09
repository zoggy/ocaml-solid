module Ldp = Rdf_ldp
open Cohttp

type Ldp_types.error +=
  | Post_error of int * Iri.t
  | Get_error of int * Iri.t
  | Put_error of int * Iri.t
  | Patch_error of int * Iri.t
  | Delete_error of int * Iri.t

val map_opt : ('a -> 'b) -> 'a option -> 'b option
val do_opt : ('a -> unit) -> 'a option -> unit

val get_link : ('a * 'b) list -> 'a -> 'b option

val string_of_metadata : Ldp_types.meta -> string

val mime_turtle : string

module type Requests =
  sig
    val dbg : string -> unit Lwt.t
    val call: ?body: Cohttp_lwt_body.t -> ?chunked: bool ->
      ?headers: Header.t -> Code.meth -> Iri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t
  end

val response_metadata : Iri.t -> (Response.t * Cohttp_lwt_body.t) -> Ldp_types.meta

module type Cache =
  sig
    val get :
      (?headers: Header.t -> Iri.t ->
       (Response.t * Cohttp_lwt_body.t) Lwt.t) ->
        ?headers:Header.t -> Iri.t -> (Response.t * string) Lwt.t
  end

module type Cache_impl =
  sig
    type key
    val key : Header.t -> Iri.t -> key option
    val store : key -> string -> unit Lwt.t
    val find : key -> string option Lwt.t
  end

module Make_cache (I:Cache_impl) : Cache

module type Http =
  sig
    val dbg : string -> unit Lwt.t
    val head : Iri.t -> Ldp_types.meta Lwt.t
    val get_non_rdf : ?accept:string -> Iri.t -> (string * string) Lwt.t

    (** Retrieve a RDF resource.
      @return the resource contents and the optional graph. If
      the graph had to be parsed and an error occured,
      [Some (Error ..)] is returned.
      If a graph was given, it may have been modified before the error
      occured.
      @param g A graph to fill by parsing the resource contents.
      @param parse Indicate whether to parse
        the resource contents to fill a new graph or the given one.
        Default is [false] if no graph is given, else default is [true].
    *)
    val get_rdf : ?g:Rdf_graph.graph -> ?parse: bool -> Iri.t ->
      (string * (Rdf_graph.graph, Ldp_types.error) result option) Lwt.t

    val get_rdf_graph : ?g:Rdf_graph.graph -> Iri.t -> Rdf_graph.graph Lwt.t
    val get_container : ?g:Rdf_graph.graph -> Iri.t -> Rdf_graph.graph Lwt.t
    val is_container : Rdf_graph.graph -> bool

    (** @param parse default is [true]. Parse or not the retrieved
      resource if mime-type is text/turtle. *)
    val get : ?parse: bool -> Iri.t -> Ldp_types.resource Lwt.t

    val post :
      ?data:string ->
      ?mime:string ->
      ?slug:string -> typ:Iri.t -> Iri.t -> Ldp_types.meta Lwt.t

    val post_container : ?data: Rdf_graph.graph ->
      ?slug:string -> Iri.t -> Ldp_types.meta Lwt.t

    val post_direct_container : ?data: Rdf_graph.graph ->
      ?slug:string -> ?membershipResource: Iri.t ->
        relation: [< `HasMember of Iri.t | `IsMemberOf of Iri.t ] ->
          Iri.t -> Ldp_types.meta Lwt.t

    val post_indirect_container : ?data: Rdf_graph.graph ->
      ?slug:string -> ?membershipResource: Iri.t ->
        relation: [< `HasMember of Iri.t | `IsMemberOf of Iri.t ] ->
          insertedContent: Iri.t -> Iri.t -> Ldp_types.meta Lwt.t

    val post_rdf :
      ?data:Rdf_graph.graph ->
      ?slug:string -> ?typ: Iri.t -> Iri.t -> Ldp_types.meta Lwt.t

    (** Default [mime] is text/turtle.
        Default [typ] is nonRdfSource.*)
    val put : ?data:string -> ?mime:string -> ?typ: Iri.t -> Iri.t -> Ldp_types.meta Lwt.t
    val post_non_rdf :
      ?data:string -> ?mime:string -> Iri.t -> Ldp_types.meta Lwt.t
    val patch :
      ?del:Rdf_graph.graph -> ?ins:Rdf_graph.graph -> Iri.t -> unit Lwt.t
    val delete : Iri.t -> unit Lwt.t
    val login : Iri.t -> string option Lwt.t
  end

module Cached_http : Cache -> Requests -> Http
module Http : Requests -> Http