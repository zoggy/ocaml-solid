(** *)

(** Reading and writing configuration files in RDF graphs. *)

(** To represent option names from root. *)

type path_elt = [`S of string | `I of Iri.t]
type path = path_elt list

(** {2 Errors} *)

type error =
| Invalid_value of Rdf_term.term (** Unexpected term *)
| Invalid_path of path  (** Invalid path used (empty list) *)
| Path_conflict of path (** When adding an option, an option path
                            cannot be the prefix of another one. *)
| Error_at_path of path * error
   (** Error while reading the option at the given path. *)
| Exn_at_path of path * exn
   (** Exception raised while reading the option at the given path *)

exception Error of error

val string_of_error : error -> string

(** {3 Convenient functions to raise [Error]} *)

val invalid_value : Rdf_term.term -> 'a
val invalid_path : path -> 'a
val path_conflict : path -> 'a
val error_at_path : path -> error -> 'a
val exn_at_path : path -> exn -> 'a

(** {2 Wrappers}

A wrapper is a pair of functions to read and write values of
some type from and to graph.
*)

module Wrapper : sig
    type 'a t = {
        to_term : ?with_doc: bool -> Rdf_graph.graph -> 'a -> Rdf_term.term ;
        from_term : ?def: 'a -> Rdf_graph.graph -> Rdf_term.term -> 'a ;
      }

    val make :
      (?with_doc: bool -> Rdf_graph.graph -> 'a -> Rdf_term.term) ->
        (?def: 'a -> Rdf_graph.graph -> Rdf_term.term -> 'a) -> 'a t

    val int : int t
    val float : float t
    val bool : bool t
    val string : string t
    val typed_string : Iri.t -> string t
    val string_ : ?typ:Iri.t -> ('a -> string) -> (string -> 'a) -> 'a t
    val list : ?typ: Iri.t -> 'a t -> 'a list t
    val option : 'a t -> 'a option t
    val pair : 'a t -> 'b t -> ('a * 'b) t
    val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  end

type 'a wrapper = 'a Wrapper.t

(** {2 Options and option groups} *)

(** An option with a value of type ['a]. When the option is found
  in a graph (see {!input}), the value is modified in place.
  Use {!get} to retrieve the option value. *)
type 'a conf_option

(** [option wrapper v] creates an option with initial value [v]
  and using the given [wrapper] to read and write from and to graph.
  @param doc an optional description for the option.
  @param cb an optional function to call each time the option is read.
*)
val option : ?doc: string -> ?cb: ('a -> unit) ->
  'a wrapper -> 'a -> 'a conf_option

(** [get option] returns the value of the given option. *)
val get : 'a conf_option -> 'a

(** [set option value] sets the value of the given option
  and calls the associated callback if any. *)
val set : 'a conf_option -> 'a -> unit

(** A group is used to group options and other groups.
  An [`Open] group is a group in which other options and groups can be
  added. Nothing can be added to a [`Closed] group. *)
type 'a group

(** Create a new empty open group. *)
val group : [`Open] group

(** [add group path option] adds the given [option] to
  [group] at [path]. *)
val add : [`Open] group -> path -> 'a conf_option -> [`Open] group

(** [add_group group path g] adds the group [g] to [group] at [path].*)
val add_group : [`Open] group -> path -> 'a group -> [`Open] group

(** {3:convenient Convenient functions to create options} *)

val int : ?doc: string -> ?cb: (int -> unit) -> int -> int conf_option
val float : ?doc: string -> ?cb: (float -> unit) -> float -> float conf_option
val bool : ?doc: string -> ?cb: (bool -> unit) -> bool -> bool conf_option
val string : ?doc: string -> ?cb: (string -> unit) -> string -> string conf_option
val list : ?doc: string -> ?cb: ('a list -> unit) -> 'a wrapper ->
  'a list -> 'a list conf_option
val option_ : ?doc: string -> ?cb: ('a option -> unit) ->
  'a wrapper -> 'a option -> 'a option conf_option
val pair : ?doc: string -> ?cb: ('a * 'b -> unit) ->
  'a wrapper -> 'b wrapper -> 'a * 'b -> ('a * 'b) conf_option
val triple : ?doc: string -> ?cb: ('a * 'b * 'c -> unit) ->
  'a wrapper -> 'b wrapper -> 'c wrapper ->
    'a * 'b * 'c -> ('a * 'b * 'c) conf_option

(** {2:input Reading options} *)

(** @param root start reading option values from this IRI. If not provided,
  use the graph name instead. *)
val from_graph : 'a group -> ?root:Rdf_term.term -> Rdf_graph.graph -> unit

(** {2:output Writing options}

The following function output the current state of the group, i.e.
the options it contains, with their current value.

The [with_doc] parameter indicates whether to output doc associated
to options. Default is [true].
*)

val to_graph : ?with_doc: bool -> 'a group -> Iri.t -> Rdf_graph.graph
