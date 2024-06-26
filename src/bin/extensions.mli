type t

val pp : Format.formatter -> t -> unit

val name : t -> string
(** Returns the name of the extension. *)

val load_typing_extension :
  t -> Loop.State.t -> (Loop.State.t, [> `Msg of string]) result
(** [load_typing_extension e] loads and returns the typing extension associated
    with [e].

    Fails if an error occurs during loading of
    an external typing extension. *)

val load_model_extension :
  t -> Loop.State.t -> (Loop.State.t, [> `Msg of string]) result
(** [load_model_extension e] loads and returns the model extension associated
    with [e].

    Fails if [e] has no model extension, or an error occurs during loading of an
    external model extension. *)

val list : unit -> t list
(** Lists the available extensions. *)

val invalid : unit -> string list
(** Lists the name of dune plugins registered as extensions but with an invalid
    name. *)

type kind = Typing | Model

val parse :
  string -> (t * kind option, [> `Msg of string ]) result
(** Parses an extension name, with optional restriction prefix.

    Returns a pair [(ext, kind)] where [kind] is [None] if both model and typing
    extensions should be loaded, [Some Typing] if only the typing extension
    should be loaded, and [Some Model] if only the model extension should be
    loaded. *)