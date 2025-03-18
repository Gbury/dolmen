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

val parse :
  string -> (t, [> `Msg of string ]) result
(** Parses an extension name. *)
