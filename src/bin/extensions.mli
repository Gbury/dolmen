type t

val pp : Format.formatter -> t -> unit

val name : t -> string
(** Returns the name of the extension. *)

val load_typing_extension :
  t -> Loop.State.t -> Loop.State.t
(** [load_typing_extension e] loads and activates the typing extension associated
    with [e], if any. *)

val load_model_extension :
  t -> Loop.State.t -> Loop.State.t
(** [load_model_extension e] loads and activates the model extension associated
    with [e], if any. *)

val list : unit -> t list
(** Lists the available extensions. *)

val list_conflicts : unit -> string list
(** Returns the name of external extensions that have the same name as an
    internal extension and cannot be loaded. *)

val parse :
  string -> (t, [> `Msg of string ]) result
(** Parses an extension name. *)
