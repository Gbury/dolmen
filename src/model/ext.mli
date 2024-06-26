

(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Define model extensions.

    @since 0.11 *)

type t
(** The type of evaluation extensions. *)

val name : t -> string
(** Extension name, should be suitable for cli options. *)

val builtins : t -> Env.builtins
(** Returns the evaluation builtins from an extension. *)

val create :
  name:string -> builtins:Env.builtins -> t
(** Create a new extension. *)

val bvconv : t
(** Builtin-in extension for conversion between bit-vectors and integers *)

val list : unit -> t list
(** The list of all extensions. *)

val find_exn : string -> t
(** Returns the last created extension with the given name. Raises [Not_found]
    if no such extension exists. *)