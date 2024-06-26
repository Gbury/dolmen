

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
(** Built-in extension for conversion between bit-vectors and integers *)

val find_all : string -> t list
(** Returns the extensions that have been registered with the given name. *)