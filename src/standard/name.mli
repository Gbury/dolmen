
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Names

    This is an abstraction of the names that can appear in parsed files.
    Names are basically a slightly more structured representation of the
    strings used to refer to symbols in input files. *)

(** {2 Type definition} *)

type t = private
  | Simple of string
  (** Regular symbols. *)
  | Indexed of {
      basename : string;
      indexes : string list;
    }
  (** Indexed symbols (currently only come from smtlib) *)
  | Qualified of {
      path : string list;
      basename : string;
    }
  (** Qualified names, including a module path before the basename. *)
(** The type of names. *)


(** {2 Std functions} *)

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
(** Std functions. *)

val print : Format.formatter -> t -> unit
(** Printing function. *)


(** {2 Std functions} *)

module Map : Maps.S with type key := t


(** {2 Creation functions} *)

val simple : string -> t
(** Create a simple/regular name. *)

val indexed : string -> string list -> t
(** Create an indexed name. *)

val qualified : string list -> string -> t
(** Create a qualified name. *)

