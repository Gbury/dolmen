(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

open Farith

val ops : F.t Value.ops
(** ops for bitvector values. *)

val mk : F.t -> Value.t
(** [mk f] floating point value creation. *)

(** {2 Corner cases & builtins} *)
(** ************************************************************************ *)

exception Unhandled_exponand_and_mantissa of { ew : int; mw : int; }
(** Raised when the exponand and mantissa siez do not respect the constraints
    imposed by `Farith`. *)

val builtins : Env.builtins
(** builtins for floating-points *)
