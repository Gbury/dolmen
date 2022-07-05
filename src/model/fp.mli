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

val builtins : Env.t -> Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for floating-points *)
