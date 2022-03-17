(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

val ops : Z.t Value.ops
(** ops for Z.tean values. *)

val mk : Z.t -> Value.t
(** Z.Tean value creation. *)

val builtins : Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for Integers *)
