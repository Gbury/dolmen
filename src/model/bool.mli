
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

val ops : bool Value.ops
(** ops for boolean values. *)

val builtins : Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for booleans *)

