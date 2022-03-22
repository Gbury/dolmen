(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

val ops : Z.t Value.ops
(** ops for integer values. *)

val mk : Z.t -> Value.t
(** integer value creation. *)

val builtins : Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for integers *)

val truncate : Q.t -> Z.t
val floor : Q.t -> Z.t
val ceil : Q.t -> Z.t
