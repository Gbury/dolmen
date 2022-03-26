
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

type t = Z.t
(** Ints are represented using zarith's integer type. *)

val ops : Z.t Value.ops
(** ops for integer values. *)

val mk : Z.t -> Value.t
(** integer value creation. *)

val builtins : Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for integers *)

(** {2 Value helpers} *)
(** ************************************************************************ *)

val ceil : Q.t -> Z.t
val floor : Q.t -> Z.t
val truncate : Q.t -> Z.t

