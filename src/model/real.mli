
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

type t = Q.t
(* Reals as rationals. Currently works because we have limited operations
   on reals, but we might need to upgrade to a more complete represetnation
   at one point. *)

val ops : t Value.ops
(** ops for real values. *)

val mk : t -> Value.t
(** real value creation. *)

val get : Value.t -> Q.t
(** Get a rational value. *)

val builtins : Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for reals *)

