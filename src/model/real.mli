
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

(** {2 Corner cases & builtins} *)
(** ************************************************************************ *)

type conf

val conf :
  ?div_by_zero:(Env.t -> Value.t -> Value.t -> Value.t) ->
  unit -> conf

val builtins : conf:conf -> Env.t -> Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for reals *)

