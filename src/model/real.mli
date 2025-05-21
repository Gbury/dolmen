
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


(** {2 Builtin values} *)
(** ************************************************************************ *)

val decimal : string -> Value.t option
val lt : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val gt : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val geq : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val leq : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val minus : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val add : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val sub : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val mul : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val div :
  cst:Dolmen.Std.Expr.Term.Const.t ->
  eval:(Env.t -> Dolmen.Std.Expr.Term.t -> Value.t) ->
  env:Env.t ->
  Value.t option
val div_e : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val div_t : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val div_f : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val mod_e : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val mod_t : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val mod_f : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val floor : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val ceiling : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val truncate : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val round : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val is_int : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val is_rat : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val floor_to_int : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val pow : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
