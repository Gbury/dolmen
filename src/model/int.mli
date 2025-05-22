
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

type t = Z.t
(** Ints are represented using zarith's integer type. *)

val ops : Z.t Value.ops
(** ops for integer values. *)

val mk : Z.t -> Value.t
(** integer value creation. *)


(** {2 Value helpers} *)
(** ************************************************************************ *)

val raw_ceil : Q.t -> Z.t
val raw_floor : Q.t -> Z.t
val raw_truncate : Q.t -> Z.t


(** {2 Builtin values} *)
(** ************************************************************************ *)

val integer : string -> Value.t option
val lt : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val gt : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val geq : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val leq : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val minus : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val add : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val sub : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val mul : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val pow : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val div_e :
  cst:Dolmen.Std.Expr.Term.Const.t ->
  eval:(Env.t -> Dolmen.Std.Expr.Term.t -> Value.t) ->
  env:Env.t ->
  Value.t option
val div_t : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val div_f : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val mod_e :
  cst:Dolmen.Std.Expr.Term.Const.t ->
  eval:(Env.t -> Dolmen.Std.Expr.Term.t -> Value.t) ->
  env:Env.t ->
  Value.t option
val mod_t : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val mod_f : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val divisible : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val abs : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val floor : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val ceiling : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val truncate : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val round : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val is_int : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
val is_rat : cst:Dolmen.Std.Expr.Term.Const.t -> Value.t option
