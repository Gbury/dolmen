
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Exception} *)
(** ************************************************************************ *)

module V : Map.S with type key = Dolmen.Std.Expr.Term.Var.t
module C : Map.S with type key = Dolmen.Std.Expr.Term.Const.t

exception Partial_interpretation of
    Dolmen.Std.Expr.Term.Const.t * Value.t list

exception Incorrect_extension of
    Dolmen.Std.Expr.Term.Const.t * Value.t list * Value.t

(** {2 Type definitions} *)
(** ************************************************************************ *)

type t
(** The type of environments for evaluations. *)

val empty : t
(* The empty env. *)

val print : Format.formatter -> t -> unit
(** Print function *)

val vars : t -> Value.t V.t

val csts : t -> Value.t C.t

val disjoint_union : t -> t -> t
(** Disjoint union *)


(** {2 Variables and Constants values} *)
(** ************************************************************************ *)

module type S = sig

  type key

  val find_opt : key -> t -> Value.t option

  val add : key -> Value.t -> t -> t

  val remove : key -> t -> t

end

module Var : S with type key := Dolmen.Std.Expr.Term.Var.t
module Cst : S with type key := Dolmen.Std.Expr.Term.Const.t

