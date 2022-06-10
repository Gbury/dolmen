
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Type definitions} *)
(** ************************************************************************ *)

type t
(** The type of environments for evaluations. *)

val mk :
  Model.t ->
  builtins:(t -> Dolmen.Std.Expr.Term.Const.t -> Value.t) -> t
(* The empty env. *)

val builtins : t -> Dolmen.Std.Expr.Term.Const.t -> Value.t
(** Return the builtins stored in the env. *)

val model : t -> Model.t

val update_model : t -> (Model.t -> Model.t) -> t


