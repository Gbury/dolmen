
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Type definitions} *)
(** ************************************************************************ *)

type t
(** The type of environments for evaluations. *)

type builtins =
  eval:(t -> Dolmen.Std.Expr.Term.t -> Value.t) ->
  t -> Dolmen.Std.Expr.Term.Const.t -> Value.t option

val mk : Model.t -> builtins:builtins -> t
(* The empty env. *)

val builtins : t -> builtins
(** Return the builtins stored in the env. *)

val model : t -> Model.t

val update_model : t -> (Model.t -> Model.t) -> t

module Ext : sig
  type t
  (** The type of evaluation extensions. *)

  val name : t -> string
  (** Extension name, should be suitable for cli options. *)

  val builtins : t -> builtins
  (** Returns the evaluation builtins from an extension. *)

  val create :
    name:string -> builtins:builtins -> t
  (** Create a new extension. *)

  val list : unit -> t list
  (** The list of all extensions. *)
end
