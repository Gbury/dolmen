
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

type t = Q.t
(* Rationals, using zarith. *)

val ops : t Value.ops
(** ops for real values. *)

val mk : t -> Value.t
(** real value creation. *)

val builtins : Env.builtins
(** builtins for reals *)

