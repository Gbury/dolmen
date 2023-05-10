
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

val ops : bool Value.ops
(** ops for boolean values. *)

val mk : bool -> Value.t
(** Boolean value creation. *)

val builtins : Env.builtins
(** builtins for booleans *)

