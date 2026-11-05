
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definitions} *)
(** ************************************************************************ *)

val str_ops : Smt_string.Ustr.t Value.ops
(** String value ops. *)

val regl_ops : Smt_string.Reglan.t Value.ops
(** RegLan value ops. *)


(** {2 Builtins} *)
(** ************************************************************************ *)

val builtins : Env.builtins
(** Builtins for the SMT-LIB strings/regex theory. *)
