(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

val ops : Z.t Value.ops
(** ops for bitvector values. *)

val mk : int -> Z.t -> Value.t
(** [mk n z] Bitvector of size [n] value creation. *)

val builtins : Dolmen.Std.Expr.Term.Const.t -> Value.t option
(** builtins for bitvectors *)
