(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

type t = Z.t
(** Bitvectors, represented as unsigned, unbounded integers. *)

val ops : t Value.ops
(** ops for bitvector values. *)

val mk : int -> Z.t -> Value.t
(** [mk n z] Bitvector of size [n], and bits [z] creation. *)

val builtins : Env.builtins
(** builtins for bitvectors *)

val ubitv : int -> Value.t -> Z.t
(** Extract the value as an unsigned integer *)

val sbitv : int -> Value.t -> Z.t
(** Extract the value as a signed integer *)

val bvconv_builtins : Env.builtins
(** builtins for conversions between bitvectors and integers *)