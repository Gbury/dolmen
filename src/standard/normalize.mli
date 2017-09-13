
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Normalizing functions for supported languages *)

val tptp : Term.t -> Term.t
(** Normalizer for tptp terms *)

val smtlib : Term.t -> Term.t
(** Normalizer for smtlib terms *)

