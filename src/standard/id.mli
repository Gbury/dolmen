
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard implementation of identifiers *)

(** {2 Type definitions} *)

type namespace = Namespace.t

type t = {
  name : Name.t;
  ns : Namespace.t;
}
(** The type of identifiers, basically a name together with a namespace. *)

(** {2 Implemented interfaces} *)

include Dolmen_intf.Id.Logic
  with type t := t
   and type namespace := namespace

(** {2 Usual comparison functions} *)

val hash    : t -> int
val equal   : t -> t -> bool
val compare : t -> t -> int
(** Usual functions for hashing, comparisons, equality. *)

val print : Format.formatter -> t -> unit
(** Printing functions. *)

module Map : Maps.S with type key := t
(** Maps for ids *)


(** {2 Additional functions} *)

val create : namespace -> Name.t -> t
(** Create an identifier. *)

val ns : t -> namespace
(** Accessor for the id's namespace. *)

val name : t -> Name.t
(** Accessor for the id's name. *)


(** {2 Standard attributes} *)

val stmt : t
(** Used to attach names to extension statements. *)

val ac_symbol : t
(** Used to denote associative-commutative symbols. *)

val predicate_def: t
(** Used to differentiate between functions and predicates in alt-ergo. *)

val case_split : t
(** Used to annote axioms/antecedants which are case split in alt-ergo. *)

val theory_decl : t
(** Used to define theories; used primarily in alt-ergo where it affects
    what engine is used to trigger axioms in the theory. *)

val rwrt_rule : t
(** The tagged term is (or at least should be) a rewrite rule. *)

val tptp_role : t
(** The tagged statement has a tptp role. Should be used as a function
    symbol applied to the actual tptp role. *)

val tptp_kind : t
(** The tagged statement is of the given kind (e.g. tff, thf, ...).
    Should be used as a function symbol applied to the actual tptp kind. *)

