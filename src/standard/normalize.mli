
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Normalizing functions

    Functions in this module are meant to help normalize terms, that is, for a given
    language, map the semantic pre-defined builtins of that language to the builtins
    symbols defined by Dolmen Terms.

    For instance, the tptp language has a specific syntaxic construction
    for equality (that is, there is a reduction used to parse equality), thus
    equalities in tptp are parsed using the builtin equality symbol for terms.
    On the other hand, smtlib has no syntax rules for equality, which is seen
    as a regular application of the symbol name "=". Thus, equalities in smtlib files
    will be parsed as applications of a symbol named "=". Normalization maps
    that application to the builtin notion of equality defined for terms.

    WARNING: this normalization process is a best effort, but cannot be
    complete in general. Some constructions such as tptp's "$i" (pre-existing type
    for terms), have no clear builtin notion in other languages. For a general
    and complete translation, the ast should be typechecked.
*)

module Tptp : sig

  val mapper : Term.t Term.mapper
  (** A mapper suitable to normalize terms parsed from a tptp problem file. *)

end

module Smtlib : sig

  val mapper : Term.t Term.mapper
  (** A mapper suitable to normalize terms parsed from a tptp problem file. *)

end
