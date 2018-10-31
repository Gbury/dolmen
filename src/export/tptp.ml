
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Pretty-printing terms in tptp syntax

    This modules defines a pretty-printer for terms in the tptp syntax. *)

let pretty_builtin = function
  | Term.Wildcard         -> Pretty.mk "$_" Pretty.Prefix
  | Term.Ttype            -> Pretty.mk "Type" Pretty.Prefix
  | Term.Prop             -> Pretty.mk "$o" Pretty.Prefix
  | Term.True             -> Pretty.mk "$true" Pretty.Prefix
  | Term.False            -> Pretty.mk "$false" Pretty.Prefix
  | Term.Eq               -> Pretty.mk "=" Pretty.Infix
  | Term.Distinct         -> Pretty.mk "$distinct" Pretty.Prefix

  | Term.Ite              -> Pretty.mk "$ite" Pretty.Prefix
  | Term.Sequent          -> Pretty.mk "-->" Pretty.Infix

  | Term.Int              -> Pretty.mk "$int" Pretty.Prefix
  | Term.Minus            -> Pretty.mk "$uminus" Pretty.Prefix
  | Term.Add              -> Pretty.mk "$sum" Pretty.Prefix
  | Term.Sub              -> Pretty.mk "$difference" Pretty.Prefix
  | Term.Mult             -> Pretty.mk "$product" Pretty.Prefix
  | Term.Lt               -> Pretty.mk "$less" Pretty.Prefix
  | Term.Leq              -> Pretty.mk "$lesseq" Pretty.Prefix
  | Term.Gt               -> Pretty.mk "$greater" Pretty.Prefix
  | Term.Geq              -> Pretty.mk "$greatereq" Pretty.Prefix

  | Term.Subtype          -> Pretty.mk "<<" Pretty.Infix
  | Term.Product          -> Pretty.mk "*" Pretty.Infix
  | Term.Union            -> Pretty.mk "+" Pretty.Infix

  | Term.Not              -> Pretty.mk "~" Pretty.Prefix
  | Term.And              -> Pretty.mk "&" Pretty.Infix ~assoc:Pretty.Left
  | Term.Or               -> Pretty.mk "|" Pretty.Infix ~assoc:Pretty.Left
  | Term.Nand             -> Pretty.mk "~&" Pretty.Infix
  | Term.Xor              -> Pretty.mk "<~>" Pretty.Infix
  | Term.Nor              -> Pretty.mk "~|" Pretty.Infix
  | Term.Imply            -> Pretty.mk "=>" Pretty.Infix ~assoc:Pretty.Right
  | Term.Implied          -> Pretty.mk "<=" Pretty.Infix
  | Term.Equiv            -> Pretty.mk "<=>" Pretty.Infix



