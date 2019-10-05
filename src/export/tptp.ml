
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Pretty-printing terms in tptp syntax

    This modules defines a pretty-printer for terms in the tptp syntax. *)

open Dolmen_std

(* Pretty printing information for builtins *)
(* ************************************************************************ *)

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

(* Variables and constants escaping *)
(* ************************************************************************ *)

let is_dollar c = Uchar.equal c (Uchar.of_char '$')
let is_underscore c = Uchar.equal c (Uchar.of_char '_')

let is_alpha c = Uucp.Alpha.is_alphabetic c
let is_num c = Uucp.Num.numeric_type c <> `None

(** Alphanumeric characters as defined by tptp (yes, it includes underscores, :p ) *)
let is_alphanum c = is_alpha c || is_num c || is_underscore c

let var_escape =
  let name id = Escape.Normal (Dolmen_std.Id.full_name id) in
  let rename = Escape.rename ~sep:'_' in
  let escape = Escape.umap (fun i -> function
      | None -> [ Uchar.of_char '_' ]
      | Some c ->
        begin match Uucp.Block.block c with
          | `ASCII when i = 1 && is_underscore c ->
            [Uchar.of_char 'V'; c]
          | `ASCII when i = 1 && Uucp.Case.is_lower c ->
            begin match Uucp.Case.Map.to_upper c with
              | `Self -> [ c ]
              | `Uchars l -> l
            end
          | `ASCII when (i = 1 && is_dollar c) || is_alphanum c ->
            [ c ]
          | _ -> [ Uchar.of_char 'V'; Uchar.of_char '_' ]
        end) in
  Escape.mk ~lang:"tptp" ~name ~escape ~rename

let cst_escape =
  let name id = Escape.Normal (Dolmen_std.Id.full_name id) in
  let rename = Escape.rename ~sep:'_' in
  let escape = Escape.umap (fun i -> function
      | None -> [ Uchar.of_char '_' ]
      | Some c ->
        begin match Uucp.Block.block c with
          | `ASCII when i = 1 && is_underscore c ->
            [Uchar.of_char 'V'; c]
          | `ASCII when i = 1 && Uucp.Case.is_upper c ->
            begin match Uucp.Case.Map.to_lower c with
              | `Self -> [ c ]
              | `Uchars l -> l @ [Uchar.of_char '_']
            end
          | `ASCII when (i = 1 && is_dollar c) || is_alphanum c ->
            [ c ]
          | _ -> [ Uchar.of_char '_' ]
        end) in
  Escape.mk ~lang:"tptp" ~name ~escape ~rename

(* Printing functions for terms *)
(* ************************************************************************ *)

let var fmt v = Escape.id var_escape fmt v
let cst fmt c = Escape.id cst_escape fmt c

let id fmt id = assert false (* match on the id namespace *)

let builtins fmt b = assert false

and binder fmt b = assert false

and colon fmt (u, v) = assert false

and app fmt (f, args) = assert false

and _match fmt (e, branches) = assert false

and term_descr fmt d = assert false

and term fmt t = assert false

(* Printing functions for formulas *)
(* ************************************************************************ *)


