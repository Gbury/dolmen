
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : Dolmen_intf.Location.S>
%parameter <I : Ast.Id>
%parameter <T : Ast.Term
  with type location := L.t and type id := I.t>
%parameter <S : Ast.Statement
  with type location := L.t  and type id := I.t and type term := T.t>

%start <S.t list> file
%start <S.t option> input

%{

  let pp_num_list fmt (l, singular, plural) =
    let n = List.length l in
    Format.fprintf fmt "%d %s" n (if n = 1 then singular else plural)

  let mismatched_lists ~loc l1 l2 =
    let msg = Format.dprintf
      "@[<v>@[<hov>Expected@ two@ lists@ of@ the@ same@ size,@ but@ got:@]@ \
        - @[<hov>%a@]@ - @[<hov>%a@]@]"
      pp_num_list l1 pp_num_list l2
    in
    raise (L.Syntax_error (loc, `Regular msg))

%}

%%

/* Hand-written following syntax.bnf */

/* Complete file, i.e Top-level declarations */

file:
  | l=tptp_input* EOF { l }

input:
  | i=tptp_input
    { Some i }
  | EOF
    { None }

tptp_input:
  | i=annotated_formula
  | i=include_
    { i }

/* ******************** */
/* Top-level statements */

annotated_formula:
  | f=thf_annotated
  | f=tff_annotated
  | f=tcf_annotated
  | f=fof_annotated
  | f=cnf_annotated
  | f=tpi_annotated
    { f }

tpi_annotated:
  | TPI LEFT_PAREN s=name COMMA role=formula_role COMMA
    f=tpi_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.tpi ~loc ?annot s ~role f }

thf_annotated:
  | THF LEFT_PAREN s=name COMMA role=formula_role COMMA
    f=thf_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.thf ~loc ?annot s ~role f }

tff_annotated:
  | TFF LEFT_PAREN s=name COMMA role=formula_role COMMA
    f=tff_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.tff ~loc ?annot s ~role f }

tcf_annotated:
  | TCF LEFT_PAREN s=name COMMA role=formula_role COMMA
    f=tcf_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.tcf ~loc ?annot s ~role f }

fof_annotated:
  | FOF LEFT_PAREN s=name COMMA role=formula_role COMMA
    f=fof_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.fof ~loc ?annot s ~role f }

cnf_annotated:
  | CNF LEFT_PAREN s=name COMMA role=formula_role COMMA
    f=cnf_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.cnf ~loc ?annot s ~role f }

annotations:
  | COMMA s=source i=optional_info
    { let loc = L.mk_pos $startpos $endpos in Some (S.annot ~loc s i) }
  | { None }

tpi_formula:
  | f=fof_formula { f }

formula_role:
  | s=LOWER_WORD
    { let loc = L.mk_pos $startpos $endpos in
      let id = I.mk I.attr s in
      T.const ~loc id }
  | s=LOWER_WORD DASH g=general_term
    { let t =
        let loc = L.mk_pos $startpos(s) $endpos(s) in
        let id = I.mk I.attr s in
        T.const ~loc id
      in
      let loc = L.mk_pos $startpos $endpos in
      T.colon ~loc t g
    }


/* ************ */
/* THF formulas */

thf_formula:
  | f=thf_logic_formula
  | f=thf_atom_typing
  | f=thf_subtype
    { f }

thf_logic_formula:
  | f=thf_unitary_formula
  | f=thf_unary_formula
  | f=thf_binary_formula
  | f=thf_defined_infix
  | f=thf_definition
  | f=thf_sequent
    { f }

thf_binary_formula:
  | f=thf_binary_nonassoc
  | f=thf_binary_assoc
  | f=thf_binary_type
    { f }

thf_binary_nonassoc:
  | f=thf_unit_formula c=nonassoc_connective g=thf_unit_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f; g] }

thf_binary_assoc:
  | f=thf_or_formula
  | f=thf_and_formula
  | f=thf_apply_formula
    { f }

thf_or_formula:
  | f=thf_unit_formula VLINE g=thf_unit_formula
  | f=thf_or_formula VLINE g=thf_unit_formula
    { let op = let loc = L.mk_pos $startpos($2) $endpos($2) in T.or_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc op [f; g] }

thf_and_formula:
  | f=thf_unit_formula AND g=thf_unit_formula
  | f=thf_and_formula AND g=thf_unit_formula
    { let op = let loc = L.mk_pos $startpos($2) $endpos($2) in T.and_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc op [f; g] }

thf_apply_formula:
  | f=thf_unit_formula APPLY g=thf_unit_formula
  | f=thf_apply_formula APPLY g=thf_unit_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f [g] }

thf_unit_formula:
  /* we use an explicit precedence here to solve a shift/reduce conflict, see tokens.mly */
  | f=thf_unitary_formula %prec quantifier_prec
  | f=thf_unary_formula
  | f=thf_defined_infix
    { f }

thf_preunit_formula:
  | f=thf_unitary_formula
  | f=thf_prefix_unary
    { f }

thf_unitary_formula:
  | f=variable
  | f=thf_quantified_formula
  | f=thf_atomic_formula
  | LEFT_PAREN f=thf_logic_formula RIGHT_PAREN
    { f }

thf_quantified_formula:
  | quant=thf_quantification f=thf_unit_formula
    { let loc = Some (L.mk_pos $startpos $endpos) in
      let q, l = quant in
      q ?loc l f }

thf_quantification:
  | q=thf_quantifier LEFT_BRACKET l=thf_variable_list RIGHT_BRACKET COLON
    { q, l }

thf_variable_list:
  | v=thf_typed_variable
   { [ v ] }
  | v=thf_typed_variable COMMA l=thf_variable_list
   { v :: l }

thf_typed_variable:
  | c=variable COLON ty=thf_top_level_type
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc c ty }

thf_unary_formula:
  | f=thf_prefix_unary
  | f=thf_infix_unary
    { f }

thf_prefix_unary:
  | c=thf_unary_connective f=thf_preunit_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f] }

thf_infix_unary:
  | LEFT_PAREN f=thf_unitary_term s=infix_inequality g=thf_unitary_term RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc s [f; g] }

thf_atomic_formula:
  | f=thf_plain_atomic
  | f=thf_defined_atomic
  | f=thf_system_atomic
  | f=thf_fof_function
    { f }

thf_plain_atomic:
  | f=constant
  | f=thf_tuple
    { f }

thf_defined_atomic:
  | f=defined_constant
  | f=thf_defined_term
  | f=nhf_long_connective
  | f=thf_let
  | LEFT_PAREN f=thf_conn_term RIGHT_PAREN
    { f }

thf_defined_term:
  | f=defined_term
  | f=th1_defined_term
    { f }

thf_defined_infix:
  | f=thf_unitary_formula c=defined_infix_pred g=thf_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f; g] }

thf_system_atomic:
  | f=system_constant
    { f }

thf_let:
  | LET LEFT_PAREN tys=thf_let_types COMMA defs=thf_let_defns COMMA f=thf_logic_formula RIGHT_PAREN
    { let bound =
        let loc = L.mk_pos $startpos(tys) $endpos(defs) in
        try List.map2 (fun var ty -> T.colon ~loc var ty) defs tys
        with Invalid_argument _ ->
          mismatched_lists ~loc
            (defs, "bound variable", "bound variables")
            (tys, "type", "types")
      in
      let loc = L.mk_pos $startpos $endpos in
      T.letin ~loc bound f
    }

thf_let_types:
  | t=thf_atom_typing
    { [t] }
  | LEFT_BRACKET l=thf_atom_typing_list RIGHT_BRACKET
    { l }

thf_atom_typing_list:
  | t=thf_atom_typing
    { [t] }
  | t=thf_atom_typing COMMA l=thf_atom_typing_list
    { t :: l }

thf_let_defns:
  | t=thf_let_defn
    { [t] }
  | LEFT_BRACKET l=thf_let_defn_list RIGHT_BRACKET
    { l }

thf_let_defn:
  | x=thf_logic_formula a=assignment t=thf_logic_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc a [x; t] }

thf_let_defn_list:
  | t=thf_let_defn
    { [t] }
  | t=thf_let_defn COMMA l=thf_let_defn_list
    { t :: l }

thf_unitary_term:
  | f=thf_atomic_formula
  | f=variable
  | LEFT_PAREN f=thf_logic_formula RIGHT_PAREN
    { f }

thf_conn_term:
  | c=nonassoc_connective
  | c=assoc_connective
  | c=infix_equality
  | c=infix_inequality
  | c=thf_unary_connective
    { c }

thf_tuple:
  | LEFT_BRACKET RIGHT_BRACKET
    { let loc = L.mk_pos $startpos $endpos in T.tuple ~loc [] }
  | LEFT_BRACKET l=thf_formula_list RIGHT_BRACKET
    { let loc = L.mk_pos $startpos $endpos in T.tuple ~loc l }

thf_fof_function:
  | f=_functor LEFT_PAREN l=thf_arguments RIGHT_PAREN
  | f=defined_functor LEFT_PAREN l=thf_arguments RIGHT_PAREN
  | f=system_functor LEFT_PAREN l=thf_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

thf_arguments:
  | l=thf_formula_list
    { l }

thf_formula_list:
  | f=thf_logic_formula
    { [f] }
  | f=thf_logic_formula COMMA l=thf_formula_list
    { f :: l }

thf_atom_typing:
  | t=untyped_atom COLON ty=thf_top_level_type
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc t ty }
  | LEFT_PAREN t=thf_atom_typing RIGHT_PAREN
    { t }

thf_top_level_type:
  | t=thf_unitary_type
  | t=thf_mapping_type
  | t=thf_apply_type
    { t }

thf_unitary_type:
  | f=thf_unitary_formula
    { f }

thf_apply_type:
  | t=thf_apply_formula
    { t }

thf_binary_type:
  | t=thf_mapping_type
  | t=thf_xprod_type
  | t=thf_union_type
    { t }

thf_mapping_type:
  | a=thf_unitary_type ARROW b=thf_unitary_type
  | a=thf_unitary_type ARROW b=thf_mapping_type
    { let loc = L.mk_pos $startpos $endpos in T.arrow ~loc a b }

thf_xprod_type:
  | a=thf_unitary_type STAR b=thf_unitary_type
  | a=thf_xprod_type STAR b=thf_unitary_type
    { let loc = L.mk_pos $startpos $endpos in T.product ~loc a b }

thf_union_type:
  | a=thf_unitary_type PLUS b=thf_unitary_type
  | a=thf_union_type PLUS b=thf_unitary_type
    { let loc = L.mk_pos $startpos $endpos in T.union ~loc a b }

thf_subtype:
  | a=untyped_atom subtype_sign b=atom
    { let loc = L.mk_pos $startpos $endpos in T.subtype ~loc a b }

thf_definition:
  | x=thf_atomic_formula eq=identical e=thf_logic_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc eq [x; e] }

thf_sequent:
  | hyp=thf_tuple gentzen_arrow goal=thf_tuple
    { let loc = L.mk_pos $startpos $endpos in T.sequent ~loc [hyp] [goal] }


/* *********** */
/* TFF formula */

tff_formula:
  | f=tff_logic_formula
  | f=tff_atom_typing
  | f=tff_subtype
    { f }

tff_logic_formula:
  | f=tff_unitary_formula
  | f=tff_unary_formula
  | f=tff_binary_formula
  | f=tff_defined_infix
  | f=txf_definition
  | f=txf_sequent
    { f }

tff_binary_formula:
  | f=tff_binary_nonassoc
  | f=tff_binary_assoc
    { f }

tff_binary_nonassoc:
  | f=tff_unit_formula c=nonassoc_connective g=tff_unit_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f; g] }

tff_binary_assoc:
  | f=tff_or_formula
  | f=tff_and_formula
    { f }

tff_or_formula:
  | f=tff_unit_formula VLINE g=tff_unit_formula
  | f=tff_or_formula VLINE g=tff_unit_formula
    { let op = let loc = L.mk_pos $startpos($2) $endpos($2) in T.or_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc op [f; g] }

tff_and_formula:
  | f=tff_unit_formula AND g=tff_unit_formula
  | f=tff_and_formula AND g=tff_unit_formula
    { let op = let loc = L.mk_pos $startpos($2) $endpos($2) in T.and_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc op [f; g] }

tff_unit_formula:
  | f=tff_unitary_formula
  | f=tff_unary_formula
  | f=tff_defined_infix
    { f }

tff_preunit_formula:
  | f=tff_unitary_formula
  | f=tff_prefix_unary
    { f }

tff_unitary_formula:
  | f=tff_quantified_formula
  | f=tff_atomic_formula
  | f=txf_unitary_formula
  | LEFT_PAREN f=tff_logic_formula RIGHT_PAREN
    { f }

txf_unitary_formula:
  | v=variable
    { v }

tff_quantified_formula:
  | q=fof_quantifier LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON f=tff_unit_formula
    { let loc = Some (L.mk_pos $startpos $endpos) in q ?loc l f }

tff_variable_list:
  | v=tff_variable
    { [ v ] }
  | v=tff_variable COMMA l=tff_variable_list
    { v :: l }

tff_variable:
  | v=tff_typed_variable
  | v=variable
    { v }

tff_typed_variable:
  | v=variable COLON ty=tff_atomic_type
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc v ty }

tff_unary_formula:
  | f=tff_prefix_unary
  | f=tff_infix_unary
    { f }

tff_prefix_unary:
  | u=tff_unary_connective f=tff_preunit_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc u [f] }

tff_infix_unary:
  | LEFT_PAREN f=tff_unitary_term e=infix_inequality g=tff_unitary_term RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc e [f; g] }

tff_atomic_formula:
  | f=tff_plain_atomic
  | f=tff_defined_atomic
  | f=tff_system_atomic
    { f }

tff_plain_atomic:
  | c=constant
    { c }
  | f=_functor LEFT_PAREN l=tff_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

tff_defined_atomic:
  | t=tff_defined_plain
    { t }

tff_defined_plain:
  | c=defined_constant
    { c }
  | f=defined_functor LEFT_PAREN l=tff_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }
  | f=nxf_atom
  | f=txf_let
    { f }

tff_defined_infix:
  | LEFT_PAREN f=tff_unitary_term p=defined_infix_pred g=tff_unitary_term RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc p [f; g] }

tff_system_atomic:
  | c=system_constant
    { c }
  | f=system_functor LEFT_PAREN l=tff_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

txf_let:
  | LET LEFT_PAREN tys=txf_let_types COMMA defs=txf_let_defns COMMA body=tff_term RIGHT_PAREN
    { let bound =
        let loc = L.mk_pos $startpos(tys) $endpos(defs) in
        try List.map2 (fun var ty -> T.colon ~loc var ty) defs tys
        with Invalid_argument _ ->
          mismatched_lists ~loc
            (defs, "bound variable", "bound variables")
            (tys, "type", "types")
      in
      let loc = L.mk_pos $startpos $endpos in
      T.letin ~loc bound body
    }

txf_let_types:
  | t=tff_atom_typing
    { [t] }
  | RIGHT_BRACKET l=tff_atom_typing_list RIGHT_PAREN
    { l }

tff_atom_typing_list:
  | t=tff_atom_typing
    { [t] }
  | t=tff_atom_typing COMMA l=tff_atom_typing_list
    { t :: l }

txf_let_defns:
  | t=txf_let_defn
    { [t] }
  | LEFT_BRACKET l=txf_let_defn_list RIGHT_BRACKET
    { l }

txf_let_defn:
  | x=txf_let_LHS eq=assignment e=tff_term
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc eq [x; e] }

txf_let_LHS:
  | t=tff_plain_atomic
  | t=txf_tuple
    { t }

txf_let_defn_list:
  | t=txf_let_defn
    { [t] }
  | t=txf_let_defn COMMA l=txf_let_defn_list
    { t :: l }

nxf_atom:
  | c=nxf_long_connective APPLY LEFT_PAREN l=tff_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c l }

tff_term:
  | f=tff_logic_formula
  | f=defined_term
  | f=txf_tuple
    { f }

tff_unitary_term:
  | f=tff_atomic_formula
  | f=defined_term
  | f=txf_tuple
  | f=variable
  | LEFT_PAREN f=tff_logic_formula RIGHT_PAREN
    { f }

txf_tuple:
  | LEFT_BRACKET RIGHT_BRACKET
    { let loc = L.mk_pos $startpos $endpos in T.tuple ~loc [] }
  | LEFT_BRACKET l=tff_arguments RIGHT_BRACKET
    { let loc = L.mk_pos $startpos $endpos in T.tuple ~loc l }

tff_arguments:
  | t=tff_term
    { [t] }
  | t=tff_term COMMA l=tff_arguments
    { t :: l }

tff_atom_typing:
  | t=untyped_atom COLON ty=tff_top_level_type
   { let loc = L.mk_pos $startpos $endpos in T.colon ~loc t ty }

tff_top_level_type:
  | t=tff_atomic_type
  | t=tff_non_atomic_type
    { t }

tff_non_atomic_type:
  | t=tff_mapping_type
  | t=tf1_quantified_type
  | LEFT_PAREN t=tff_non_atomic_type RIGHT_PAREN
    { t }

tf1_quantified_type:
  | FORALL_TY LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON t=tff_monotype
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc l t }

tff_monotype:
  | t=tff_atomic_type
  | LEFT_PAREN t=tff_mapping_type RIGHT_PAREN
  | t=tf1_quantified_type
    { t }

tff_unitary_type:
  | t=tff_atomic_type
  | LEFT_PAREN t=tff_xprod_type RIGHT_PAREN
    { t }

tff_atomic_type:
  | t=type_constant
  | t=defined_type
  | t=variable
  | LEFT_PAREN t=tff_atomic_type RIGHT_PAREN
  | t=txf_tuple_type
    { t }
  | f=type_functor LEFT_PAREN l=tff_type_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

tff_type_arguments:
  | t=tff_atomic_type
    { [t] }
  | t=tff_atomic_type COMMA l=tff_type_arguments
    { t :: l }

tff_mapping_type:
  | a=tff_unitary_type ARROW b=tff_atomic_type
    { let loc = L.mk_pos $startpos $endpos in T.arrow ~loc a b }

tff_xprod_type:
  | a=tff_unitary_type STAR b=tff_atomic_type
  | a=tff_xprod_type STAR b=tff_atomic_type
    { let loc = L.mk_pos $startpos $endpos in T.product ~loc a b }

txf_tuple_type:
  | LEFT_BRACKET t=tff_type_list RIGHT_BRACKET
    { t }

tff_type_list:
  | t=tff_top_level_type
    { t }
  | t=tff_top_level_type COMMA l=tff_type_list
    { let loc = L.mk_pos $startpos $endpos in T.product ~loc t l }

tff_subtype:
  | a=untyped_atom subtype_sign b=atom
   { let loc = L.mk_pos $startpos $endpos in T.subtype ~loc a b }

txf_definition:
  | a=tff_atomic_formula eq=identical b=tff_term
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc eq [a; b] }

txf_sequent:
  | hyp=txf_tuple GENTZEN_ARROW goal=txf_tuple
    { let loc = L.mk_pos $startpos $endpos in T.sequent ~loc [hyp] [goal] }

/* ************ */
/* NHF formulas */

nhf_long_connective:
  | LEFT_CURLY c=ntf_connective_name RIGHT_CURLY
    { c }
  | LEFT_CURLY c=ntf_connective_name LEFT_PAREN l=nhf_parameter_list RIGHT_PAREN RIGHT_CURLY
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c l }

nhf_parameter_list:
  | t=nhf_parameter
    { [t] }
  | t=nhf_parameter COMMA l=nhf_parameter_list
    { t :: l }

nhf_parameter:
  | t=ntf_index
  | t=nhf_key_pair
    { t }

nhf_key_pair:
  | t=thf_definition
    { t }

nxf_long_connective:
  | LEFT_CURLY c=ntf_connective_name RIGHT_CURLY
    { c }
  | LEFT_CURLY c=ntf_connective_name LEFT_PAREN l=nxf_parameter_list RIGHT_PAREN RIGHT_CURLY
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c l }

nxf_parameter_list:
  | t=nxf_parameter
    { [t] }
  | t=nxf_parameter COMMA l=nxf_parameter_list
    { t :: l }

nxf_parameter:
  | t=ntf_index
  | t=nxf_key_pair
    { t }

nxf_key_pair:
  | t=txf_definition
    { t }

ntf_connective_name:
  | c=def_or_sys_constant
    { c }

ntf_index:
  | HASH t=tff_unitary_term
    { t }

ntf_short_connective:
  /* TODO: make these ast builtins ? requires to find names for these */
  | LEFT_BRACKET DOT RIGHT_BRACKET
    { let c = I.mk I.term "[.]" in
      let loc = L.mk_pos $startpos $endpos in T.const ~loc c }
  | LESS DOT ARROW
    { let c = I.mk I.term "<.>" in
      let loc = L.mk_pos $startpos $endpos in T.const ~loc c }
  | LEFT_CURLY DOT RIGHT_CURLY
    { let c = I.mk I.term "{.}" in
      let loc = L.mk_pos $startpos $endpos in T.const ~loc c }
  | LEFT_PAREN DOT RIGHT_PAREN
    { let c = I.mk I.term "(.)" in
      let loc = L.mk_pos $startpos $endpos in T.const ~loc c }


/* ************ */
/* TCF formulas */

tcf_formula:
  | t=tcf_logic_formula
  | t=tff_atom_typing
    { t }

tcf_logic_formula:
  | t=tcf_quantified_formula
  | t=cnf_formula
    { t }

tcf_quantified_formula:
  | FORALL LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON t=tcf_logic_formula
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc l t }


/* ************ */
/* FOF formulas */

fof_formula:
  | f=fof_logic_formula
  | f=fof_sequent
    { f }

fof_logic_formula:
  | f=fof_binary_formula
  | f=fof_unary_formula
  | f=fof_unitary_formula
    { f }

fof_binary_formula:
  | f=fof_binary_nonassoc
  | f=fof_binary_assoc
    { f }

fof_binary_nonassoc:
  | f=fof_unit_formula c=nonassoc_connective g=fof_unit_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f; g] }

fof_binary_assoc:
  | f=fof_or_formula
  | f=fof_and_formula
    { f }

fof_or_formula:
  | f=fof_unit_formula VLINE g=fof_unit_formula
  | f=fof_or_formula VLINE g=fof_unit_formula
    { let op = let loc = L.mk_pos $startpos($2) $endpos($2) in T.or_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc op [f; g] }

fof_and_formula:
  | f=fof_unit_formula AND g=fof_unit_formula
  | f=fof_and_formula AND g=fof_unit_formula
    { let op = let loc = L.mk_pos $startpos($2) $endpos($2) in T.and_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc op [f; g] }

fof_unary_formula:
  | c=unary_connective f=fof_unit_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f] }
  | t=fof_infix_unary
    { t }

fof_infix_unary:
  | a=fof_term f=infix_inequality b=fof_term
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f [a; b] }

fof_unit_formula:
  | t=fof_unitary_formula
  | t=fof_unary_formula
    { t }

fof_unitary_formula:
  | f=fof_quantified_formula
  | f=fof_atomic_formula
  | LEFT_PAREN f=fof_logic_formula RIGHT_PAREN
    { f }

fof_quantified_formula:
  | q=fof_quantifier LEFT_BRACKET l=fof_variable_list RIGHT_BRACKET COLON f=fof_unit_formula
    { let loc = Some (L.mk_pos $startpos $endpos) in q ?loc l f }

fof_variable_list:
  | v=variable
    { [ v ] }
  | v=variable COMMA l=fof_variable_list
    { v :: l }

fof_atomic_formula:
  | t=fof_plain_atomic_formula
  | t=fof_defined_atomic_formula
  | t=fof_system_atomic_formula
    { t }

fof_plain_atomic_formula:
  | t=fof_plain_term
    { t }

fof_defined_atomic_formula:
  | t=fof_defined_plain_formula
  | t=fof_defined_infix_formula
    { t }

fof_defined_plain_formula:
  | t=fof_defined_plain_term
    { t }

fof_defined_infix_formula:
  | a=fof_term f=defined_infix_pred b=fof_term
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f [a; b] }

fof_system_atomic_formula:
  | t=fof_system_term
    { t }

fof_plain_term:
  | c=constant
    { c }
  | f=_functor LEFT_PAREN l=fof_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

fof_defined_term:
  | t=defined_term
  | t=fof_defined_atomic_term
    { t }

fof_defined_atomic_term:
  | t=fof_defined_plain_term
    { t }

fof_defined_plain_term:
  | c=defined_constant
    { c }
  | f=defined_functor LEFT_PAREN l=fof_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

fof_system_term:
  | c=system_constant
    { c }
  | f=system_functor LEFT_PAREN l=fof_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

fof_arguments:
  | t=fof_term
    { [t] }
  | t=fof_term COMMA l=fof_arguments
    { t :: l }

fof_term:
  | t=fof_function_term
    { t }
  | v=variable
    { v }

fof_function_term:
  | t=fof_plain_term
  | t=fof_defined_term
  | t=fof_system_term
    { t }

fof_sequent:
  | hyp=fof_formula_tuple GENTZEN_ARROW goal=fof_formula_tuple
    { let loc = L.mk_pos $startpos $endpos in T.sequent ~loc hyp goal }
  | LEFT_PAREN t=fof_sequent RIGHT_PAREN
    { t }

fof_formula_tuple:
  | LEFT_CURLY RIGHT_CURLY
    { [] }
  | LEFT_CURLY l=fof_formula_tuple_list RIGHT_CURLY
    { l }

fof_formula_tuple_list:
  | f=fof_logic_formula
    { [ f ] }
  | f=fof_logic_formula COMMA l=fof_formula_tuple_list
    { f :: l }


/* ************ */
/* CNF Formulas */

cnf_formula:
  | f=cnf_disjunction
  | LEFT_PAREN f=cnf_formula RIGHT_PAREN
    { f }

cnf_disjunction:
  | x=cnf_literal
    { x }
  | f=cnf_disjunction VLINE x=cnf_literal
    { let op = let loc = L.mk_pos $startpos($2) $endpos($2) in T.or_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc op [f; x] }

cnf_literal:
  | f=fof_atomic_formula
    { f }
  | NOT f=fof_atomic_formula
  | NOT LEFT_PAREN f=fof_atomic_formula RIGHT_PAREN
    { let c = let loc = L.mk_pos $startpos($1) $endpos($1) in T.not_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f] }
  | f=fof_infix_unary
    { f }


/* *********** */
/* Connectives */

%inline thf_quantifier:
  | q=fof_quantifier
  | q=th0_quantifier
  | q=th1_quantifier
    { q }

%inline thf_unary_connective:
  | c=unary_connective
  | c=ntf_short_connective
    { c }

%inline th1_quantifier:
  | FORALL_TY
    { T.forall }
  | EXISTS_TY
    { T.exists }

%inline th0_quantifier:
  | LAMBDA
    { T.lambda }
  | INDEFINITE_DESCRIPTION_OP
    { T.choice }
  | DEFINITE_DESCRIPTION_OP
    { T.description }

%inline subtype_sign:
  | SUBTYPE
    { () }

%inline tff_unary_connective:
  | c=unary_connective
  | c=ntf_short_connective
    { c }

%inline fof_quantifier:
  | EXISTS
    { T.exists }
  | FORALL
    { T.forall }

%inline nonassoc_connective:
  | EQUIV
    { let loc = L.mk_pos $startpos $endpos in T.equiv_t ~loc () }
  | IMPLY
    { let loc = L.mk_pos $startpos $endpos in T.implies_t ~loc () }
  | LEFT_IMPLY
    { let loc = L.mk_pos $startpos $endpos in T.implied_t ~loc () }
  | XOR
    { let loc = L.mk_pos $startpos $endpos in T.xor_t ~loc () }
  | NOTVLINE
    { let loc = L.mk_pos $startpos $endpos in T.nor_t ~loc () }
  | NOTAND
    { let loc = L.mk_pos $startpos $endpos in T.nand_t ~loc () }

%inline assoc_connective:
  | VLINE
    { let loc = L.mk_pos $startpos $endpos in T.or_t ~loc () }
  | AND
    { let loc = L.mk_pos $startpos $endpos in T.and_t ~loc () }

%inline unary_connective:
  | NOT
    { let loc = L.mk_pos $startpos $endpos in T.not_t ~loc () }

%inline gentzen_arrow:
  | GENTZEN_ARROW
    { () }

%inline assignment:
  | ASSIGNMENT
    { let loc = L.mk_pos $startpos $endpos in T.eq_t ~loc () }

%inline identical:
  | IDENTICAL
    { let loc = L.mk_pos $startpos $endpos in T.neq_t ~loc () }


/* ********************* */
/* Types for THF and TFF */

%inline type_constant:
  | t=type_functor
    { t }

%inline type_functor:
  | a=atomic_word
    { a }

%inline defined_type:
  | a=atomic_defined_word
    { a }


/* ****************** */
/* Common definitions */

%inline atom:
  | c=untyped_atom
  | c=defined_constant
    { c }

%inline untyped_atom:
  | c=constant
  | c=system_constant
    { c }

%inline defined_infix_pred:
  | f=infix_equality
    { f }

%inline infix_equality:
  | EQUAL
    { let loc = L.mk_pos $startpos $endpos in T.eq_t ~loc () }

%inline infix_inequality:
  | NOT_EQUAL
    { let loc = L.mk_pos $startpos $endpos in T.neq_t ~loc () }

%inline constant:
  | f=_functor
    { f }

%inline _functor:
  | a=atomic_word
    { a }

%inline defined_constant:
  | f=defined_functor
    { f }

%inline defined_functor:
  | a=atomic_defined_word
    { a }

%inline system_constant:
  | f=system_functor
    { f }

%inline system_functor:
  | a=atomic_system_word
    { a }

%inline def_or_sys_constant:
  | c=defined_constant
  | c=system_constant
    { c }

%inline th1_defined_term:
  | PI
    { let loc = L.mk_pos $startpos $endpos in T.pi_t ~loc () }
  | SIGMA
    { let loc = L.mk_pos $startpos $endpos in T.sigma_t ~loc () }
  | DEFINITE_DESCRIPTION_TERM
    { let loc = L.mk_pos $startpos $endpos in T.description_t ~loc () }
  | INDEFINITE_DESCRIPTION_TERM
    { let loc = L.mk_pos $startpos $endpos in T.choice_t ~loc () }
  | EQUAL_TERM
    { let loc = L.mk_pos $startpos $endpos in T.eq_t ~loc () }

%inline defined_term:
  | t=number
  | t=distinct_object
    { t }

%inline variable:
  | v=UPPER_WORD
    { let loc = L.mk_pos $startpos $endpos in T.var ~loc (I.mk I.term v) }


/* *************** */
/* Formula sources */

%inline source:
  | t=general_term
    { t }

%inline optional_info:
  | COMMA i=useful_info
    { i }
  | { [] }

%inline useful_info:
  | l=general_list
    { l }


/* ****************** */
/* Include directives */

include_:
  | INCLUDE LEFT_PAREN f=file_name l=include_optionals RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.include_ ~loc f l }

include_optionals:
  | COMMA l=formula_selection
    { l }
  /* TODO: figure out the semantics of `space_name` */
  | COMMA l=formula_selection COMMA space_name
    { l }
  | { [] }

formula_selection:
  | LEFT_BRACKET l=name_list RIGHT_BRACKET
    { l }
  | STAR
    { [] }

name_list:
  | n=name
    { [n] }
  | n=name COMMA l=name_list
    { n :: l }

%inline space_name:
  | n=name
    { n }


/* **************** */
/* Non-logical data */

general_term:
  | d=general_data
    { d }
  | d=general_data COLON t=general_term
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc d t }
  | l=general_list
    { let f = let loc = L.mk_pos $startpos $endpos in T.data_t ~loc () in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

general_data:
  | d=atomic_word
  | d=general_function
  | d=variable
  | d=number
  | d=distinct_object
  | d=formula_data
    { d }

general_function:
  | f=atomic_word LEFT_PAREN l=general_terms RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

formula_data:
  | DOLLAR_THF LEFT_PAREN f=thf_formula RIGHT_PAREN
  | DOLLAR_TFF LEFT_PAREN f=tff_formula RIGHT_PAREN
  | DOLLAR_FOF LEFT_PAREN f=fof_formula RIGHT_PAREN
  | DOLLAR_CNF LEFT_PAREN f=cnf_formula RIGHT_PAREN
  | DOLLAR_FOT LEFT_PAREN f=fof_term    RIGHT_PAREN
    { f }

general_list:
  | LEFT_BRACKET RIGHT_BRACKET
    { [] }
  | LEFT_BRACKET l=general_terms RIGHT_BRACKET
    { l }

general_terms:
  | t=general_term
    { [ t ] }
  | t=general_term COMMA l=general_terms
    { t :: l }


/* General purposes */

/*
  name:
    | n=atomic_word
    | n=integer

  This production has been expanded to
  produce ids instead of terms.
*/
name:
  | s=LOWER_WORD
  | s=SINGLE_QUOTED
  | s=INTEGER
    { I.mk I.decl s }

%inline atomic_word:
  | s=LOWER_WORD
  | s=SINGLE_QUOTED
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s) }

%inline atomic_defined_word:
  | s=DOLLAR_WORD
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s) }

%inline atomic_system_word:
  | s=DOLLAR_DOLLAR_WORD
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s) }

%inline number:
  | n=integer
  | n=rational
  | n=real
    { n }

file_name:
  | s=SINGLE_QUOTED
    { let n = String.length s in String.sub s 1 (n - 2) }


/* *************************************** */
/* Wrapper around some lexical definitions */

%inline distinct_object:
  | s=DISTINCT_OBJECT
    { let loc = L.mk_pos $startpos $endpos in
      T.distinct ~loc (I.mk I.term s) }

%inline integer:
  | n=INTEGER
    { let loc = L.mk_pos $startpos $endpos in T.int ~loc n }

%inline rational:
  | n=RATIONAL
    { let loc = L.mk_pos $startpos $endpos in T.rat ~loc n }

%inline real:
  | n=REAL
    { let loc = L.mk_pos $startpos $endpos in T.real ~loc n }


