
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : ParseLocation.S>
%parameter <I : Ast_tptp.Id>
%parameter <T : Ast_tptp.Term
  with type location := L.t and type id := I.t>
%parameter <S : Ast_tptp.Statement
  with type location := L.t  and type id := I.t and type term := T.t>

%start <S.t list> file
%start <S.t option> input
/* Optional arguments are badly inferred, so these type annotations are required */
%type <(?loc:L.t -> T.t list -> T.t -> T.t) * T.t list> thf_quantification
%type <?loc:L.t -> T.t list -> T.t -> T.t>
  thf_quantifier th1_quantifier th0_quantifier fof_quantifier

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
  | i=tptp_include
    { i }

/* Formula records */

annotated_formula:
  | f=thf_annotated
  | f=tfx_annotated
  | f=tff_annotated
  | f=tcf_annotated
  | f=fof_annotated
  | f=cnf_annotated
  | f=tpi_annotated
    { f }

tpi_annotated:
  | TPI LEFT_PAREN s=name COMMA r=formula_role COMMA
    f=tpi_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.tpi ~loc ?annot s r f }

tpi_formula:
  | f=fof_formula { f }

thf_annotated:
  | THF LEFT_PAREN s=name COMMA r=formula_role COMMA
    f=thf_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.thf ~loc ?annot s r f }

tfx_annotated:
  | TFX LEFT_PAREN s=name COMMA r=formula_role COMMA
    f=tfx_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.tfx ~loc ?annot s r f }

tff_annotated:
  | TFF LEFT_PAREN s=name COMMA r=formula_role COMMA
    f=tff_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.tff ~loc ?annot s r f }

tcf_annotated:
  | TCF LEFT_PAREN s=name COMMA r=formula_role COMMA
    f=tcf_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.tcf ~loc ?annot s r f }

fof_annotated:
  | FOF LEFT_PAREN s=name COMMA r=formula_role COMMA
    f=fof_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.fof ~loc ?annot s r f }

cnf_annotated:
  | CNF LEFT_PAREN s=name COMMA r=formula_role COMMA
    f=cnf_formula annot=annotations RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.cnf ~loc ?annot s r f }

annotations:
  | COMMA s=source i=optional_info
    { let loc = L.mk_pos $startpos $endpos in Some (S.annot ~loc s i) }
  | { None }

formula_role:
  | s=LOWER_WORD { s }


/* THF formulas */

thf_formula:
  | f=thf_sequent
  | f=thf_logic_formula
    { f }

thf_logic_formula:
  | f=thf_binary_formula
  | f=thf_unitary_formula
  | f=thf_type_formula
  | f=thf_subtype
    { f }

thf_binary_formula:
  | f=thf_binary_pair
  | f=thf_binary_tuple
    { f }

thf_binary_pair:
  | f=thf_unitary_formula c=thf_pair_connective g=thf_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f; g] }

thf_binary_tuple:
  | f=thf_or_formula
  | f=thf_and_formula
  | f=thf_apply_formula
    { f }

thf_or_formula:
  | f=thf_unitary_formula VLINE g=thf_unitary_formula
  | f=thf_or_formula VLINE g=thf_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.or_t [f; g] }

thf_and_formula:
  | f=thf_unitary_formula AND g=thf_unitary_formula
  | f=thf_and_formula AND g=thf_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.and_t [f; g] }

thf_apply_formula:
  | f=thf_unitary_formula APPLY g=thf_unitary_formula
  | f=thf_apply_formula APPLY g=thf_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f [g] }

thf_unitary_formula:
  | f=thf_quantified_formula
  | f=thf_unary_formula
  | f=thf_atom
  | f=thf_conditional
  | f=thf_let
  | LEFT_PAREN f=thf_logic_formula RIGHT_PAREN
    { f }
  | l=thf_tuple
    { let loc = L.mk_pos $startpos $endpos in T.tuple ~loc l }

thf_quantified_formula:
  | q=thf_quantification f=thf_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in (fst q) ~loc (snd q) f }

thf_quantification:
  | q=thf_quantifier LEFT_BRACKET l=thf_variable_list RIGHT_BRACKET COLON
    { (q, l) }

thf_variable_list:
  | v=thf_variable
   { [ v ] }
  | v=thf_variable COMMA l=thf_variable_list
   { v :: l }

thf_variable:
  | v=thf_typed_variable
  | v=variable
    { v }

thf_typed_variable:
  | c=variable COLON ty=thf_top_level_type
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc c ty }

thf_unary_formula:
  | c=thf_unary_connective LEFT_PAREN f=thf_logic_formula RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f] }

thf_atom:
  | t=thf_function
  | t=variable
  | t=defined_term
  | t=thf_conn_term
    { t }

thf_function:
  | t=atom
    { t }
  | f=tptp_functor LEFT_PAREN l=thf_arguments RIGHT_PAREN
  | f=defined_functor LEFT_PAREN l=thf_arguments RIGHT_PAREN
  | f=system_functor LEFT_PAREN l=thf_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

thf_conn_term:
  | t=thf_pair_connective
  | t=assoc_connective
  | t=thf_unary_connective
    { t }

thf_conditional:
  | ITE LEFT_PAREN cond=thf_logic_formula COMMA
    if_then=thf_logic_formula COMMA if_else=thf_logic_formula RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.ite ~loc cond if_then if_else }

thf_let:
  | LET LEFT_PAREN l=thf_unitary_formula COMMA f=thf_formula RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.letin ~loc [l] f }

thf_arguments:
  | l=thf_formula_list
    { l }

thf_type_formula:
  | f=thf_typeable_formula COLON ty=thf_top_level_type
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc f ty }

thf_typeable_formula:
  | f=thf_atom
  | LEFT_PAREN f=thf_logic_formula RIGHT_PAREN
    { f }

thf_subtype:
  | t=thf_atom subtype_sign u=thf_atom
    { let loc = L.mk_pos $startpos $endpos in T.subtype ~loc t u }

thf_top_level_type:
  | f=thf_unitary_type
  | f=thf_mapping_type
  { f }

thf_unitary_type:
  | f=thf_unitary_formula
  | LEFT_PAREN f=thf_binary_type RIGHT_PAREN
    { f }

thf_binary_type:
  | t=thf_mapping_type
  | t=thf_xprod_type
  | t=thf_union_type
    { t }

thf_mapping_type:
  | arg=thf_unitary_type ARROW ret=thf_unitary_type
  | arg=thf_unitary_type ARROW ret=thf_mapping_type
    { let loc = L.mk_pos $startpos $endpos in T.arrow ~loc arg ret }

thf_xprod_type:
  | left=thf_unitary_type STAR right=thf_unitary_type
  | left=thf_xprod_type STAR right=thf_unitary_type
    { let loc = L.mk_pos $startpos $endpos in T.product ~loc left right }

thf_union_type:
  | left=thf_unitary_type PLUS right=thf_unitary_type
  | left=thf_union_type PLUS right=thf_unitary_type
    { let loc = L.mk_pos $startpos $endpos in T.union ~loc left right }

thf_sequent:
  | LEFT_PAREN s = thf_sequent RIGHT_PAREN
    { s }
  | hyp=thf_tuple GENTZEN_ARROW goal=thf_tuple
    { let loc = L.mk_pos $startpos $endpos in T.sequent ~loc hyp goal }

thf_tuple:
  | LEFT_BRACKET RIGHT_BRACKET
    { [] }
  | LEFT_BRACKET l = thf_formula_list RIGHT_BRACKET
    { l }

thf_formula_list:
  | f=thf_logic_formula
    { [ f ] }
  | f=thf_logic_formula COMMA l=thf_formula_list
    { f :: l }


/* TFX formulae */

tfx_formula:
  | f=tfx_logic_formula
  | f=thf_sequent
    { f }

tfx_logic_formula:
  | f=thf_logic_formula
    { f }


/* TFF formulae */

tff_formula:
  | f=tff_logic_formula
  | f=tff_typed_atom
  | f=tff_sequent
    { f }

tff_logic_formula:
  | f=tff_binary_formula
  | f=tff_unitary_formula
  | f=tff_subtype
    { f }

tff_binary_formula:
  | f=tff_binary_nonassoc
  | f=tff_binary_assoc
    { f }

tff_binary_nonassoc:
  | f=tff_unitary_formula c=binary_connective g=tff_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f; g] }

tff_binary_assoc:
  | f=tff_or_formula
  | f=tff_and_formula
    { f }

tff_or_formula:
  | f=tff_unitary_formula VLINE g=tff_unitary_formula
  | f=tff_or_formula VLINE g=tff_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.or_t [f; g] }

tff_and_formula:
  | f=tff_unitary_formula AND g=tff_unitary_formula
  | f=tff_and_formula AND g=tff_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.and_t [f; g] }

tff_unitary_formula:
  | f=tff_quantified_formula
  | f=tff_unary_formula
  | f=tff_atomic_formula
  | f=tff_conditional
  | f=tff_let
  | LEFT_PAREN f=tff_logic_formula RIGHT_PAREN
    { f }

tff_quantified_formula:
  | q=fof_quantifier LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON f=tff_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in q ~loc l f }

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
  | u=unary_connective f=tff_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc u [f] }
  | f=fof_infix_unary
    { f }

tff_atomic_formula:
  | f=fof_atomic_formula
    { f }

tff_conditional:
  | ITE_F LEFT_PAREN cond=tff_logic_formula COMMA if_then=tff_logic_formula
    COMMA if_else=tff_logic_formula RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.ite ~loc cond if_then if_else }

tff_let:
  | LET_TF LEFT_PAREN l=tff_let_term_defns COMMA f=tff_formula RIGHT_PAREN
  | LET_FF LEFT_PAREN l=tff_let_formula_defns COMMA f=tff_formula RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.letin ~loc l f }

tff_let_term_defns:
  | f=tff_let_term_defn
    { [f] }
  | LEFT_BRACKET l=tff_let_term_list RIGHT_BRACKET
    { l }

tff_let_term_list:
  | f=tff_let_term_defn
    { [ f ] }
  | f=tff_let_term_defn COMMA l=tff_let_term_list
    { f :: l }

tff_let_term_defn:
  | FORALL LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON t=tff_let_term_defn
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc l t }
  | t=tff_let_term_binding
    { t }

tff_let_term_binding:
  | t=fof_plain_term EQUAL u=fof_term
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.eq_t [t; u] }
  | LEFT_PAREN t=tff_let_term_binding RIGHT_PAREN
    { t }

tff_let_formula_defns:
  | f=tff_let_formula_defn
    { [ f ] }
  | LEFT_BRACKET l=tff_let_formula_list RIGHT_BRACKET
    { l }

tff_let_formula_list:
  | f=tff_let_formula_defn
    { [ f ] }
  | f=tff_let_formula_defn COMMA l=tff_let_formula_list
    { f :: l }

tff_let_formula_defn:
  | FORALL LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON t=tff_let_formula_defn
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc l t }
  | t=tff_let_formula_binding
    { t }

tff_let_formula_binding:
  | t=fof_plain_atomic_formula EQUIV u=tff_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.equiv_t [t; u] }
  | LEFT_PAREN t=tff_let_formula_binding RIGHT_PAREN
    { t }

tff_sequent:
  | hyp=tff_formula_tuple GENTZEN_ARROW goal=tff_formula_tuple
    { let loc = L.mk_pos $startpos $endpos in T.sequent ~loc hyp goal }
  | LEFT_PAREN t=tff_sequent RIGHT_PAREN
    { t }

tff_formula_tuple:
  | LEFT_BRACKET RIGHT_BRACKET
    { [] }
  | LEFT_BRACKET l=tff_formula_tuple_list RIGHT_BRACKET
    { l }

tff_formula_tuple_list:
  | f=tff_logic_formula
    { [ f ] }
  | f=tff_logic_formula COMMA l=tff_formula_tuple_list
    { f :: l }

tff_typed_atom:
  | t=untyped_atom COLON ty=tff_top_level_type
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc t ty }
  | LEFT_PAREN t=tff_typed_atom RIGHT_PAREN
    { t }

tff_subtype:
  | a=untyped_atom subtype_sign b=atom
    { let loc = L.mk_pos $startpos $endpos in T.subtype ~loc a b }

tff_top_level_type:
  | t=tff_atomic_type
  | t=tff_mapping_type
  | t=tf1_quantified_type
  | LEFT_PAREN t=tff_top_level_type RIGHT_PAREN
    { t }

tf1_quantified_type:
  | FORALL_TY LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON t=tff_monotype
    { let loc = L.mk_pos $startpos $endpos in T.pi ~loc l t }

tff_monotype:
  | t=tff_atomic_type
  | LEFT_PAREN t=tff_mapping_type RIGHT_PAREN
    { t }

tff_unitary_type:
  | t=tff_atomic_type
  | LEFT_PAREN t=tff_xprod_type RIGHT_PAREN
    { t }

tff_atomic_type:
  | t=type_constant
  | t=defined_type
  | t=variable
    { t }
  | f=type_functor LEFT_PAREN l=tff_type_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

tff_type_arguments:
  | t=tff_atomic_type
    { [ t ] }
  | t=tff_atomic_type COMMA l=tff_type_arguments
    { t :: l }

tff_mapping_type:
  | arg=tff_unitary_type ARROW ret=tff_atomic_type
    { let loc = L.mk_pos $startpos $endpos in T.arrow ~loc arg ret }

tff_xprod_type:
  | t=tff_unitary_type STAR u=tff_atomic_type
  | t=tff_xprod_type STAR u=tff_atomic_type
    { let loc = L.mk_pos $startpos $endpos in T.product ~loc t u }

/* TCF formulae */

tcf_formula:
  | f=tcf_logic_formula
  | f=tff_typed_atom
    { f }

tcf_logic_formula:
  | f=tcf_quantified_formula
  | f=cnf_formula
    { f }

tcf_quantified_formula:
  | FORALL LEFT_BRACKET l=tff_variable_list RIGHT_BRACKET COLON f=cnf_formula
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc l f }


/* FOF formulas */

fof_formula:
  | f=fof_logic_formula
  | f=fof_sequent
    { f }

fof_logic_formula:
  | f=fof_binary_formula
  | f=fof_unitary_formula
    { f }

fof_binary_formula:
  | f=fof_binary_nonassoc
  | f=fof_binary_assoc
    { f }

fof_binary_nonassoc:
  | f=fof_unitary_formula c=binary_connective g=fof_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f; g] }

fof_binary_assoc:
  | f=fof_or_formula
  | f=fof_and_formula
    { f }

fof_or_formula:
  | f=fof_unitary_formula VLINE g=fof_unitary_formula
  | f=fof_or_formula VLINE g=fof_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.or_t [f; g] }

fof_and_formula:
  | f=fof_unitary_formula AND g=fof_unitary_formula
  | f=fof_and_formula AND g=fof_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.and_t [f; g] }

fof_unitary_formula:
  | f=fof_quantified_formula
  | f=fof_unary_formula
  | f=fof_atomic_formula
  | LEFT_PAREN f=fof_logic_formula RIGHT_PAREN
    { f }

fof_quantified_formula:
  | q=fof_quantifier LEFT_BRACKET l=fof_variable_list RIGHT_BRACKET COLON f=fof_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in q ~loc l f }

fof_variable_list:
  | v=variable
    { [ v ] }
  | v=variable COMMA l=fof_variable_list
    { v :: l }

fof_unary_formula:
  | c=unary_connective f=fof_unitary_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f] }
  | f=fof_infix_unary
    { f }

fof_infix_unary:
  | a=fof_term NOT_EQUAL b=fof_term
    { let loc = L.mk_pos $startpos $endpos in
      T.apply ~loc T.not_t [T.apply ~loc T.eq_t [a; b] ] }

fof_atomic_formula:
  | f=fof_plain_atomic_formula
  | f=fof_defined_atomic_formula
  | f=fof_system_atomic_formula
    { f }

fof_plain_atomic_formula:
  | t=fof_plain_term
    { t }

fof_defined_atomic_formula:
  | f=fof_defined_plain_formula
  | f=fof_defined_infix_formula
    { f }

fof_defined_plain_formula:
  | t=fof_defined_plain_term
    { t }

fof_defined_infix_formula:
  | a=fof_term s=defined_infix_pred b=fof_term
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc s [a; b] }

fof_system_atomic_formula:
  | t=fof_system_term
    { t }

fof_plain_term:
  | c=constant
    { c }
  | f=tptp_functor LEFT_PAREN l=fof_arguments RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f l }

fof_defined_term:
  | t=defined_term
    { t }
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
    { [ t ] }
  | t=fof_term COMMA l=fof_arguments
    { t :: l }

fof_term:
  | t=variable
  | t=tff_let_term
  | t=fof_function_term
  | t=tff_conditional_term
    { t }

fof_function_term:
  | t=fof_plain_term
  | t=fof_defined_term
  | t=fof_system_term
    { t }

tff_conditional_term:
  | ITE_T cond=tff_logic_formula COMMA
    then_term=fof_term COMMA else_term=fof_term RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.ite ~loc cond then_term else_term }

tff_let_term:
  | LET_FT LEFT_PAREN l=tff_let_formula_defns COMMA t=fof_term RIGHT_PAREN
  | LET_TT LEFT_PAREN l=tff_let_term_defns COMMA t=fof_term RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.letin ~loc l t }

fof_sequent:
  | hyp=fof_formula_tuple GENTZEN_ARROW goal=fof_formula_tuple
    { let loc = L.mk_pos $startpos $endpos in T.sequent ~loc hyp goal }
  | LEFT_PAREN t=fof_sequent RIGHT_PAREN
    { t }

fof_formula_tuple:
  | LEFT_BRACKET RIGHT_BRACKET
    { [] }
  | LEFT_BRACKET l=fof_formula_tuple_list RIGHT_BRACKET
    { l }

fof_formula_tuple_list:
  | f=fof_logic_formula
    { [ f ] }
  | f=fof_logic_formula COMMA l=fof_formula_tuple_list
    { f :: l }

cnf_formula:
  | f=disjunction
  | LEFT_PAREN f=disjunction RIGHT_PAREN
    { f }

disjunction:
  | x=literal
    { x }
  | f=disjunction VLINE x=literal
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.or_t [f; x] }

literal:
  | f=fof_atomic_formula
    { f }
  | c=unary_negation f=fof_atomic_formula
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc c [f] }
  | f=fof_infix_unary
    { f }


/* THF connective */

thf_quantifier:
  | q=fof_quantifier
  | q=th0_quantifier
  | q=th1_quantifier
    { q }

th1_quantifier:
  | FORALL_TY
    { T.forall }
  | EXISTS_TY
    { T.exists }

th0_quantifier:
  | LAMBDA
    { T.lambda }
  | TH0_DEFINITE_DESCRIPTION
    { T.description }
  | TH0_INDEFINITE_DESCRIPTION
    { T.choice }

thf_pair_connective:
  | t=infix_equality
  | t=infix_inequality
  | t=binary_connective
  | t=assignment
    { t }

thf_unary_connective:
  | c=unary_connective
  | c=th1_unary_connective
    { c }

th1_unary_connective:
  | PI
    { T.pi_t }
  | SIGMA
    { T.sigma_t }
  | TH1_DEFINITE_DESCRIPTION
    { T.description_t }
  | TH1_INDEFINITE_DESCRIPTION
    { T.choice_t }
  | TH1_EQUALITY
    { T.poly_eq_t }

subtype_sign:
  | LESS LESS { () }

fof_quantifier:
  | FORALL
    { T.forall }
  | EXISTS
    { T.exists }

binary_connective:
  | EQUIV
    { T.equiv_t }
  | IMPLY
    { T.implies_t }
  | LEFT_IMPLY
    { T.implied_t }
  | XOR
    { T.xor_t }
  | NOTVLINE
    { T.nor_t }
  | NOTAND
    { T.nand_t }

assoc_connective:
  | VLINE
    { T.or_t }
  | AND
    { T.and_t }

unary_connective:
  | c=unary_negation
    { c }

assignment:
  | ASSIGNMENT
    { T.assignment_t }

unary_negation:
  | NOT
    { T.not_t }

type_constant:
  | t=type_functor
    { t }

type_functor:
  | t=atomic_word
    { t }

defined_type:
  | t=atomic_defined_word
    { t }

/* First order atoms */

atom:
  | t=untyped_atom
  | t=defined_constant
    { t }

untyped_atom:
  | t=constant
  | t=system_constant
    { t }

defined_infix_pred:
  | t=infix_equality
  | t=assignment
    { t }

infix_equality:
  | EQUAL
    { T.eq_t }

infix_inequality:
  | NOT_EQUAL
    { T.neq_t }

constant:
  | f=tptp_functor
    { f }

tptp_functor:
  | t=atomic_word
    { t }

system_constant:
  | f=system_functor
    { f }

system_functor:
  | t=atomic_system_word
    { t }

defined_constant:
  | f=defined_functor
    { f }

defined_functor:
  | t=atomic_defined_word
    { t }

defined_term:
  | t=number
  | t=distinct_object
    { t }

variable:
  | x=UPPER_WORD
    { let id = I.mk I.term x in
      let loc = L.mk_pos $startpos $endpos in T.var ~loc id }


/* Formula sources */

source:
  | t=general_term
    { t }

optional_info:
  | COMMA i=useful_info
    { i }
  | { [] }

useful_info:
  | l=general_list
    { l }


/* Inlcude directives */

tptp_include:
  | INCLUDE LEFT_PAREN f=file_name g=formula_section RIGHT_PAREN DOT
    { let loc = L.mk_pos $startpos $endpos in S.include_ ~loc f g }

formula_section:
  | COMMA LEFT_BRACKET l=name_list RIGHT_BRACKET
    { l }
  | { [] }

name_list:
  | n=name
    { [ n ] }
  | n=name COMMA l=name_list
    { n :: l }

general_term:
  | d=general_data
    { d }
  | l=general_list
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc T.data_t l }
  | d=general_data COLON t=general_term
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc d t }

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
  name: atomic_word | integer

  this porduction has been expanded to
  produce ids instead of terms
*/
name:
  | s=LOWER_WORD
  | s=SINGLE_QUOTED
  | s=INTEGER
    { I.mk I.decl s }

atomic_word:
  | s=LOWER_WORD
  | s=SINGLE_QUOTED
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s) }

atomic_defined_word:
  | s=DOLLAR_WORD
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s) }

atomic_system_word:
  | s=DOLLAR_DOLLAR_WORD
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s) }

number:
  | n=integer
  | n=rational
  | n=real
    { n }

file_name:
  | s=SINGLE_QUOTED
    { let n = String.length s in String.sub s 1 (n - 2) }

/* Wrapper around some lexical definitions */

distinct_object:
  | s=DISTINCT_OBJECT
    { let loc = L.mk_pos $startpos $endpos in
      T.distinct ~loc (I.mk I.term s) }

integer:
  | n=INTEGER
    { let loc = L.mk_pos $startpos $endpos in T.int ~loc n }

rational:
  | n=RATIONAL
    { let loc = L.mk_pos $startpos $endpos in T.rat ~loc n }

real:
  | n=REAL
    { let loc = L.mk_pos $startpos $endpos in T.real ~loc n }


