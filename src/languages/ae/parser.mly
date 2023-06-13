
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : Dolmen_intf.Location.S>
%parameter <I : Ast.Id>
%parameter <T : Ast.Term
  with type location := L.t and type id := I.t>
%parameter <S : Ast.Statement
  with type location := L.t  and type id := I.t and type term := T.t>

%start <S.t list> file
%start <S.t option> input

%%

file:
  | l=decl* EOF { l }

/* The current syntax has no clear delimiters to denote the end
   of declarations resulting in a lot of end-of-stream conflicts.
   This prevents incremental parsing from working correctly,
   hence the assert false */
input:
  | EOF { assert false }
/* this declaration creates end-of-stream conflicts
  | decl    { assert false }
*/

/* Identifiers */

raw_ident:
  | id=ID
    { (fun ns -> I.mk ns id) }

decl_ident:
  | id=raw_ident
    { id I.decl }

ident:
  | id=raw_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (id I.term) }

raw_named_ident:
  | id=ID
    { I.mk I.term id }
  | id=ID str=STRING
    { let track = I.mk I.track str in
      I.tracked ~track I.term id }

named_ident:
  | id=raw_named_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc id }

ty_ident:
  | id=raw_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (id I.sort) }


/* Binders */

logic_binder:
  | v=ident COLON ty=primitive_type
    { let loc = L.mk_pos $startpos $endpos in
      T.colon ~loc v ty }

multi_logic_binder:
  | vars=separated_nonempty_list(COMMA, named_ident) COLON ty=primitive_type
    { let loc = L.mk_pos $startpos $endpos in
      List.map (fun x -> T.colon ~loc x ty) vars }


/* Type variables */

type_var:
  | QUOTE id=ID
    { let loc = L.mk_pos $startpos $endpos in
      let v = I.mk I.var ("'" ^ id) in
      T.const ~loc v }

type_vars:
  | { [] }
  | v=type_var
    { [v] }
  | LEFTPAR l=separated_nonempty_list(COMMA, type_var) RIGHTPAR
    { l }



/* Type Expressions */

primitive_type:
  | BOOL
    { let loc = L.mk_pos $startpos $endpos in
      T.bool ~loc () }
  | UNIT
    { let loc = L.mk_pos $startpos $endpos in
      T.ty_unit ~loc () }
  | INT
    { let loc = L.mk_pos $startpos $endpos in
      T.ty_int ~loc () }
  | REAL
    { let loc = L.mk_pos $startpos $endpos in
      T.ty_real ~loc () }

  | BITV LEFTSQ sz=INTEGER RIGHTSQ
    { let loc = L.mk_pos $startpos $endpos in
      let n =
        (* The lexer should guarantee that the length of a bitv is
         * a syntactically correct integer. *)
        match int_of_string sz with
        | i -> i
        | exception Invalid_argument _ -> assert false
      in
      T.ty_bitv ~loc n }

  | c=ty_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.apply ~loc c [] }

  | v=type_var
    { v }

  | arg=primitive_type c=ty_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.apply ~loc c [arg] }

  | LEFTPAR args=separated_nonempty_list(COMMA, primitive_type) RIGHTPAR c=ty_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.apply ~loc c args }

primitive_type_or_prop:
  | ty=primitive_type
    { ty }
  | PROP
    { let loc = L.mk_pos $startpos $endpos in
      T.prop ~loc () }

logic_type:
  | ty=primitive_type_or_prop
    { ty }
  | l=separated_list(COMMA, primitive_type) RIGHTARROW ret=primitive_type_or_prop
    { let loc = L.mk_pos $startpos $endpos in
      List.fold_right (T.arrow ~loc) l ret }


/* Main Expression language */

lexpr:
  | e=simple_expr
    { e }

  /* Unary Operators */

  | NOT p=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.not_ ~loc p }
  | MINUS x=lexpr %prec uminus
    { let loc = L.mk_pos $startpos $endpos in
      T.uminus ~loc x }


  /* Binary Operators */

  | a=lexpr PLUS b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.add ~loc a b }
  | a=lexpr MINUS b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.sub ~loc a b }
  | a=lexpr TIMES b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.mult ~loc a b }
  | a=lexpr SLASH b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.div ~loc a b }
  | a=lexpr PERCENT b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.mod_ ~loc a b }
  | a= lexpr POW b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.int_pow ~loc a b }
  | a=lexpr POWDOT b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.real_pow ~loc a b }

  | p=lexpr AND q=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.and_ ~loc [p; q] }
  | p=lexpr OR q=lexpr
    { let loc = L.mk_pos $startpos $endpos in
    T.or_ ~loc [p; q] }
  | p=lexpr XOR q=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.xor ~loc p q }
  | p=lexpr LRARROW q=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.equiv ~loc p q }
  | p=lexpr RIGHTARROW q=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.imply ~loc p q }

  | a=lexpr LT b=lexpr %prec prec_relation
    { let loc = L.mk_pos $startpos $endpos in
      T.lt ~loc a b }
  | a=lexpr LE b=lexpr %prec prec_relation
    { let loc = L.mk_pos $startpos $endpos in
      T.leq ~loc a b }
  | a=lexpr GT b=lexpr %prec prec_relation
    { let loc = L.mk_pos $startpos $endpos in
      T.gt ~loc a b }
  | a=lexpr GE b=lexpr %prec prec_relation
    { let loc = L.mk_pos $startpos $endpos in
      T.geq ~loc a b }

  | a=lexpr EQUAL b=lexpr %prec prec_relation
    { let loc = L.mk_pos $startpos $endpos in
      T.eq ~loc a b }
  | a=lexpr NOTEQ b=lexpr %prec prec_relation
    { let loc = L.mk_pos $startpos $endpos in
      T.neq ~loc [a; b] }


  /* Bit Vectors */

  | LEFTSQ BAR c=INTEGER BAR RIGHTSQ
    { let loc = L.mk_pos $startpos $endpos in
      T.bitv ~loc c }
  | e=lexpr HAT LEFTBR i=INTEGER COMMA j=INTEGER RIGHTBR
    { let loc = L.mk_pos $startpos $endpos in
      let i, j =
        match int_of_string i, int_of_string j with
        | i, j -> i, j
        | exception Invalid_argument _ -> assert false
      in
      T.bitv_extract ~loc e i j }
  | e=lexpr AT f=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.bitv_concat ~loc e f }


  /* Predicates/Function Calls */

  | DISTINCT LEFTPAR l=list2_lexpr_sep_comma RIGHTPAR
    { let loc = L.mk_pos $startpos $endpos in
      T.neq ~loc l }

  | IF cond=lexpr THEN then_t=lexpr ELSE else_t=lexpr %prec prec_ite
    { let loc = L.mk_pos $startpos $endpos in
      T.ite ~loc cond then_t else_t }

  | FORALL vars=separated_nonempty_list(COMMA, multi_logic_binder)
           triggers=triggers filters=filters DOT body=lexpr %prec prec_forall
    { let loc = L.mk_pos $startpos $endpos in
      let body = T.triggers ~loc body triggers in
      let body = T.filters ~loc body filters in
      T.forall ~loc (List.flatten vars) body }
  | EXISTS vars=separated_nonempty_list(COMMA, multi_logic_binder)
           triggers=triggers filters=filters DOT body=lexpr %prec prec_exists
    { let loc = L.mk_pos $startpos $endpos in
      let body = T.triggers ~loc body triggers in
      let body = T.filters ~loc body filters in
      T.exists ~loc (List.flatten vars) body }

  | name=STRING COLON e=lexpr %prec prec_named
   { let loc = L.mk_pos $startpos $endpos in
     let id = I.mk I.track name in
     T.tracked ~loc id e }

  | LET l=separated_nonempty_list(COMMA, let_binder) IN body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.letin ~loc l body }

  | CHECK e=lexpr
   { let loc = L.mk_pos $startpos $endpos in
     T.check ~loc e }

  | CUT e=lexpr
   { let loc = L.mk_pos $startpos $endpos in
     T.cut ~loc e }


  /* Match */

  | MATCH e=lexpr WITH cases=match_cases END
    { let loc = L.mk_pos $startpos $endpos in
      T.match_ ~loc e (List.rev cases) }

match_case:
  | p=simple_pattern RIGHTARROW e = lexpr
    { p, e }

match_cases:
  | c=match_case
  | BAR c=match_case
    { [c] }
  | l=match_cases BAR c=match_case
    { c :: l }

simple_pattern:
  | t=ident
    { t }
  | f=ident LEFTPAR args=separated_nonempty_list(COMMA,ident) RIGHTPAR
   { let loc = L.mk_pos $startpos $endpos in
     T.apply ~loc f args }

let_binder:
  | a=ident EQUAL b=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.eq ~loc a b }

simple_expr :
  | t=ident
    { t }
  | LEFTPAR e=lexpr RIGHTPAR
    { e }

  | s=INTEGER
    { let loc = L.mk_pos $startpos $endpos in
      T.int ~loc s }
  | s=DECIMAL
    { let loc = L.mk_pos $startpos $endpos in
      T.real ~loc s }
  | s=HEXADECIMAL
    { let loc = L.mk_pos $startpos $endpos in
      T.hexa ~loc s }

  | TRUE
    { let loc = L.mk_pos $startpos $endpos in
      T.true_ ~loc () }
  | FALSE
    { let loc = L.mk_pos $startpos $endpos in
      T.false_ ~loc () }
  | VOID
    { let loc = L.mk_pos $startpos $endpos in
      T.void ~loc () }


  /* Records */

  | LEFTBR l=separated_nonempty_list(PV, label_expr) RIGHTBR
    { let loc = L.mk_pos $startpos $endpos in
      T.record ~loc l }
  | LEFTBR s=simple_expr WITH l=separated_nonempty_list(PV, label_expr) RIGHTBR
    { let loc = L.mk_pos $startpos $endpos in
      T.record_with ~loc s l }
  | s=simple_expr DOT label=raw_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.record_access ~loc s (label I.term) }


  /* Function/Predicate Calls */

  | f=ident LEFTPAR args=separated_list(COMMA, lexpr) RIGHTPAR
    { let loc = L.mk_pos $startpos $endpos in
      T.apply ~loc f args }


  /* Arrays */

  | s=simple_expr LEFTSQ e=lexpr RIGHTSQ
    { let loc = L.mk_pos $startpos $endpos in
      T.array_get ~loc s e }

  | s=simple_expr LEFTSQ l=separated_nonempty_list(COMMA, array_assignment) RIGHTSQ
    { let loc = L.mk_pos $startpos $endpos in
      List.fold_left (fun acc (idx, value) ->
        T.array_set ~loc acc idx value
      ) s l }


  | s=simple_expr COLON ty=primitive_type
    { let loc = L.mk_pos $startpos $endpos in
      T.colon ~loc s ty }

  | s=simple_expr QM id=raw_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.adt_check ~loc s (id I.term) }

  | s=simple_expr c=QM_ID
    { let loc = L.mk_pos $startpos $endpos in
      T.adt_check ~loc s (I.mk I.term c) }

  | s=simple_expr SHARP label=raw_ident
    { let loc = L.mk_pos $startpos $endpos in
      T.adt_project ~loc s (label I.term) }

array_assignment:
  | e1=lexpr LEFTARROW e2=lexpr
    { (e1, e2) }

triggers:
  | { [] }
  | LEFTSQ l=separated_nonempty_list(BAR, trigger) RIGHTSQ
    { l }

filters:
  | { [] }
  | LEFTBR l=separated_nonempty_list(COMMA, lexpr) RIGHTBR
   { l }

trigger:
  | l=separated_nonempty_list(COMMA, lexpr_or_dom)
   { let loc = L.mk_pos $startpos $endpos in
     T.trigger ~loc l }

lexpr_or_dom:
  | e=lexpr
    { e }
  | e=lexpr IN ls=sq lb=bound COMMA rb=bound rs=sq
    { let loc = L.mk_pos $startpos $endpos in
      T.in_interval ~loc e (lb, not ls) (rb, rs) }
  | id=raw_ident MAPS_TO e=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.maps_to ~loc (id I.term) e }

sq:
  | LEFTSQ
    { true }
  | RIGHTSQ
    { false }

bound:
  | QM
    { let loc = L.mk_pos $startpos $endpos in
      let v = I.mk I.term "?" in
      T.const ~loc v }
  | s=ID
  | s=QM_ID
    { let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s) }
  | s=INTEGER
    { let loc = L.mk_pos $startpos $endpos in
      T.int ~loc s }
  | s=DECIMAL
    { let loc = L.mk_pos $startpos $endpos in
      T.real ~loc s }
  | s=HEXADECIMAL
    { let loc = L.mk_pos $startpos $endpos in
      T.hexa ~loc s }
  | MINUS s=INTEGER
    { let loc = L.mk_pos $startpos $endpos in
      T.int ~loc ("-" ^ s) }
  | MINUS s=DECIMAL
    { let loc = L.mk_pos $startpos $endpos in
      T.real ~loc ("-" ^ s) }
  | MINUS s=HEXADECIMAL
    { let loc = L.mk_pos $startpos $endpos in
      T.hexa ~loc ("-" ^ s) }

list2_lexpr_sep_comma:
  | e1=lexpr COMMA e2=lexpr
    { [e1; e2] }
  | e=lexpr COMMA l=list2_lexpr_sep_comma
    { e :: l }

label_expr:
  | id=ident EQUAL e=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      T.eq ~loc id e }


/* Type definitions */

record_label_with_type:
  | id=raw_ident COLON ty=primitive_type
    { id I.term, ty }

record_type:
  | LEFTBR l=separated_nonempty_list(PV, record_label_with_type) RIGHTBR
    { l }

algebraic_label_with_type:
  | id=ident COLON ty=primitive_type
    { let loc = L.mk_pos $startpos $endpos in
      T.colon ~loc id ty }

algebraic_args:
  | { [] }
  | OF LEFTBR l=separated_nonempty_list(PV, algebraic_label_with_type) RIGHTBR
    { l }

algebraic_constructor:
  | c=raw_ident l=algebraic_args
    { c I.term, l }

algebraic_typedef:
  | vars=type_vars c=raw_ident EQUAL l=separated_nonempty_list(BAR, algebraic_constructor)
    { let loc = L.mk_pos $startpos $endpos in
      S.algebraic_type ~loc (c I.sort) vars l }


/* Top-level declarations */

ac_modifier:
  | /* empty */
    { false }
  | AC
    { true }

theory_elt:
  | AXIOM name=decl_ident COLON body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.axiom ~loc name body }

  | CASESPLIT name=decl_ident COLON body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.case_split ~loc name body }

rewriting_list:
  | e=lexpr
  | e=lexpr PV
    { [e] }
  | e=lexpr PV l=rewriting_list
    { e :: l }

%inline function_def:
  | FUNC f=raw_named_ident
    LEFTPAR args=separated_list(COMMA, logic_binder) RIGHTPAR
    COLON ret_ty=primitive_type EQUAL body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.fun_def ~loc f [] args ret_ty body }

%inline predicate_def:
  | PRED p=raw_named_ident EQUAL body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.pred_def ~loc p [] [] body }

  | PRED p=raw_named_ident
    LEFTPAR args=separated_list(COMMA, logic_binder) RIGHTPAR EQUAL body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.pred_def ~loc p [] args body }

%inline function_or_predicate_def:
  | s=function_def
  | s=predicate_def
    { s }

decl:
  | THEORY id=decl_ident EXTENDS ext=decl_ident EQUAL l=theory_elt* END
    { let loc = L.mk_pos $startpos $endpos in
      S.theory ~loc id ext l }

  | TYPE vars=type_vars id=raw_ident
    { let loc = L.mk_pos $startpos $endpos in
      S.abstract_type ~loc (id I.sort) vars }

  | TYPE l=separated_nonempty_list(AND, algebraic_typedef)
    { let loc = L.mk_pos $startpos $endpos in
      S.rec_types ~loc l }

  | TYPE vars=type_vars id=raw_ident EQUAL r=record_type
    { let loc = L.mk_pos $startpos $endpos in
      S.record_type ~loc (id I.sort) vars r }

  | LOGIC ac=ac_modifier args=separated_nonempty_list(COMMA, raw_named_ident) COLON ty=logic_type
    { let loc = L.mk_pos $startpos $endpos in
      S.logic ~loc ~ac args ty }

  | l=separated_nonempty_list(AND, function_or_predicate_def)
    { let loc = L.mk_pos $startpos $endpos in
      S.defs ~loc l }

  | AXIOM name=decl_ident COLON body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.axiom ~loc name body }

  | REWRITING name=decl_ident COLON l=rewriting_list
    { let loc = L.mk_pos $startpos $endpos in
      S.rewriting ~loc name l }

  | GOAL name=decl_ident COLON body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.prove_goal ~loc name body }

  | CHECK_SAT name=decl_ident COLON body=lexpr
    { let loc = L.mk_pos $startpos $endpos in
      S.prove_sat ~loc ~name [body] }
