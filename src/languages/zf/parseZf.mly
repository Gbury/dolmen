
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parser for Zipperposition Formulas} *)

%parameter <L : ParseLocation.S>
%parameter <I : Ast_zf.Id>
%parameter <T : Ast_zf.Term
  with type location := L.t and type id := I.t>
%parameter <S : Ast_zf.Statement
  with type location := L.t and type id := I.t and type term := T.t>

%start <S.t list> file
%start <S.t option> input

%%
name:
  | w=LOWER_WORD
  | w=UPPER_WORD
    { I.mk I.term w }

raw_var:
  | s=name
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc s }

typed_var_block:
  | v=raw_var
    { [ v ] }
  | WILDCARD
    { [ T.wildcard ] }
  | LEFT_PAREN l=raw_var+ COLON t=term RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in
      List.map (fun x -> T.colon ~loc x t) l }

typed_var_list:
  | l=typed_var_block
    { l }
  | l=typed_var_block l2=typed_var_list
    { l @ l2 }

typed_ty_var_block:
  | v=raw_var
    { [ v ] }
  | v=raw_var COLON TYPE
    { let loc = L.mk_pos $startpos $endpos in [ T.colon ~loc v T.tType ] }
  | LEFT_PAREN l=raw_var+ COLON TYPE RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in
      List.map (fun x -> T.colon ~loc x T.tType) l }

typed_ty_var_list:
  | l=typed_ty_var_block
    { l }
  | l=typed_ty_var_block l2=typed_ty_var_list
    { l @ l2 }

var:
  | v=raw_var
    { v }
  | WILDCARD
    { T.wildcard }

const:
  | TYPE
    { T.tType }
  | PROP
    { T.prop }
  | LOGIC_TRUE
    { T.true_ }
  | LOGIC_FALSE
    { T.false_ }

atomic_term:
  | v=var
    { v }
  | t=const
    { t }
  | LEFT_PAREN t=term RIGHT_PAREN
    { t }

apply_term:
  | t=atomic_term
    { t }
  | t=atomic_term u=atomic_term+
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc t u }
  | LOGIC_NOT t=apply_term
    { let loc = L.mk_pos $startpos $endpos in T.not_ ~loc t }

eq_term:
  | t=apply_term
    { t }
  | t=apply_term LOGIC_EQ u=apply_term
    { let loc = L.mk_pos $startpos $endpos in T.eq ~loc t u }
  | t=apply_term LOGIC_NEQ u=apply_term
    { let loc = L.mk_pos $startpos $endpos in T.not_ ~loc (T.eq ~loc t u) }

and_term:
  | t=eq_term
    { t }
  | t=eq_term LOGIC_AND u=and_term
    { let loc = L.mk_pos $startpos $endpos in T.and_ ~loc [t; u] }

or_term:
  | t=and_term
    { t }
  | t=and_term LOGIC_OR u=or_term
    { let loc = L.mk_pos $startpos $endpos in T.or_ ~loc [t; u] }
  | t=and_term LOGIC_IMPLY u=or_term
    { let loc = L.mk_pos $startpos $endpos in T.imply ~loc t u }
  | t=and_term LOGIC_EQUIV u=or_term
    { let loc = L.mk_pos $startpos $endpos in T.equiv ~loc t u }

term:
  | t=or_term
    { t }
  | t=apply_term ARROW u=term
    { let loc = L.mk_pos $startpos $endpos in T.arrow ~loc t u }
  | PI vars=typed_ty_var_list DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.pi ~loc vars t }
  | LOGIC_FORALL vars=typed_var_list DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc vars t }
  | LOGIC_EXISTS vars=typed_var_list DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.exists ~loc vars t }
  | error
    { let loc = L.mk_pos $startpos $endpos in raise (L.Syntax_error (loc, "expected term")) }

constructor:
  | v=name l=atomic_term*
    { v, l }

constructors:
  | VERTICAL_BAR? l=separated_nonempty_list(VERTICAL_BAR, constructor)
    { l }

type_def:
  | t=name vars=raw_var* EQDEF l=constructors
    { let loc = L.mk_pos $startpos $endpos in S.inductive ~loc t vars l }

mutual_types:
  | l=separated_nonempty_list(AND, type_def) { l }

attr:
  | AC
    { T.ac }
  | NAME COLON v=name
    { let loc = L.mk_pos $startpos $endpos in
      T.name ~loc v }

attrs:
  | LEFT_BRACKET l=separated_nonempty_list(COMMA, attr) RIGHT_BRACKET
    { l }
  | { [] }

def:
  | id=name COLON ty=term EQDEF t=term
    { let v =
        let loc = L.mk_pos $startpos $endpos in
        T.const ~loc id
      in
      let loc = L.mk_pos $startpos $endpos in
      let eq = T.eq ~loc v t in
      S.definition ~loc id ty [eq] }
  | id=name COLON ty=term WHERE rules=separated_nonempty_list(SEMI_COLON, term)
    { let loc = L.mk_pos $startpos $endpos in
      S.definition ~loc id ty rules }

statement:
  | INCLUDE s=QUOTED DOT
    { let loc = L.mk_pos $startpos $endpos in S.import ~loc s }
  | VAL attrs=attrs v=name COLON t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.decl ~loc ~attrs v t }
  | REWRITE attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.rewrite ~loc ~attrs t }
  | ASSERT attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.assume ~loc ~attrs t }
  | LEMMA attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.lemma ~loc ~attrs t }
  | GOAL attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.goal ~loc ~attrs t }
  | DEF attrs=attrs l=separated_nonempty_list(AND,def) DOT
    { let loc = L.mk_pos $startpos $endpos in S.defs ~loc ~attrs l }
  | DATA attrs=attrs l=mutual_types DOT
    { let loc = L.mk_pos $startpos $endpos in S.data ~loc ~attrs l }
  | error
    { let loc = L.mk_pos $startpos $endpos in raise (L.Syntax_error (loc, "expected statement")) }

input:
  | EOF         { None }
  | s=statement { Some s }

file:
  | l=statement* EOF { l }

%%

