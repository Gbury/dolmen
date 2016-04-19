
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parser for Zipperposition Formulas} *)

%parameter <L : ParseLocation.S>
%parameter <T : Ast_zf.Term with type location := L.t>
%parameter <S : Ast_zf.Statement with type location := L.t and type term := T.t>

%start <S.t list> file
%start <S.t option> input

%%

name:
  | w=LOWER_WORD { w }
  | w=UPPER_WORD { w }

raw_var:
  | s=name { let loc = L.mk_pos $startpos $endpos in T.const ~loc s }

typed_var:
  | v=raw_var
    { v }
  | LEFT_PAREN v=raw_var COLON t=term RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.typed ~loc v t }

typed_ty_var:
  | v=raw_var
    { v }
  | v=raw_var COLON TYPE
    { let loc = L.mk_pos $startpos $endpos in T.typed ~loc v T.tType }
  | LEFT_PAREN v=raw_var COLON TYPE RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in T.typed ~loc v T.tType }

var:
  | v=raw_var
    { v }
  | WILDCARD
    { let loc = L.mk_pos $startpos $endpos in T.at_loc ~loc T.wildcard }

const:
  | TYPE
    { let loc = L.mk_pos $startpos $endpos in T.at_loc ~loc T.tType }
  | PROP
    { let loc = L.mk_pos $startpos $endpos in T.at_loc ~loc T.prop }
  | LOGIC_TRUE
    { let loc = L.mk_pos $startpos $endpos in T.at_loc ~loc T.true_ }
  | LOGIC_FALSE
    { let loc = L.mk_pos $startpos $endpos in T.at_loc ~loc T.false_ }

atomic_term:
  | v=var                         { v }
  | t=const                       { t }
  | LEFT_PAREN t=term RIGHT_PAREN { t }

apply_term:
  | t=atomic_term
    { t }
  | t=atomic_term u=atomic_term+
    { let loc = L.mk_pos $startpos $endpos in T.app ~loc t u }
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
  | LOGIC_FORALL vars=typed_var+ DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc vars t }
  | LOGIC_EXISTS vars=typed_var+ DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.exists ~loc vars t }
  | t=apply_term ARROW u=term
    { let loc = L.mk_pos $startpos $endpos in T.fun_ty ~loc [t] u }
  | PI vars=typed_ty_var+ DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.forall_ty ~loc vars t }
  | error
    { let loc = L.mk_pos $startpos $endpos in raise (L.Syntax_error (loc, "expected term")) }

constructor:
  | v=name l=atomic_term* { v, l }

constructors:
  | VERTICAL_BAR? l=separated_nonempty_list(VERTICAL_BAR, constructor) { l }

type_def:
  | t=name vars=raw_var* EQDEF l=constructors
    { let loc = L.mk_pos $startpos $endpos in S.inductive ~loc t vars l }

mutual_types:
  | l=separated_nonempty_list(AND, type_def) { l }

attrs:
  | LEFT_BRACKET s=name RIGHT_BRACKET
    { let loc = L.mk_pos $startpos $endpos in S.attr ~loc s }
  | { S.default_attr }

statement:
  | VAL v=name COLON t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.decl ~loc v t }
  | DEF v=name COLON t=term EQDEF u=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.def ~loc v t u }
  | ASSERT a=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.assume ~loc ~attr:a t }
  | GOAL a=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.goal ~loc ~attr:a t }
  | DATA l=mutual_types DOT
    { let loc = L.mk_pos $startpos $endpos in S.data ~loc l }
  | error
    { let loc = L.mk_pos $startpos $endpos in raise (L.Syntax_error (loc, "expected statement")) }

input:
  | EOF         { None }
  | s=statement { Some s }

file:
  | l=statement* EOF { l }

%%

