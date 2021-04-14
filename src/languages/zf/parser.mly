
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parser for Zipperposition Formulas} *)

%parameter <L : Dolmen_intf.Location.S>
%parameter <I : Ast.Id>
%parameter <T : Ast.Term with type location := L.t and type id := I.t>
%parameter <S : Ast.Statement with type location := L.t and type id := I.t and type term := T.t>

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

wildcard:
  | WILDCARD
    { let loc = L.mk_pos $startpos $endpos in T.wildcard ~loc () }

t_type:
  | TYPE
    { let loc = L.mk_pos $startpos $endpos in T.tType ~loc () }

var_or_wildcard:
  | v=raw_var
  | v=wildcard
    { v }

typed_var_block:
  | v=raw_var
  | v=wildcard
    { [ v ] }
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
  | v=raw_var COLON ty=t_type
    { let loc = L.mk_pos $startpos $endpos in
      [ T.colon ~loc v ty ]
    }
  | LEFT_PAREN l=raw_var+ COLON ty=t_type RIGHT_PAREN
    { let loc = L.mk_pos $startpos $endpos in
      List.map (fun x -> T.colon ~loc x ty) l
    }

typed_ty_var_list:
  | l=typed_ty_var_block
    { l }
  | l=typed_ty_var_block l2=typed_ty_var_list
    { l @ l2 }

var:
  | v=raw_var
  | v=wildcard
    { v }

const:
  | TYPE
    { let loc = L.mk_pos $startpos $endpos in T.tType ~loc () }
  | PROP
    { let loc = L.mk_pos $startpos $endpos in T.prop ~loc () }
  | INT
    { let loc = L.mk_pos $startpos $endpos in T.ty_int ~loc () }
  | LOGIC_TRUE
    { let loc = L.mk_pos $startpos $endpos in T.true_ ~loc () }
  | LOGIC_FALSE
    { let loc = L.mk_pos $startpos $endpos in T.false_ ~loc () }

match_branch:
  | VERTICAL_BAR c=raw_var vars=var_or_wildcard* ARROW rhs=term
    { let pattern =
        let loc = L.mk_pos $startpos(c) $endpos(vars) in
        T.apply ~loc c vars
      in
      (pattern,rhs) }

atomic_term:
  | v=var
    { v }
  | t=const
    { t }
  | i=INTEGER
    { let loc = L.mk_pos $startpos $endpos in T.int ~loc i }
  | LEFT_PAREN t=term RIGHT_PAREN
    { t }
  | MATCH t=term WITH l=match_branch+ END
    { let loc = L.mk_pos $startpos $endpos in T.match_ ~loc t l }

apply_term:
  | t=atomic_term
    { t }
  | t=atomic_term u=atomic_term+
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc t u }
  | ARITH_MINUS t=apply_term
    { let loc = L.mk_pos $startpos $endpos in T.uminus ~loc t }

mult_term:
  | t=apply_term
    { t }
  | a=apply_term ARITH_PRODUCT b=mult_term
    { let loc = L.mk_pos $startpos $endpos in T.mult ~loc a b }

%inline PLUS_OP:
  | ARITH_PLUS
    { T.add }
  | ARITH_MINUS
    { T.sub }

plus_term:
  | t=mult_term
    { t }
  | a=mult_term o=PLUS_OP b=plus_term
    { let loc = Some (L.mk_pos $startpos $endpos) in o ?loc a b }

%inline ARITH_OP:
  | ARITH_LT
    { T.lt }
  | ARITH_LEQ
    { T.leq }
  | ARITH_GT
    { T.gt }
  | ARITH_GEQ
    { T.geq }

arith_op_term:
  | t=plus_term
    { t }
  | a=plus_term o=ARITH_OP b=plus_term
    { let loc = Some (L.mk_pos $startpos $endpos) in o ?loc a b }

not_term:
  | t=arith_op_term
    { t }
  | LOGIC_NOT t=arith_op_term
    { let loc = L.mk_pos $startpos $endpos in T.not_ ~loc t }

eq_term:
  | t=not_term
    { t }
  | t=not_term LOGIC_EQ u=not_term
    { let loc = L.mk_pos $startpos $endpos in T.eq ~loc t u }
  | t=not_term LOGIC_NEQ u=not_term
    { let loc = L.mk_pos $startpos $endpos in T.neq ~loc [t; u] }

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
  | LOGIC_FORALL vars=typed_var_list DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc vars t }
  | LOGIC_EXISTS vars=typed_var_list DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.exists ~loc vars t }
  | FUN vars=typed_var_list DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.lambda ~loc vars t }
  | t=apply_term ARROW u=term
    { let loc = L.mk_pos $startpos $endpos in T.arrow ~loc t u }
  | PI vars=typed_ty_var_list DOT t=term
    { let loc = L.mk_pos $startpos $endpos in T.pi ~loc vars t }
  | IF a=term THEN b=term ELSE c=term
    { let loc = L.mk_pos $startpos $endpos in T.ite ~loc a b c }
  /*
  | error
    { let loc = L.mk_pos $startpos $endpos in
      let msg = Format.dprintf ": expected a term" in
      raise (L.Syntax_error (loc, msg)) }
  */

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
  | l=separated_nonempty_list(AND, type_def)
    { l }

attr:
  | a=atomic_attr
    { a }
  | s=raw_var l=atomic_attr+
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc s l }

atomic_attr:
  | s=raw_var
    { s }
  | s=QUOTED
    { let loc = L.mk_pos $startpos $endpos in T.quoted ~loc s }
  | LEFT_PAREN a=attr RIGHT_PAREN
    { a }

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
  | DEF attrs=attrs l=separated_nonempty_list(AND,def) DOT
    { let loc = L.mk_pos $startpos $endpos in S.defs ~loc ~attrs l }
  | REWRITE attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.rewrite ~loc ~attrs t }
  | ASSERT attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.assume ~loc ~attrs t }
  | LEMMA attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.lemma ~loc ~attrs t }
  | GOAL attrs=attrs t=term DOT
    { let loc = L.mk_pos $startpos $endpos in S.goal ~loc ~attrs t }
  | DATA attrs=attrs l=mutual_types DOT
    { let loc = L.mk_pos $startpos $endpos in S.data ~loc ~attrs l }
  /*
  | error
    { let loc = L.mk_pos $startpos $endpos in
      let msg = Format.dprintf ": expected a statement" in
      raise (L.Syntax_error (loc, msg)) }
  */

input:
  | EOF         { None }
  | s=statement { Some s }

file:
  | l=statement* EOF { l }

%%

