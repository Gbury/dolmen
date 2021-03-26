
(* This file is free software, part of dolmen. See file "LICENSE" for more details *)

/* Functor parameters */

%parameter <L : Dolmen_intf.Location.S>
%parameter <T : Ast.Term with type location := L.t>
%parameter <S : Ast.Statement with type location := L.t and type term := T.t>

/* Starting symbols */

%start <S.t list> file
%start <S.t option> input

%%

input:
  | NEWLINE* EOF
    { None }
  | NEWLINE* s=start
    { Some s }
  | NEWLINE* c=clause
    { Some c }

file:
  | NEWLINE* h=start l=cnf
    { h :: l }

start:
  | P CNF nbvar=INT nbclause=INT NEWLINE
    { let loc = L.mk_pos $startpos $endpos in
      S.p_cnf ~loc nbvar nbclause }

cnf:
  | EOF
    { [] }
  | NEWLINE l=cnf
    { l }
  | c=clause l=cnf
    { c :: l }

clause:
  | c=nonempty_list(atom) ZERO NEWLINE
    { let loc = L.mk_pos $startpos $endpos in S.clause ~loc c }

atom:
  | i=INT
    { let loc = L.mk_pos $startpos $endpos in T.atom ~loc i }

