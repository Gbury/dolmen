
(* This file is free software, part of dolmen. See file "LICENSE" for more details *)

/* Functor parameters */

%parameter <L : ParseLocation.S>
%parameter <T : Ast_dimacs.Term with type location := L.t>
%parameter <S : Ast_dimacs.Statement with type location := L.t and type term := T.t>

/* Starting symbols */

%start <S.t list> file
%start <S.t option> input

%%

input:
  | start? c=clause
    { Some c }
  | EOF
    { None }

file:
  | NEWLINE* start l=clause* EOF
    { l }

start:
  | P CNF INT INT NEWLINE
    { () }

clause:
  | NEWLINE* c=naked_clause
    { c }

naked_clause:
  | c=nonempty_list(atom) ZERO NEWLINE
    { let loc = L.mk_pos $startpos $endpos in S.clause ~loc c }

atom:
  | s=INT
    { let i =int_of_string s in
      let loc = L.mk_pos $startpos $endpos in T.atom ~loc i }

