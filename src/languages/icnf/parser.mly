
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
  | NEWLINE i=input
    { i }
  | s=start
    { Some s }
  | c=clause
    { Some c }
  | a=assumption
    { Some a }
  | EOF
    { None }

file:
  | NEWLINE* start l=problem
    { l }

start:
  | P INCCNF NEWLINE
    { let loc = L.mk_pos $startpos $endpos in S.p_inccnf ~loc () }

problem:
  | EOF
    { [] }
  | NEWLINE l=problem
    { l }
  | c=clause l=problem
    { c :: l }
  | a=assumption l=problem
    { a :: l }

clause:
  | l=atom+ ZERO NEWLINE
    { let loc = L.mk_pos $startpos $endpos in S.clause ~loc l }

assumption:
  | A l=atom* ZERO NEWLINE
    { let loc = L.mk_pos $startpos $endpos in S.assumption ~loc l }

atom:
  | i=INT
    { let loc = L.mk_pos $startpos $endpos in T.atom ~loc i }

