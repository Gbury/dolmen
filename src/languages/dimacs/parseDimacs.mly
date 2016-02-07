
(* This file is free software, part of dolmen. See file "LICENSE" for more details *)

/* Functor parameters */

%parameter <L : ParseLocation.S>
%parameter <T : Ast_dimacs.S>

/* Starting symbols */

%start <T.clause list> file

%%

file:
  | NEWLINE* start l=clause* EOF
    { l }

start:
  | P CNF nbvar=INT nbclause=INT NEWLINE+ { () }

clause:
  | c=nonempty_list(atom) ZERO NEWLINE+
    { T.mk_clause c }

atom:
  | i=INT
    { T.mk_atom i }

