
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Smtlib parser */

%token EOF

%token OPEN CLOSE
%token <string> NUM DEC HEX BIN STR SYMBOL KEYWORD

/* Currently unused, see lexer.
%token BINARY DECIMAL HEXADECIMAL NUMERAL STRING
*/
%token UNDERSCORE ATTRIBUTE AS LET EXISTS FORALL MATCH

%token SAT
       UNSAT
       DEFINE_FUN
       DEFINE_FUN_REC
       DEFINE_FUNS_REC
       MODEL

%%

