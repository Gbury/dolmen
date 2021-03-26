
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Smtlib parser */

%token EOF

%token OPEN CLOSE
%token <string> NUM DEC HEX BIN STR SYMBOL KEYWORD

/* Currently unused, see lexer.
%token BINARY DECIMAL HEXADECIMAL NUMERAL STRING
*/
%token UNDERSCORE ATTRIBUTE AS LET EXISTS FORALL MATCH PAR

%token ASSERT
       CHECK_SAT
       CHECK_SAT_ASSUMING
       DECLARE_CONST
       DECLARE_DATATYPE
       DECLARE_DATATYPES
       DECLARE_FUN
       DECLARE_SORT
       DEFINE_FUN
       DEFINE_FUN_REC
       DEFINE_FUNS_REC
       DEFINE_SORT
       ECHO EXIT
       GET_ASSERTIONS
       GET_ASSIGNMENT
       GET_INFO
       GET_MODEL
       GET_OPTION
       GET_PROOF
       GET_UNSAT_ASSUMPTIONS
       GET_UNSAT_CORE
       GET_VALUE
       POP
       PUSH
       RESET
       RESET_ASSERTIONS
       SET_INFO
       SET_LOGIC
       SET_OPTION

%%

