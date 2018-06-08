
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Smtlib parser */

%token EOF

%token OPEN CLOSE
%token <string> NUMERAL DECIMAL HEXADECIMAL BINARY STRING SYMBOL KEYWORD

%token UNDERSCORE

%token AS PAR NOT

%token LET FORALL EXISTS ATTRIBUTE 

%token ASSERT CHECK_SAT 
DECLARE_CONST DECLARE_DATATYPE DECLARE_DATATYPES 
DECLARE_FUN DECLARE_HEAP DECLARE_SORT
DEFINE_FUN_REC DEFINE_FUNS_REC DEFINE_FUN DEFINE_SORT
EXIT
GET_ASSERTIONS GET_ASSIGNMENT GET_INFO GET_MODEL 
GET_OPTION GET_PROOF GET_UNSAT_CORE
GET_VALUE 
POP PUSH
SET_INFO SET_LOGIC SET_OPTION 

%%


