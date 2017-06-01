
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Zf parser */

%token EOF

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token WILDCARD
%token COMMA
%token DOT
%token SEMI_COLON
%token COLON
%token EQDEF
%token WHERE
%token AND

%token LOGIC_TRUE
%token LOGIC_FALSE
%token LOGIC_AND
%token LOGIC_OR
%token LOGIC_NOT
%token LOGIC_IMPLY
%token LOGIC_FORALL
%token LOGIC_EXISTS
%token LOGIC_EQ
%token LOGIC_NEQ
%token LOGIC_EQUIV

%token ARITH_PLUS
%token ARITH_MINUS
%token ARITH_PRODUCT
%token ARITH_LT
%token ARITH_LEQ
%token ARITH_GT
%token ARITH_GEQ

%token IF
%token THEN
%token ELSE

%token MATCH
%token WITH
%token END
%token FUN

%token INT
%token PROP
%token TYPE

%token ASSERT
%token DATA
%token DEF
%token VAL
%token GOAL
%token REWRITE
%token LEMMA
%token INCLUDE

%token ARROW
%token PI
%token VERTICAL_BAR

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> QUOTED
%token <string> INTEGER

%%


