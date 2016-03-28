
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Zf parser */

%token EOF

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token WILDCARD
%token DOT
%token COLON
%token EQDEF
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

%token PROP
%token TYPE

%token ASSERT
%token DATA
%token DEF
%token VAL
%token GOAL

%token ARROW
%token PI
%token VERTICAL_BAR

%token <string> LOWER_WORD
%token <string> UPPER_WORD

%%


