
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Tptp parser */

%token EOF

%token DOT
%token COMMA
%token COLON

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token CNF
%token FOF
%token TFF
%token THF
%token TPI
%token INCLUDE

%token NOT

%token STAR
%token ARROW
%token FORALL_TY
%token TYPE_TY
%token WILDCARD

%token LAMBDA
%token APPLY
%token AND
%token NOTAND
%token VLINE
%token NOTVLINE
%token IMPLY
%token LEFT_IMPLY
%token EQUIV
%token XOR
%token GENTZEN_ARROW
%token EQUAL
%token NOT_EQUAL

%token ITE_F
%token LET_TF
%token LET_FF

%token FORALL
%token EXISTS

%token UNDERSCORE

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> SINGLE_QUOTED
%token <string> DISTINCT_OBJECT
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> REAL
%token <string> RATIONAL
%token <string> INTEGER

%left VLINE
%left AND
%nonassoc EQUIV
%nonassoc XOR
%nonassoc IMPLY
%nonassoc LEFT_IMPLY
%nonassoc NOTVLINE
%nonassoc NOTAND

%%

