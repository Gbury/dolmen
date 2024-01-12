
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Tptp parser */

%token EOF

%token DASH
%token DOT
%token COMMA
%token COLON

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_CURLY
%token RIGHT_CURLY

%token CNF
%token FOF
%token TFF
%token TCF
%token THF
%token TPI
%token INCLUDE

%token LAMBDA
%token APPLY
%token DEFINITE_DESCRIPTION
%token INDEFINITE_DESCRIPTION
%token FORALL_TY
%token FORALL
%token EXISTS_TY
%token EXISTS

%token PI
%token SIGMA

%token LESS
%token ARROW

%token STAR
%token PLUS

%token XOR
%token EQUIV
%token IMPLY
%token LEFT_IMPLY

%token NOT
%token AND
%token VLINE
%token NOTAND
%token NOTVLINE

%token EQUAL
%token NOT_EQUAL
%token ASSIGNMENT
%token IDENTICAL
%token GENTZEN_ARROW

%token ITE
%token LET

%token DOLLAR_THF
%token DOLLAR_TFF
%token DOLLAR_FOF
%token DOLLAR_CNF
%token DOLLAR_FOT

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> SINGLE_QUOTED
%token <string> DISTINCT_OBJECT
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> REAL
%token <string> RATIONAL
%token <string> INTEGER

/*

%left VLINE
%left AND
%nonassoc EQUIV
%nonassoc XOR
%nonassoc IMPLY
%nonassoc LEFT_IMPLY
%nonassoc NOTVLINE
%nonassoc NOTAND

*/

%%

