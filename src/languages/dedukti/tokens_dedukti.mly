
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Dedukti parser */

%token EOF
%token DOT
%token COMMA
%token COLON
%token ARROW
%token FATARROW
%token LONGARROW
%token DEF
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token WHNF
%token HNF
%token SNF
%token STEP
%token INFER
%token CONV
%token CHECK
%token PRINT
%token GDT
%token <string> OTHER
%token UNDERSCORE
%token <string>NAME
%token TYPE
%token <string*string> ID
%token <string> STRING

%%

