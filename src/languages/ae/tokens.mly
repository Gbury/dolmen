
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

/* Token declarations for Alt-Ergo parser */

%token <string> ID
%token <string> QM_ID
%token <string> INTEGER
%token <string> DECIMAL
%token <string> HEXADECIMAL
%token <string> STRING
%token MATCH WITH THEORY EXTENDS END QM
%token AND LEFTARROW RIGHTARROW AC AT AXIOM CASESPLIT REWRITING
%token BAR HAT
%token BOOL COLON COMMA PV DISTINCT DOT SHARP ELSE OF EOF EQUAL
%token EXISTS FALSE VOID FORALL FUNC GE GOAL CHECK_SAT GT CHECK CUT
%token IF IN INT BITV MAPS_TO
%token LE LET LEFTPAR LEFTSQ LEFTBR LOGIC LRARROW XOR LT MINUS
%token NOT NOTEQ OR PERCENT PLUS PRED PROP
%token QUOTE REAL UNIT
%token RIGHTPAR RIGHTSQ RIGHTBR
%token SLASH POW POWDOT
%token THEN TIMES TRUE TYPE

/* Precedences */

%nonassoc IN
%nonassoc prec_forall prec_exists
%right RIGHTARROW LRARROW XOR
%right OR
%right AND
%nonassoc prec_ite
%left prec_relation EQUAL NOTEQ LT LE GT GE
%left PLUS MINUS
%left TIMES SLASH PERCENT POW POWDOT AT
%nonassoc HAT
%nonassoc uminus
%nonassoc NOT
%right prec_named
%nonassoc CHECK CUT

%%

