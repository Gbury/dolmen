
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {1 Alt-ergo Lexer} *)

{
  exception Error

  module T = Dolmen_std.Tok

  open Tokens_ae

  (* Token printing *)

  let reserved s =
    T.descr s
      ~kind:"reserved word"
      ~hint:"reserved words cannot be used as identifiers"

  let descr token : T.descr =
    match (token : token) with
    | ID s -> T.descr ~kind:"identifier" s
    | QM_ID s -> T.descr ~kind:"variable id" s
    | INTEGER s -> T.descr ~kind:"integer" s
    | DECIMAL s -> T.descr ~kind:"decimal" s
    | HEXADECIMAL s -> T.descr ~kind:"hexadecimal" s
    | STRING s -> T.descr ~kind:"string" s
    | MATCH -> reserved "match"
    | WITH -> reserved "with"
    | THEORY -> reserved "theory"
    | EXTENDS -> reserved "extends"
    | END -> reserved "end"
    | QM -> reserved "?"
    | AND -> reserved "and"
    | LEFTARROW -> reserved "<-"
                     (*
    | RIGHTARROW AC AT AXIOM CASESPLIT REWRITING
%token BAR HAT
%token BOOL COLON COMMA PV DISTINCT DOT SHARP ELSE OF EOF EQUAL
%token EXISTS FALSE VOID FORALL FUNC GE GOAL GT CHECK CUT
%token IF IN INT BITV MAPS_TO
%token LE LET LEFTPAR LEFTSQ LEFTBR LOGIC LRARROW XOR LT MINUS
%token NOT NOTEQ OR PERCENT PLUS PRED PROP
%token QUOTE REAL UNIT
%token RIGHTPAR RIGHTSQ RIGHTBR
%token SLASH POW POWDOT
%token THEN TIMES TRUE TYPE
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
*)
    | _ -> assert false

  (* Token parsing *)

  let escaped_char = function
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c -> c

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = (letter | '_') (letter | '_' | digit | '?' | '\'')*

let integer = digit+
let signed_integer = ['-' '+']? integer
let exp = ['e' 'E'] signed_integer
let real_exp = digit+ exp
let decimal = (digit+ '.' digit*) | (digit* '.' digit+)
let real_dec = decimal exp
let real = real_exp | real_dec
let hex = digit | ['a'-'f''A'-'F']
let hex_exp = ['p' 'P'] signed_integer
let real_hex = "0x" hex+ '.' hex* hex_exp

rule token newline = parse
  | '\n'                      { newline lexbuf; token newline lexbuf }
  | [' ' '\t' '\r']+          { token newline lexbuf }
  | '?'                       { QM }
  | '?' identifier as id      { QM_ID id }
  | identifier as i           { match i with
        | "ac"          -> AC
        | "and"         -> AND
        | "axiom"       -> AXIOM
        | "bitv"        -> BITV
        | "bool"        -> BOOL
        | "case_split"  -> CASESPLIT
        | "check"       -> CHECK
        | "cut"         -> CUT
        | "distinct"    -> DISTINCT
        | "else"        -> ELSE
        | "end"         -> END
        | "exists"      -> EXISTS
        | "extends"     -> EXTENDS
        | "false"       -> FALSE
        | "forall"      -> FORALL
        | "function"    -> FUNC
        | "goal"        -> GOAL
        | "if"          -> IF
        | "in"          -> IN
        | "int"         -> INT
        | "let"         -> LET
        | "logic"       -> LOGIC
        | "not"         -> NOT
        | "or"          -> OR
        | "xor"         -> XOR
        | "predicate"   -> PRED
        | "prop"        -> PROP
        | "real"        -> REAL
        | "rewriting"   -> REWRITING
        | "then"        -> THEN
        | "theory"      -> THEORY
        | "true"        -> TRUE
        | "type"        -> TYPE
        | "unit"        -> UNIT
        | "void"        -> VOID
        | "match"       -> MATCH
        | "with"        -> WITH
        | "of"          -> OF
        | _             -> ID i
      }
  | integer as s              { INTEGER s }
  | real as s                 { DECIMAL s }
  | real_hex as s             { HEXADECIMAL s }
  | "(*"                      { parse_comment newline lexbuf; token newline lexbuf }
  | "'"                       { QUOTE }
  | ","                       { COMMA }
  | ";"                       { PV }
  | "("                       { LEFTPAR }
  | ")"                       { RIGHTPAR }
  | ":"                       { COLON }
  | "->"                      { RIGHTARROW }
  | "<-"                      { LEFTARROW }
  | "<->"                     { LRARROW }
  | "="                       { EQUAL }
  | "<"                       { LT }
  | "<="                      { LE }
  | ">"                       { GT }
  | ">="                      { GE }
  | "<>"                      { NOTEQ }
  | "+"                       { PLUS }
  | "-"                       { MINUS }
  | "*"                       { TIMES }
  | "**."                     { POWDOT }
  | "**"                      { POW }
  | "/"                       { SLASH }
  | "%"                       { PERCENT }
  | "@"                       { AT }
  | "."                       { DOT }
  | "#"                       { SHARP }
  | "["                       { LEFTSQ }
  | "]"                       { RIGHTSQ }
  | "{"                       { LEFTBR }
  | "}"                       { RIGHTBR }
  | "|"                       { BAR }
  | "^"                       { HAT }
  | "|->"                     { MAPS_TO }
  | "\""                      { parse_string newline (Buffer.create 1024) lexbuf }
  | eof                       { EOF }
  | _ { raise Error }

and parse_comment newline = parse
  | "*)"    { () }
  | "(*"    { parse_comment newline lexbuf; parse_comment newline lexbuf }
  | eof     { raise Error }
  | _ as c  { if c = '\n' then newline lexbuf; parse_comment newline lexbuf }

and parse_string newline str_buf = parse
  | "\""          { STRING (Buffer.contents str_buf) }
  | "\\" (_ as c) { Buffer.add_char str_buf (escaped_char c);
                    parse_string newline str_buf lexbuf }
  | '\n'          { newline lexbuf;
                    Buffer.add_char str_buf '\n';
                    parse_string newline str_buf lexbuf }
  | eof           { raise Error }
  | _ as c        { Buffer.add_char str_buf c;
                    parse_string newline str_buf lexbuf }


