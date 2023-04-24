
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {1 Alt-ergo Lexer} *)

{
  exception Error

  module T = Dolmen_std.Tok

  open Tokens

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
    | RIGHTARROW -> reserved "->"
    | AC -> reserved "ac"
    | AT -> reserved "@"
    | AXIOM -> reserved "axiom"
    | CASESPLIT -> reserved "case_split"
    | REWRITING -> reserved "rewriting"
    | BAR -> reserved "|"
    | HAT -> reserved "^"
    | BOOL -> reserved "bool"
    | COLON -> reserved ":"
    | COMMA -> reserved ","
    | PV -> reserved ";"
    | DISTINCT -> reserved "distinct"
    | DOT -> reserved "."
    | SHARP -> reserved "#"
    | ELSE -> reserved "else"
    | OF -> reserved "of"
    | EOF -> T.descr ~kind:"end of file token" ""
    | EQUAL -> reserved "equal"
    | EXISTS -> reserved "exists"
    | FALSE -> reserved "false"
    | VOID -> reserved "void"
    | FORALL ->reserved "forall"
    | FUNC -> reserved "function"
    | GE -> reserved ">="
    | GOAL -> reserved "goal"
    | CHECK_SAT -> reserved "check_sat"
    | GT -> reserved ">"
    | CHECK -> reserved "check"
    | CUT -> reserved "cut"
    | IF -> reserved "if"
    | IN -> reserved "in"
    | INT -> reserved "int"
    | BITV -> reserved "bitv"
    | MAPS_TO -> reserved "|->"
    | LE -> reserved "<="
    | LET -> reserved "let"
    | LEFTPAR -> reserved "("
    | LEFTSQ -> reserved "["
    | LEFTBR -> reserved "{"
    | LOGIC -> reserved "logic"
    | LRARROW -> reserved "<->"
    | XOR -> reserved "xor"
    | LT -> reserved "<"
    | MINUS -> reserved "-"
    | NOT -> reserved "not"
    | NOTEQ -> reserved "<>"
    | OR -> reserved "or"
    | PERCENT -> reserved "%"
    | PLUS -> reserved "+"
    | PRED -> reserved "predicate"
    | PROP -> reserved "prop"
    | QUOTE -> reserved "'"
    | REAL -> reserved "real"
    | UNIT -> reserved "unit"
    | RIGHTPAR -> reserved ")"
    | RIGHTSQ -> reserved "]"
    | RIGHTBR -> reserved "}"
    | SLASH -> reserved "/"
    | POW -> reserved "**"
    | POWDOT -> reserved "**."
    | THEN -> reserved "then"
    | TIMES -> reserved "*"
    | TRUE -> reserved "true"
    | TYPE -> reserved "type"

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
let real_dec = decimal exp?
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
        | "check_sat"   -> CHECK_SAT
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


