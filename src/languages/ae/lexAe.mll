
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {1 Alt-ergo Lexer} *)

{
  open Tokens_ae

  let escaped_char = function
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c -> c

  exception Error
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

rule token = parse
  | '\n'                      { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+          { token lexbuf }
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
  | "(*"                      { parse_comment lexbuf; token lexbuf }
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
  | "\""                      { parse_string (Buffer.create 1024) lexbuf }
  | eof                       { EOF }
  | _ { raise Error }

and parse_comment = parse
  | "*)"    { () }
  | "(*"    { parse_comment lexbuf; parse_comment lexbuf }
  | eof     { raise Error }
  | _ as c  { if c = '\n' then Lexing.new_line lexbuf;
              parse_comment lexbuf }

and parse_string str_buf = parse
  | "\""          { STRING (Buffer.contents str_buf) }
  | "\\" (_ as c) { Buffer.add_char str_buf (escaped_char c);
                    parse_string str_buf lexbuf }
  | '\n'          { Lexing.new_line lexbuf;
                    Buffer.add_char str_buf '\n';
                    parse_string str_buf lexbuf }
  | eof           { raise Error }
  | _ as c        { Buffer.add_char str_buf c;
                    parse_string str_buf lexbuf }


