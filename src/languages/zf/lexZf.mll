
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Lexer for Zipperposition Formulas} *)

{
  open Tokens_zf

  exception Error
}

let printable_char = [^ '\n']
let comment_line = '#' printable_char*

let numeric = ['0' - '9']
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*

(* let quoted = '"' ([^ '"'] | '\\' '"')* '"' *)

let zero_numeric = '0'
let non_zero_numeric = ['1' - '9']
let numeric = ['0' - '9']
let sign = ['+' '-']

let dot_decimal = '.' numeric +
let positive_decimal = non_zero_numeric numeric*
let decimal = zero_numeric | positive_decimal
let unsigned_integer = decimal
let signed_integer = sign unsigned_integer
let integer = signed_integer | unsigned_integer

rule token newline = parse
  | eof { EOF }
  | '\n' { newline lexbuf; token newline lexbuf }
  | [' ' '\t' '\r'] { token newline lexbuf }
  | comment_line { token newline lexbuf }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '.' { DOT }
  | ',' { COMMA }
  | '_' { WILDCARD }
  | ':' { COLON }
  | ';' { SEMI_COLON }
  | "=" { LOGIC_EQ }
  | "!=" { LOGIC_NEQ }
  | ":=" { EQDEF }
  | "->" { ARROW }
  | "val" { VAL }
  | "def" { DEF }
  | "where" { WHERE }
  | "type" { TYPE }
  | "prop" { PROP }
  | "int" { INT }
  | "assert" { ASSERT }
  | "lemma" { LEMMA }
  | "goal" { GOAL }
  | "and" { AND }
  | "rewrite" { REWRITE }
  | "true" { LOGIC_TRUE }
  | "false" { LOGIC_FALSE }
  | "pi" { PI }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "match" { MATCH }
  | "with" { WITH }
  | "end" { END }
  | "data" { DATA }
  | "fun" { FUN }
  | "&&" { LOGIC_AND }
  | "||" { LOGIC_OR }
  | "|" { VERTICAL_BAR }
  | "~" { LOGIC_NOT }
  | "*" { ARITH_PRODUCT }
  | "+" { ARITH_PLUS }
  | "-" { ARITH_MINUS }
  | "<" { ARITH_LT }
  | "<=" { ARITH_LEQ }
  | ">" { ARITH_GT }
  | ">=" { ARITH_GEQ }
  | "forall" { LOGIC_FORALL }
  | "exists" { LOGIC_EXISTS }
  | "=>" { LOGIC_IMPLY }
  | "<=>" { LOGIC_EQUIV }
  | "include" { INCLUDE }
  | lower_word { LOWER_WORD(Lexing.lexeme lexbuf) }
  | upper_word { UPPER_WORD(Lexing.lexeme lexbuf) }
  | integer { INTEGER(Lexing.lexeme lexbuf) }
  | '"' { quoted newline (Buffer.create 42) lexbuf }
  | _ { raise Error }

(* we unquote during lexing rather then during the parsing *)
and quoted newline b = parse
  | '"'       { QUOTED(Buffer.contents b) }
  | '\\' '"'  { Buffer.add_char b '"'; quoted newline b lexbuf }
  | _ as c    { if c = '\n' then newline lexbuf;
                Buffer.add_char b c; quoted newline b lexbuf }

