
(* This file is free software, part of dolmen. See file "LICENSE" for more details. *)

{
  exception Error

  module T = Dolmen_std.Tok

  open Tokens

  let descr token : T.descr =
    match (token : token) with
    | EOF -> T.descr ~kind:"end of file token" ""
    | P -> T.descr ~kind:"keyword" "p"
    | CNF -> T.descr ~kind:"keyword" "cnf"
    | NEWLINE -> T.descr ~kind:"newline character" ""
    | ZERO -> T.descr ~kind:"integer" "0"
    | INT i -> T.descr ~kind:"integer" (string_of_int i)

}

let zero_numeric = '0'
let numeric = ['0' - '9']
let non_zero_numeric = ['1' - '9']

let positive_number = non_zero_numeric numeric*
let negative_number = ['-'] positive_number
let number = positive_number | negative_number

let any_char_except_newline = [^ '\n']

rule token newline = parse
  | "c"             { comment newline lexbuf }
  | "p"             { P }
  | "cnf"           { CNF }
  | eof             { EOF }
  | zero_numeric    { ZERO }
  | [' ' '\t' '\r'] { token newline lexbuf }
  | number          { INT (int_of_string @@ Lexing.lexeme lexbuf) }
  | '\n'            { newline lexbuf; NEWLINE }
  | _               { raise Error }

and comment newline = parse
  | eof   { EOF }
  | '\n'  { newline lexbuf; token newline lexbuf }
  | any_char_except_newline { comment newline lexbuf }

