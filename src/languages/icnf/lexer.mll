
(* This file is free software, part of dolmen. See file "LICENSE" for more details. *)

{
  exception Error

  module T = Dolmen_std.Tok

  open Tokens

  let descr token : T.descr =
    match (token : token) with
    | EOF -> T.descr ~kind:"end of file token" ""
    | NEWLINE -> T.descr ~kind:"newline character" ""
    | A -> T.descr ~kind:"keyword" "a"
    | P -> T.descr ~kind:"keyword" "p"
    | INCCNF -> T.descr ~kind:"keyword" "inccnf"
    | ZERO -> T.descr ~kind:"integer" "0"
    | INT i -> T.descr ~kind:"integer" (string_of_int i)

}

let zero_numeric = '0'
let numeric = ['0' - '9']
let non_zero_numeric = ['1' - '9']

let positive_number = non_zero_numeric numeric*
let negative_number = ['-'] positive_number
let number = positive_number | negative_number

let printable_char = [^ '\n']
let comment = ['c'] printable_char* ['\n']

rule token newline = parse
  | "c"             { comment newline lexbuf }
  | "p"             { P }
  | "a"             { A }
  | "inccnf"        { INCCNF }
  | eof             { EOF }
  | zero_numeric    { ZERO }
  | [' ' '\t' '\r'] { token newline lexbuf }
  | number          { INT (int_of_string @@ Lexing.lexeme lexbuf) }
  | '\n'            { newline lexbuf; NEWLINE }
  | _               { raise Error }

and comment newline = parse
  | '\n'            { newline lexbuf; token newline lexbuf }
  | printable_char  { comment newline lexbuf }
  | _               { raise Error }

