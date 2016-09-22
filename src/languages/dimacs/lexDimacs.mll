
(* This file is free software, part of dolmen. See file "LICENSE" for more details. *)

{
  open Tokens_dimacs

  exception Error
}

let zero_numeric = '0'
let numeric = ['0' - '9']
let non_zero_numeric = ['1' - '9']

let positive_number = non_zero_numeric numeric*
let negative_number = ['-'] positive_number
let number = positive_number | negative_number

let printable_char = [^ '\n']
let comment = ['c'] printable_char* ['\n']

rule token = parse
  | "c"             { comment lexbuf }
  | "p"             { P }
  | "cnf"           { CNF }
  | eof             { EOF }
  | zero_numeric    { ZERO }
  | [' ' '\t' '\r'] { token lexbuf }
  | number          { INT (int_of_string @@ Lexing.lexeme lexbuf) }
  | '\n'            { Lexing.new_line lexbuf; NEWLINE }
  | _               { raise Error }

and comment = parse
  | '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | printable_char { comment lexbuf }

