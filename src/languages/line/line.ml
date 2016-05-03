
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let rec consume lexbuf =
  match LexLine.token lexbuf with
  | LexLine.CHAR '\n' -> Lexing.new_line lexbuf
  | _ -> consume lexbuf

