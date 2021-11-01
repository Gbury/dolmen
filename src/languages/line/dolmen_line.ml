
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let rec consume ~newline ~sync lexbuf =
  match LexLine.token lexbuf with
  | LexLine.EOF -> sync lexbuf; ()
  | LexLine.CHAR '\n' -> newline lexbuf; Lexing.new_line lexbuf
  | _ -> consume ~newline ~sync lexbuf

