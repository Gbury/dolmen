{

(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

  type token =
    | EOF
    | CHAR of char
}

rule token = parse
  | eof { EOF }
  | _ as c { CHAR c }
