
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

{
  open Tokens_smtlib

  exception Error

  let iter_new_line s lexbuf =
    String.iter
      (fun c ->
       match c with
       | '\n' -> Lexing.new_line lexbuf
       | _ -> ()) s

  let escape s =
    let rec iter_escape s n =
      let l = String.length s in
      if n < l-1 then
        match s.[n], s.[n+1] with
        | '\\', '"' ->
            let s =
              String.concat "\"" [
                String.sub s 0 n;
                String.sub s (n+2) (l-(n+2))
            ] in
            iter_escape s n
        | '\\', '\\' ->
            let s =
              String.concat "\\" [
                String.sub s 0 n;
                String.sub s (n+2) (l-(n+2))
              ] in
            iter_escape s n
        | _, _ -> iter_escape s (n+1)
      else s in
        iter_escape s 0
}

rule token = parse
  | eof                 { EOF }
  | [' ' '\t' '\r']+    { token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | ';' (_ # '\n')*     { token lexbuf }
  | '_'                 { UNDERSCORE }
  | "as"                { AS }
  | "let"               { LET }
  | "forall"            { FORALL }
  | "exists"            { EXISTS }
  | '!'                 { ATTRIBUTE }
  | "par"               { raise Error }
  | "NUMERAL"           { raise Error }
  | "DECIMAL"           { raise Error }
  | "STRING"            { raise Error }
  | "set-logic"         { SET_LOGIC }
  | "set-option"        { SET_OPTION }
  | "set-info"          { SET_INFO }
  | "declare-sort"      { DECLARE_SORT }
  | "define-sort"       { DEFINE_SORT }
  | "declare-fun"       { DECLARE_FUN }
  | "define-fun"        { DEFINE_FUN }
  | "push"              { PUSH }
  | "pop"               { POP }
  | "assert"            { ASSERT }
  | "check-sat"         { CHECK_SAT }
  | "get-assertions"    { GET_ASSERTIONS }
  | "get-proof"         { GET_PROOF }
  | "get-unsat-core"    { GET_UNSAT_CORE }
  | "get-value"         { GET_VALUE }
  | "get-assignment"    { GET_ASSIGNMENT }
  | "get-option"        { GET_OPTION }
  | "get-info"          { GET_INFO }
  | "exit"              { EXIT }
  | '('                 { OPEN }
  | ')'                 { CLOSE }
  | ('0' | ['1'-'9'] ['0'-'9']*) as s
        { NUMERAL s }
  | ('0' | ['1'-'9'] ['0'-'9']*) '.' ['0'-'9']+ as s
        { DECIMAL s }
  | '#' 'x' ['0'-'9' 'A'-'F' 'a'-'f']+ as s 
        { HEXADECIMAL s }
  | '#' 'b' ['0' '1']+ as s
        { BINARY s }
  | '"' ((([' '-'~' '\t' '\r' '\n'] # ['\\' '"']) | ('\\' [' '-'~' '\t' '\r' '\n']))* as s) '"' 
        { iter_new_line s lexbuf; STRING (escape s) }
  | ['a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@'] ['0'-'9' 'a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']* as s
        { SYMBOL s }
  | '|' (([' '-'~' '\t' '\r' '\n'] # ['\\' '|'])* as s) '|'
        { iter_new_line s lexbuf; SYMBOL s }
  | ':' ['0'-'9' 'a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']+ as s
        { KEYWORD s }
  | _
        { raise Error }
