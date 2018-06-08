
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
  | '!'                 { ATTRIBUTE }
  | "as"                { AS }
  | "let"               { LET }
  | "exists"            { EXISTS }
  | "forall"            { FORALL }
  | "match"             { raise Error }
  | "par"               { PAR }

  | "NUMERAL"           { raise Error }
  | "DECIMAL"           { raise Error }
  | "STRING"            { raise Error }

  | "not"               { NOT }

  | "assert"            { ASSERT }
  | "check-sat-assuming"        { raise Error }
  | "check-sat"         { CHECK_SAT }
  | "declare-const"     { DECLARE_CONST }
  | "declare-datatypes" { DECLARE_DATATYPES }
  | "declare-datatype"  { DECLARE_DATATYPE }
  | "declare-fun"       { DECLARE_FUN }
  | "declare-heap"      { DECLARE_HEAP }
  | "declare-sort"      { DECLARE_SORT }
  | "define-fun-rec"    { DEFINE_FUN_REC }
  | "define-funs-rec"   { DEFINE_FUNS_REC }
  | "define-fun"        { DEFINE_FUN }
  | "define-sort"       { DEFINE_SORT }
  | "echo"              { raise Error }
  | "exit"              { EXIT }
  | "get-assertions"    { GET_ASSERTIONS }
  | "get-assignment"    { GET_ASSIGNMENT }
  | "get-info"          { GET_INFO }
  | "get-option"        { GET_OPTION }
  | "get-model"         { GET_MODEL }
  | "get-proof"         { GET_PROOF }
  | "get-unsat-assumptions"    { raise Error }
  | "get-unsat-core"    { GET_UNSAT_CORE }
  | "get-value"         { GET_VALUE }
  | "pop"               { POP }
  | "push"              { PUSH }
  | "reset-assertions"  { raise Error }
  | "reset"             { raise Error }
  | "set-info"          { SET_INFO }
  | "set-logic"         { SET_LOGIC }
  | "set-option"        { SET_OPTION }
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
