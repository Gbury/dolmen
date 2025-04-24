
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {1 Smtlib Lexer} *)

{
  exception Error

  module T = Dolmen_std.Tok
  module M = Map.Make(String)

  open Tokens

  (* Token printing *)

  let keyword_descr s =
    T.descr s ~kind:"keyword"

  let reserved_descr s =
    T.descr s ~kind:"reserved word"

  let descr token : T.descr =
    match (token : token) with
    | EOF -> T.descr ~kind:"end of file token" ""
    | OPEN -> T.descr ~article:"an" ~kind:"opening parenthesis" ""
    | CLOSE -> T.descr ~article:"a" ~kind:"closing parenthesis" ""
    | NUM s -> T.descr ~kind:"integer" s
    | DEC s -> T.descr ~kind:"decimal" s
    | HEX s -> T.descr ~kind:"hexadecimal" s
    | BIN s -> T.descr ~kind:"binary" s
    | STR s -> T.descr ~kind:"string" s
    | SYMBOL s -> T.descr ~kind:"symbol" s
    | KEYWORD s -> keyword_descr s
    | UNDERSCORE -> reserved_descr "_"
    | ATTRIBUTE -> reserved_descr "!"
    | AS -> reserved_descr "as"
    | LET -> reserved_descr "let"
    | EXISTS -> reserved_descr "exists"
    | FORALL -> reserved_descr "forall"
    | MATCH -> reserved_descr "match"
    (* | PAR -> reserved_descr "par" *)
    | SAT -> reserved_descr "sat"
    | UNSAT -> reserved_descr "unsat"
    | DEFINE_FUN -> reserved_descr "define-fun"
    | DEFINE_FUN_REC -> reserved_descr "define-fun-rec"
    | DEFINE_FUNS_REC -> reserved_descr "define-funs-rec"
    | MODEL -> reserved_descr "model"

  (* Token parsing *)

  let bind map (x, v) = M.add x v map

  let reserved_words =
    List.fold_left bind M.empty [
    (* reserved words *)
      (* These are currently unused in smtlib scripts commands
       * (they are only used in logic definitions), hence they are currently
       * ignored, given that only scripts are currently parsed.
    "BINARY", BINARY;
    "DECIMAL", DECIMAL;
    "HEXADECIMAL", HEXADECIMAL;
    "NUMERAL", NUMERAL;
    "STRING", STRING;
      *)
    "_", UNDERSCORE;
    "!", ATTRIBUTE;
    "as", AS;
    "let", LET;
    "exists", EXISTS;
    "forall", FORALL;
    "match", MATCH;
    (* "par", PAR; *)
    (* results *)
    "sat", SAT;
    "unsat", UNSAT;
    (* command names *)
    "define-fun", DEFINE_FUN;
    "define-fun-rec", DEFINE_FUN_REC;
    "define-funs-rec", DEFINE_FUNS_REC;
    "model", MODEL;
  ]

  let symbol newline lexbuf s =
    (* register the newlines in quoted symbols to maintain correct locations.*)
    for i = 0 to (String.length s - 1) do
      match s.[i] with
      | '\n' -> newline lexbuf
      | _ -> ()
    done;
    (* Check whether the symbol is a reserved word. *)
    try M.find s reserved_words
    with Not_found -> SYMBOL s

  let quoted_symbol newline lexbuf s =
    (* register the newlines in quoted symbols to maintain correct locations.*)
    for i = 0 to (String.length s - 1) do
      match s.[i] with
      | '\n' -> newline lexbuf
      | _ -> ()
    done;
    (* Quoted symbols allow to use reserved words as symbols *)
    SYMBOL s
}

let white_space_char = ['\t' '\n' '\r' ' ']
let printable_char = [' ' - '~' '\128' - '\255']
let white_space_or_printable = ['\t' '\n' '\r' ' ' - '~' '\128' - '\255']
let digit = ['0' - '9']
let letter = ['A' - 'Z' 'a' - 'z']

let numeral = '0' | (['1' - '9'] digit*)
let decimal = numeral '.' '0'* numeral

let hex = ['0' - '9'] | ['A' - 'F'] | ['a' - 'f']
let hexadecimal = "#x" hex+

let bin = ['0' '1']
let binary = "#b" bin+

let ss_first_char =
  letter | ['+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']
let ss_char = ss_first_char | digit
let simple_symbol = ss_first_char ss_char*

let quoted_symbol_char = (white_space_or_printable # ['|' '\\'])

let keyword = ':' simple_symbol

let comment = ';' (white_space_or_printable # ['\r' '\n'])*

rule token newline = parse
  (* Whitespace, newlines and comments *)
  | eof                 { EOF }
  | [' ' '\t' '\r']+    { token newline lexbuf }
  | '\n'                { newline lexbuf; token newline lexbuf }
  | comment             { token newline lexbuf }

  (* SMTLIB tokens *)
  | '('                 { OPEN }
  | ')'                 { CLOSE }
  | numeral as s        { NUM s }
  | decimal as s        { DEC s }
  | hexadecimal as s    { HEX s }
  | binary as s         { BIN s }
  | '"'                 { string newline (Buffer.create 42) lexbuf }
  | keyword as s        { KEYWORD s }
  | simple_symbol as s                  { symbol newline lexbuf s }
  | '|' (quoted_symbol_char* as s) '|'  { quoted_symbol newline lexbuf s }
  | _                                   { raise Error }

and string newline b = parse
  | '"' '"'             { Buffer.add_char b '"'; string newline b lexbuf }
  | '"'                 { STR (Buffer.contents b) }
  | (printable_char | white_space_char) as c
    { if c = '\n' then newline lexbuf;
      Buffer.add_char b c; string newline b lexbuf }
  | _                   { raise Error }

