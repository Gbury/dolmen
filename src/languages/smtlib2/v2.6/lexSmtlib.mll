
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {1 Smtlib Lexer} *)

{
  exception Error

  module T = Dolmen_std.Tok
  module M = Map.Make(String)

  open Tokens_smtlib

  (* Token printing *)

  let keyword_descr s =
    T.descr s ~kind:"keyword"

  let reserved_descr s =
    T.descr s ~kind:"reserved word"

  let descr token : T.descr =
    match (token : token) with
    | EOF -> T.descr ~kind:"end of file token" ""
    | OPEN -> T.descr ~article:"an" ~kind:"opening partenthesis" ""
    | CLOSE -> T.descr ~article:"a" ~kind:"closing parenthesise" ""
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
    | PAR -> reserved_descr "par"
    | ASSERT -> reserved_descr "assert"
    | CHECK_SAT -> reserved_descr "check-sat"
    | CHECK_SAT_ASSUMING -> reserved_descr "check-sat-assuming"
    | DECLARE_CONST -> reserved_descr "declare-const"
    | DECLARE_DATATYPE -> reserved_descr "declare-datatype"
    | DECLARE_DATATYPES -> reserved_descr "declare-datatypes"
    | DECLARE_FUN -> reserved_descr "declare-fun"
    | DECLARE_SORT -> reserved_descr "declare-sort"
    | DEFINE_FUN -> reserved_descr "define-fun"
    | DEFINE_FUN_REC -> reserved_descr "define-fun-rec"
    | DEFINE_FUNS_REC -> reserved_descr "define-funs-rec"
    | DEFINE_SORT -> reserved_descr "define-sort"
    | ECHO -> reserved_descr "echo"
    | EXIT -> reserved_descr "exit"
    | GET_ASSERTIONS -> reserved_descr "get-assertions"
    | GET_ASSIGNMENT -> reserved_descr "gert-assignment"
    | GET_INFO -> reserved_descr "get-info"
    | GET_MODEL -> reserved_descr "get-model"
    | GET_OPTION -> reserved_descr "get-option"
    | GET_PROOF -> reserved_descr "get-proof"
    | GET_UNSAT_ASSUMPTIONS -> reserved_descr "get-unsat-assumptions"
    | GET_UNSAT_CORE -> reserved_descr "get-unsat-core"
    | GET_VALUE -> reserved_descr "get-value"
    | POP -> reserved_descr "pop"
    | PUSH -> reserved_descr "push"
    | RESET -> reserved_descr "reset"
    | RESET_ASSERTIONS -> reserved_descr "reset-assertions"
    | SET_INFO -> reserved_descr "set-info"
    | SET_LOGIC -> reserved_descr "set-logic"
    | SET_OPTION -> reserved_descr "set-option"

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
    "par", PAR;
    (* command names *)
    "assert", ASSERT;
    "check-sat", CHECK_SAT;
    "check-sat-assuming", CHECK_SAT_ASSUMING;
    "declare-const", DECLARE_CONST;
    "declare-datatype", DECLARE_DATATYPE;
    "declare-datatypes", DECLARE_DATATYPES;
    "declare-fun", DECLARE_FUN;
    "declare-sort", DECLARE_SORT;
    "define-fun", DEFINE_FUN;
    "define-fun-rec", DEFINE_FUN_REC;
    "define-funs-rec", DEFINE_FUNS_REC;
    "define-sort", DEFINE_SORT;
    "echo", ECHO;
    "exit", EXIT;
    "get-assertions", GET_ASSERTIONS;
    "get-assignment", GET_ASSIGNMENT;
    "get-info", GET_INFO;
    "get-model", GET_MODEL;
    "get-option", GET_OPTION;
    "get-proof", GET_PROOF;
    "get-unsat-assumptions", GET_UNSAT_ASSUMPTIONS;
    "get-unsat-core", GET_UNSAT_CORE;
    "get-value", GET_VALUE;
    "pop", POP;
    "push", PUSH;
    "reset", RESET;
    "reset-assertions", RESET_ASSERTIONS;
    "set-info", SET_INFO;
    "set-logic", SET_LOGIC;
    "set-option", SET_OPTION;
  ]

  let symbol newline lexbuf s =
    (* register the newlines in quoted symbols to maintain correct locations.*)
    String.iter (function '\n' -> newline lexbuf | _ -> ()) s;
    (* Check whetehr the symbol is a reserved word. *)
    try M.find s reserved_words
    with Not_found -> SYMBOL s

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
let quoted_symbol = ['|'] quoted_symbol_char* ['|']

let symbol = simple_symbol | quoted_symbol

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
  | symbol as s         { symbol newline lexbuf s }

and string newline b = parse
  | '"' '"'             { Buffer.add_char b '"'; string newline b lexbuf }
  | '"'                 { STR (Buffer.contents b) }
  | (printable_char | white_space_char) as c
    { if c = '\n' then newline lexbuf;
      Buffer.add_char b c; string newline b lexbuf }

