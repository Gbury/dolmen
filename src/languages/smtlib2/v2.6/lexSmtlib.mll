
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {1 Smtlib Lexer} *)

{
  open Tokens_smtlib

  exception Error

  module M = Map.Make(String)

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
    "define-funs-res", DEFINE_FUNS_REC;
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

  let symbol lexbuf s =
    (* register the newlines in quoted symbols to maintain correct locations.*)
    String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> ()) s;
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
let quoted_symbol = ['|'] quoted_symbol_char+ ['|']

let symbol = simple_symbol | quoted_symbol

let keyword = ':' simple_symbol

let comment = ';' (printable_char # ['\r' '\n'])*

rule token = parse
  (* Whitespace, newlines and comments *)
  | eof                 { EOF }
  | [' ' '\t' '\r']+    { token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | comment             { token lexbuf }

  (* SMTLIB tokens *)
  | '('                 { OPEN }
  | ')'                 { CLOSE }
  | numeral as s        { NUM s }
  | decimal as s        { DEC s }
  | hexadecimal as s    { HEX s }
  | binary as s         { BIN s }
  | '"'                 { string (Buffer.create 42) lexbuf }
  | keyword as s        { KEYWORD s }
  | symbol as s         { symbol lexbuf s }

and string b = parse
  | '"' '"'             { Buffer.add_char b '"'; string b lexbuf }
  | '"'                 { STR (Buffer.contents b) }
  | (printable_char | white_space_char) as c
    { if c = '\n' then Lexing.new_line lexbuf;
      Buffer.add_char b c; string b lexbuf }

