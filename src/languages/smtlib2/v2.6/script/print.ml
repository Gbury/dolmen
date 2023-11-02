
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Printing of identifiers *)
(* ************************************************************************* *)

module L = Lexer

exception Cannot_print of string

let _cannot_print format =
  Format.kasprintf (fun msg -> raise (Cannot_print msg)) format

(* lexical definitions taken from the smtlib specification *)

let[@inline] is_whitespace c =
  let c = Char.code c in
  c = 9 (* tab *) || c = 10 (* line feed *) ||
  c = 13 (* cariage return *) || c = 32 (* space *)

let[@inline] is_printable c =
  let c = Char.code c in
  (32 <= c && c <= 126) || 128 <= c

let is_quoted_symbol_char c =
  (is_whitespace c || is_printable c) &&
  (c <> '|' && c <> '\\')

let[@inline] is_letter = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let[@inline] is_digit = function
  | '0'..'9' -> true
  | _ -> false

let[@inline] is_other_simple_symbol_chars = function
  | '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '_'
  | '-' | '+' | '=' | '<' | '>' | '.' | '?' | '/' -> true
  | _ -> false

let is_simple_symbol_char c =
  is_letter c || is_digit c || is_other_simple_symbol_chars c

(* symbol categorization *)

type symbol =
  | Simple
  | Quoted
  | Unprintable

let categorize_symbol s =
  match s with
  | "" -> Unprintable
  | "_" | "!" | "as" | "let"
  | "exists" | "forall"
  | "match" | "par"
  | "assert"
  | "check-sat"
  | "check-sat-assuming"
  | "declare-const"
  | "declare-datatype"
  | "declare-datatypes"
  | "declare-fun"
  | "declare-sort"
  | "define-fun"
  | "define-fun-rec"
  | "define-funs-rec"
  | "define-sort"
  | "echo"
  | "exit"
  | "get-assertions"
  | "get-assignment"
  | "get-info"
  | "get-model"
  | "get-option"
  | "get-proof"
  | "get-unsat-assumptions"
  | "get-unsat-core"
  | "get-value"
  | "pop"
  | "push"
  | "reset"
  | "reset-assertions"
  | "set-info"
  | "set-logic"
  | "set-option" -> Quoted
  | _  ->
    (* we are guaranteed that `s` is not the empty string *)
    if not (is_digit s.[0]) &&
       (Dolmen_std.Misc.string_for_all is_simple_symbol_char s) then
      Simple
    else if Dolmen_std.Misc.string_for_all is_quoted_symbol_char s then
      Quoted
    else
      Unprintable

let id fmt name =
  let aux fmt s =
    (* TODO: expose/add a cache to not redo the `categorize_symbol` computation each time *)
    match categorize_symbol s with
    | Simple -> Format.pp_print_string fmt s
    | Quoted -> Format.fprintf fmt "|%s|" s
    | Unprintable ->
      _cannot_print "symbol \"%s\" cannot be printed due to lexical constraints" s
  in
  match (name : Dolmen_std.Name.t) with
  | Simple s -> aux fmt s
  | Indexed { basename = _; indexes = [] } ->
    _cannot_print "indexed id with no indexes: %a" Dolmen_std.Name.print name
  | Indexed { basename; indexes; } ->
    let pp_sep fmt () = Format.fprintf fmt " " in
    Format.fprintf fmt "(_ %a %a)"
      aux basename (Format.pp_print_list ~pp_sep aux) indexes
  | Qualified _ ->
    _cannot_print "qualified identifier: %a" Dolmen_std.Name.print name

