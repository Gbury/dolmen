
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Printing of identifiers *)
(* ************************************************************************* *)

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
  | _ ->
    (* we are guaranteed that `s` is not the empty string *)
    if not (is_digit s.[0]) &&
       (Dolmen_std.Misc.string_for_all is_simple_symbol_char s) then
      Simple
    else if Dolmen_std.Misc.string_for_all is_quoted_symbol_char s then
      Quoted
    else
      Unprintable

let id_aux fmt s =
  (* TODO: expose/add a cache to not redo the `categorize_symbol` computation each time *)
  match categorize_symbol s with
  | Simple -> Format.pp_print_string fmt s
  | Quoted -> Format.fprintf fmt "|%s|" s
  | Unprintable ->
    _cannot_print "symbol \"%s\" cannot be printed due to lexical constraints" s

let id fmt name =
  match (name : Dolmen_std.Name.t) with
  | Simple s ->
    id_aux fmt s
  | Indexed { basename = _; indexes = [] } ->
    _cannot_print "indexed id with no indexes: %a" Dolmen_std.Name.print name
  | Indexed { basename; indexes; } ->
    let pp_sep fmt () = Format.fprintf fmt " " in
    Format.fprintf fmt "(_ %a %a)"
      id_aux basename (Format.pp_print_list ~pp_sep id_aux) indexes
  | Qualified _ ->
    _cannot_print "qualified identifier: %a" Dolmen_std.Name.print name


(* sanitization *)

let sanitize_aux _idx = function
  (* smtlib identifiers can be quoted, and quotable symbols are a strict
     superset of non-quoted symbols, so we only need to make sure that
     all characters are quotable *)
  | None -> [Uchar.of_char '_']
  | Some c ->
    if Uchar.is_char c &&
       is_quoted_symbol_char (Uchar.to_char c) then
      [c]
    else
      [Uchar.of_char '_']

let sanitize _id name =
  match (name : Dolmen_std.Name.t) with
  | Simple "" ->
    Dolmen_std.Name.simple "_"
  | Simple s ->
    let s' = Dolmen_std.Misc.string_unicode_map sanitize_aux s in
    (* avoid an allocation if the name has not changed *)
    if s' == s then name else Dolmen_std.Name.simple s'
  | Indexed { basename = basename; indexes; } ->
    let basename' = Dolmen_std.Misc.string_unicode_map sanitize_aux basename in
    let indexes' =
      Dolmen_std.Misc.list_map_sharing
      (Dolmen_std.Misc.string_unicode_map sanitize_aux) indexes
    in
    if basename == basename' && indexes = indexes'
    then name
    else Dolmen_std.Name.indexed basename' indexes'
  | Qualified _ ->
    (* TODO: proper error ? Related to how dolmen will handle translation
       between languages with multiple files / qualified includes... *)
    assert false



(* Printing of terms and statements *)
(* ************************************************************************* *)

module Make
    (V : Dolmen_intf.View.FO.S)
    (Env : Dolmen_intf.Env.Print
     with type name := Dolmen_std.Name.t
      and type ty_var := V.Ty.Var.t
      and type ty_cst := V.Ty.Cst.t
      and type term_var := V.Term.Var.t
      and type term_cst := V.Term.Cst.t)
= struct

  (* Ids *)
  (* *** *)

  let id _env fmt name = id fmt name

  (* Types *)
  (* ***** *)

  let ty_head_name env head =
    match (head : _ Dolmen_intf.View.FO.head) with
    | Cst c -> Env.Ty_cst.name env c
    | Builtin _ -> assert false

  let rec ty env fmt t =
    match V.Ty.view t with
    | Var v ->
      let name = Env.Ty_var.name env v in
      (* TODO: setup a cache from names to category in one of the env's keys *)
      id env fmt name
    | App (head, args) ->
      let f = ty_head_name env head in
      begin match args with
        | [] ->
          id env fmt f
        | _ :: _ ->
          Format.fprintf fmt "(%a %a)"
            (id env) f (ty_args env) args
      end

  and ty_args env fmt l =
    let pp_sep fmt () = Format.fprintf fmt " " in
    Format.pp_print_list ~pp_sep (ty env) fmt l


  (* Terms *)
  (* ***** *)


  (* Statements *)
  (* ********** *)

  let set_logic _env fmt s =
    Format.fprintf fmt "(set-logic %a)" id_aux s

  let pop _env fmt n =
    if n <= 0
    then raise (Cannot_print "pop with non-positive level")
    else Format.fprintf fmt "(pop %d)" n

  let push _env fmt n =
    if n <= 0
    then raise (Cannot_print "push with non-positive level")
    else Format.fprintf fmt "(push %d)" n

  let unit_stmt s (_env : Env.t) fmt () =
    Format.fprintf fmt "(%s)" s

  let reset = unit_stmt "reset"
  let reset_assertions = unit_stmt "reset-assertions"

  let get_unsat_core = unit_stmt "get-unsat-core"
  let get_unsat_assumptions = unit_stmt "get-unsat-assumptions"

  let get_proof = unit_stmt "get-proof"
  let get_model = unit_stmt "get-model"

  let get_assertions = unit_stmt "get-assertions"
  let get_assignment = unit_stmt "get-assignment"

  let exit = unit_stmt "exit"

  let declare_sort env fmt c =
    let n = V.Ty.Cst.arity c in
    let name = Env.Ty_cst.name env c in
    Format.fprintf fmt "(declare-sort %a %d)" (id env) name n

  let declare_fun env fmt c =
    let name = Env.Term_cst.name env c in
    let c_sig = V.Term.Cst.ty c in
    match V.Sig.view c_sig with
    | Signature ([], [], c_ty) ->
      Format.fprintf fmt "(declare-const %a %a)"
        (id env) name (ty env) c_ty
    | Signature ([], params, ret) ->
      Format.fprintf fmt "(declare-fun %a (%a) %a)"
        (id env) name (ty_args env) params (ty env) ret
    | Signature (_ :: _, _, _) ->
      (* TODO: proper error, polymorphic functions are not supported in smtlib v2.6 *)
      assert false

end

