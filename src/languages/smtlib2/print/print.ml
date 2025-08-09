
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type version =
  | V2_6
  | V2_7
  | Poly

module type Config = sig
  type token
  val version : version
end

module type Lexer = sig

  type token

  module M : Map.S with type key = string

  val reserved_words : token M.t

  val check_num : Lexing.lexbuf -> bool

  val check_dec : Lexing.lexbuf -> bool

  val check_hex : Lexing.lexbuf -> bool

  val check_bin : Lexing.lexbuf -> bool

  val check_keyword : Lexing.lexbuf -> bool

  val check_simple_symbol : Lexing.lexbuf -> bool

  val check_quoted_symbol : Lexing.lexbuf -> bool

end

module Make(Config : Config)(Lexer : Lexer with type token := Config.token) = struct

  (* Printing of identifiers *)
  (* ************************************************************************* *)

  module Misc = Dolmen_std.Misc

  exception Cannot_print of string
  exception Polymorphic_function_definition
  exception Polymorphic_function_declaration

  (* TODO: structured errors *)
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

  let[@inline] is_quoted_symbol_char c =
    (is_whitespace c || is_printable c) &&
    (c <> '|' && c <> '\\')

  (* symbol categorization *)

  type symbol =
    | Simple
    | Quoted
    | Unprintable

  let categorize_symbol s =
    if String.length s = 0 (* s = "" *) then
      Unprintable
    else begin
      match Lexer.M.find_opt s Lexer.reserved_words with
      | Some _ -> Quoted
      | None ->
        if Misc.lex_string Lexer.check_simple_symbol s then
          Simple
        else if Misc.lex_string Lexer.check_quoted_symbol s then
          Quoted
        else
          Unprintable
    end

  let symbol_aux fmt s =
    (* TODO: expose/add a cache to not redo the `categorize_symbol` computation each time *)
    match categorize_symbol s with
    | Simple -> Format.pp_print_string fmt s
    | Quoted -> Format.fprintf fmt "|%s|" s
    | Unprintable ->
      _cannot_print {|symbol "%s" cannot be printed due to lexical constraints|} s

  let index fmt s =
    if Misc.lex_string Lexer.check_num s then
      Format.pp_print_string fmt s
    else
      symbol_aux fmt s

  let symbol fmt name =
    match (name : Dolmen_std.Name.t) with
    | Simple s ->
      symbol_aux fmt s
    | Indexed { basename = _; indexes = [] } ->
      _cannot_print "indexed id with no indexes: %a" Dolmen_std.Name.print name
    | Indexed { basename; indexes; } ->
      let pp_sep fmt () = Format.fprintf fmt " " in
      Format.fprintf fmt "(_ %a %a)"
        symbol_aux basename (Format.pp_print_list ~pp_sep index) indexes
    | Qualified _ ->
      _cannot_print "qualified identifier: %a" Dolmen_std.Name.print name

  let keyword fmt name =
    match (name : Dolmen_std.Name.t) with
    | Simple s when Misc.lex_string Lexer.check_keyword s ->
      Format.pp_print_string fmt s
    | _ -> _cannot_print "not a keyword"

  let num fmt s =
    if Misc.lex_string Lexer.check_num s then
      Format.pp_print_string fmt s
    else
      _cannot_print "num"

  let dec ~k fmt s =
    (* smtlib requires at least one digit before or after a `.` *)
    let s =
      if not (String.contains s '.') then
        s ^ ".0"
      else if String.length s >= 2 then
        if s.[0] = '.'
        then "0" ^ s
        else if s.[String.length s - 1] = '.'
        then s ^ "0"
        else s
      else
        s
    in
    if Misc.lex_string Lexer.check_dec s then
      Format.pp_print_string fmt s
    else
      k ~k:(fun () -> _cannot_print "dec") ()

  let hex fmt s =
    if Misc.lex_string Lexer.check_hex s then
      Format.pp_print_string fmt s
    else
      _cannot_print "hex"

  let bin fmt s =
    if Misc.lex_string Lexer.check_bin s then
      Format.pp_print_string fmt s
    else
      _cannot_print "bin"

  let string fmt s =
    let can_print = ref true in
    let quotation = ref 0 in
    String.iter (fun c ->
        if c = '"' then quotation := !quotation + 1;
        if not (is_whitespace c || is_printable c) then can_print := false
      ) s;
    if not !can_print then
      _cannot_print "string: \"%s\"" s
    else if !quotation = 0 then
      Format.fprintf fmt {|"%s"|} s
    else begin
      Format.pp_print_char fmt '"';
      String.iter (function
          | '"' -> Format.fprintf fmt {|""|}
          | c -> Format.fprintf fmt "%c" c) s;
      Format.pp_print_char fmt '"'
    end

  (* sanitization *)

  let sanitize_aux _idx o =
    (* smtlib identifiers can be quoted, and quotable symbols are a strict
       superset of non-quoted symbols, so we only need to make sure that
       all characters are quotable *)
    match o with
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
      _cannot_print "qualified names in smtlib2"


  (* Printing of terms and statements *)
  (* ************************************************************************* *)

  module type S = Dolmen_intf.Print.Smtlib2

  module Make
      (Env : Dolmen_intf.Env.Print
       with type name := Dolmen_std.Name.t)
      (S : Dolmen_intf.View.Sexpr.S
       with type id := Dolmen_std.Id.t)
      (V : Dolmen_intf.View.TFF.S
       with type ty = Env.ty
        and type ty_var = Env.ty_var
        and type ty_cst = Env.ty_cst
        and type term = Env.term
        and type term_var = Env.term_var
        and type term_cst = Env.term_cst)
  = struct

    module I = Dolmen_std.Id
    module N = Dolmen_std.Name
    module B = Dolmen_std.Builtin
    module F = Dolmen_intf.View.TFF
    module E = Dolmen_std.View.Assoc(V)
    module H = Hashtbl.Make(V.Term.Cst)


    (* Env suff *)
    (* ******** *)

    (* splitting of decimals *)
    let split_dec : (string -> ([ `Pos | `Neg ] * string * string) option) Env.key = Env.key ()

    let set_split_dec env f = Env.set env split_dec f

    (* ":named" stuff *)
    let named_csts_key : V.Term.t H.t Env.key = Env.key ()

    let add_named env cst defining_expr =
      let env, h =
        match Env.get env named_csts_key with
        | Some h -> env, h
        | None ->
          let h = H.create 13 in
          let env = Env.set env named_csts_key h in
          env, h
      in
      assert (not (H.mem h cst));
      H.add h cst defining_expr;
      env

    let find_named env cst =
      match Env.get env named_csts_key with
      | None -> None
      | Some h ->
        begin match H.find h cst with
          | x ->
            H.remove h cst;
            Some x
          | exception Not_found -> None
        end

    (* Env suff *)
    (* ******** *)

    (* Applications of `to_real` that are **directly** under an arithmetic
       operator (such as '+'), can omit to print applications of `to_real`,
       since these will be added back when parsing/typing. *)
    let can_omit_to_real_key : bool Env.key = Env.key ()
    let set_omit_to_real env b =
      match Env.get env can_omit_to_real_key with
      | Some b' when b = b' -> env
      | _ -> Env.set env can_omit_to_real_key b
    let can_omit_to_real env =
      match Env.get env can_omit_to_real_key with
      | Some true -> true
      | _ -> false


    (* Helpers *)
    (* ******* *)

    let list pp env fmt l =
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.pp_print_list ~pp_sep (pp env) fmt l

    let symbol _env fmt name = symbol fmt name

    let id ~allow_keyword env fmt id =
      match (id : Dolmen_std.Id.t) with
      | { ns = Value String; name = Simple s; } -> string fmt s
      | { ns = Value Integer; name = Simple s; } -> num fmt s
      | { ns = Value Real; name = Simple s; } ->
        dec fmt s ~k:(fun ~k () ->
            match Env.get env split_dec with
            | None -> k ()
            | Some f ->
              begin match f s with
                | None -> k ()
                | Some (`Pos, numerator, denominator) ->
                  Format.fprintf fmt "(/ %a %a)" num numerator num denominator
                | Some (`Neg, numerator, denominator) ->
                  Format.fprintf fmt "(/ (- %a) %a)" num numerator num denominator
              end)
      | { ns = Value Hexadecimal; name = Simple s; } -> hex fmt s
      | { ns = Value Binary; name = Simple s; } -> bin fmt s
      | { ns = (Attr | Term); name = Simple s; } ->
        if (allow_keyword && Misc.lex_string Lexer.check_keyword s)
        || Misc.lex_string Lexer.check_simple_symbol s then (
          Format.pp_print_string fmt s
        ) else if Misc.lex_string Lexer.check_quoted_symbol s then
          Format.fprintf fmt "|%s|" s
        else
          _cannot_print "unprintable symbol"
      | { ns = Term; name; } ->
        symbol env fmt name
      | _ ->
        _cannot_print "unprintable id"

    let app ?(allow_keyword=false) ~pp env fmt (f, args) =
      match args with
      | [] ->
        Format.fprintf fmt "%a" (id ~allow_keyword env) f
      | _ :: _ ->
        Format.fprintf fmt "@[<hov 1>(%a@ %a)@]"
          (id ~allow_keyword:false env) f
          (list pp env) args

    let rec app_deep ?allow_keyword ~pp env fmt = function
      | `Term t -> pp env fmt t
      | `App_term (f, args) ->
        app ?allow_keyword ~pp env fmt (f, args)
      | `App (f, args) ->
        app ?allow_keyword ~pp:(app_deep ?allow_keyword ~pp) env fmt (f, args)


    (* Types *)
    (* ***** *)

    let ty_var env fmt v =
      let name = Env.Ty_var.name env v in
      (* TODO: setup a cache from names to category in one of the env's keys *)
      symbol env fmt name

    let ty_cst env fmt c =
      let name = Env.Ty_cst.name env c in
      (* TODO: setup a cache from names to category in one of the env's keys *)
      symbol env fmt name

    let ty_head_name env c =
      (* TODO: add reservations for the builtin names in the env *)
      let int = string_of_int in
      match V.Ty.Cst.builtin c with
      | B.Base | B.Univ -> Env.Ty_cst.name env c
      | B.Prop T -> N.simple "Bool"
      | B.Arith Int -> N.simple "Int"
      | B.Arith Real -> N.simple "Real"
      | B.Array T -> N.simple "Array"
      | B.Bitv T {n} -> N.indexed "Bitvec" [int n]
      | B.Float T { e = 5; s = 11; } -> N.simple "Float16"
      | B.Float T { e = 8; s = 24; } -> N.simple "Float32"
      | B.Float T { e = 11; s = 53; } -> N.simple "Float64"
      | B.Float T { e = 15; s = 113; } -> N.simple "Float128"
      | B.Float T { e; s; } -> N.indexed "FloatingPoint" [int e; int s]
      | B.Float RoundingMode -> N.simple "RoundingMode"
      | B.Str T -> N.simple "String"
      | B.Regexp T -> N.simple "RegLan"
      | _ ->
        (* Fallback: some builtins may be explicitly defined (e.g. unit) *)
        Env.Ty_cst.name env c

    let rec ty env fmt t =
      (* Here, since we want to print things, we do not expand types,
         as we prefer the abstract presentation (which is closer to
         the source input). *)
      match V.Ty.view ~expand:false t with
      | Var v -> ty_var env fmt v
      | App (head, args) ->
        let f = ty_head_name env head in
        begin match args with
          | [] ->
            symbol env fmt f
          | _ :: _ ->
            Format.fprintf fmt "(%a %a)"
              (symbol env) f (list ty env) args
        end


    (* Terms *)
    (* ***** *)

    let term_var env fmt v =
      let name = Env.Term_var.name env v in
      (* TODO: setup a cache from names to category in one of the env's keys *)
      symbol env fmt name

    let term_cst env fmt c =
      let name = Env.Term_cst.name env c in
      (* TODO: setup a cache from names to category in one of the env's keys *)
      symbol env fmt name

    let sorted_var env fmt v =
      Format.fprintf fmt "(%a %a)" (term_var env) v (ty env) (V.Term.Var.ty v)

    let add_binding_to_env env (v, _) =
      Env.Term_var.bind env v

    let add_pattern_to_env env pat =
      match (pat : _ F.Term.pattern) with
      | Var v -> Env.Term_var.bind env v
      | Constructor (_, l) -> List.fold_left Env.Term_var.bind env l

    let extract_int_lit t =
      match V.Term.view t with
      | App (head, [], []) ->
        begin match V.Term.Cst.builtin head with
          | B.Arith Integer s -> Some s
          | _ -> None
        end
      | _ -> None

    let term_cst_chainable _env c =
      (* WARNING: this `blt` function should only be called with builtins that
         do not have payload (such as terms), since the polymorphic comparison
         will not work adequately in these cases. *)
      let blt b = fun c -> V.Term.Cst.builtin c = b in
      let b = V.Term.Cst.builtin c in
      let yup () = `Chainable (blt b) in
      match b with
      | B.Equal
      | B.Arith Lt (`Int | `Real) | B.Arith Leq (`Int | `Real)
      | B.Arith Gt (`Int | `Real) | B.Arith Geq (`Int | `Real)
        -> yup ()
      | _ -> `Nope

    let term_cst_assoc _env c =
      (* WARNING: this `blt` function should only be called with builtins that
         do not have payload (such as terms), since the polymorphic comparison
         will not work adequately in these cases. *)
      let blt b = fun c -> V.Term.Cst.builtin c = b in
      let b = V.Term.Cst.builtin c in
      let left () = `Left_assoc (blt b) in
      let right () = `Right_assoc (blt b) in
      match b with
      (* left associative builtins *)
      | B.Prop (Or | And | Xor)
      | B.Arith Add (`Int | `Real)
      | B.Arith Sub (`Int | `Real)
      | B.Arith Mul (`Int | `Real)
      | B.Arith Div `Real
        -> left ()
      (* Right associative builtins *)
      | B.Prop Imply
        -> right ()
      (* all others are non-associative *)
      | _ -> `None

    let term_cst_poly _env c =
      (* Here, we want the actual concrete type, since this matters for
         actual typing afterwards, that begin said, expansion of type aliases
         should not introduce polymorphism in principle. *)
      match V.Sig.view ~expand:true (V.Term.Cst.ty c) with
      | Signature (_ :: _, _, _) -> true
      | _ -> false

    (* smtlib has implicit type arguments, i.e. the type args are not printed.
       Therefore, whenever a polymorphic symbol is used, its type arguments
       need to be inferable from its term arguments. Hence, when a symbol is
       polymorphic and there are no term arguments, we need to print an
       explicit type annotation to disambiguate things. In the other cases,
       we suppose that a symbol's type arguments can be deduced from the term
       arguments. *)
    let term_app_aux ~poly ~pp ~env fmt (head, args) =
      match args with
      | [] ->
        begin match poly () with
          | Some t_ty ->
            Format.fprintf fmt "@[<hov 1>(as@ %a@ %a)@]"
              (id ~allow_keyword:false env) head (ty env) t_ty
          | None -> app ~pp env fmt (head, args)
        end
      | _ :: _ -> app ~pp env fmt (head, args)

    (* Note: unfortunately, most of the smtlib term constructions end with a
       parenthesis, and therefore their printers are currently not tail-rec.
       This is particularly true for sequential let-bindings.

       This means that the current printers are likely to produce a stack
       overflow if used on extremely big/deep terms (or with a lot of sequential
       let bindings). We might want to consider ocaml 5 and lifting the stack
       limit for these cases, or switch to a CPS-style for printers. *)
    let rec term env fmt t =
      term_view env fmt (V.Term.ty t) (V.Term.view t)

    and term_view env fmt t_ty view =
      match (view : _ F.Term.view) with
      | Var v ->
        term_var env fmt v
      | App (head, ty_args, args) ->
        term_app env fmt (t_ty, head, ty_args, args)
      | Match (scrutinee, cases) ->
        term_match env fmt (scrutinee, cases)
      | Binder (Exists { type_vars; term_vars; triggers; }, body) ->
        exists env fmt type_vars term_vars triggers body
      | Binder (Forall { type_vars; term_vars; triggers; }, body) ->
        forall env fmt type_vars term_vars triggers body
      | Binder (Letand l, body) ->
        letand env fmt (l, body)
      | Binder (Letin l, body) ->
        letin env fmt (l, body)
      | Binder (Map vars, body) ->
        begin match Config.version with
          | V2_6 | Poly -> _cannot_print "lambda"
          | V2_7 -> quant "lambda" env fmt (vars, [], body)
        end

    and term_app env fmt (t_ty, head, ty_args, args) =
      (* first, we need to undo any left/right associativity/chainability that
         may have been expanded by the typechecker or other mechanism. *)
      let head, ty_args, args =
        let ty_args, args =
          match term_cst_assoc env head with
          | `Left_assoc top_head -> None, E.left_assoc top_head args
          | `Right_assoc top_head -> None, E.right_assoc top_head args
          | `None -> Some ty_args, args
        in
        match V.Term.Cst.builtin head, args with
        | B.Prop And, t :: _ ->
          begin match V.Term.view t with
            | App (h, _, _) ->
              begin match term_cst_chainable env h with
                | `Chainable top_head ->
                  begin match E.chainable top_head args with
                    | Some new_args -> h, None, new_args
                    | None -> head, ty_args, args
                  end
                | `Nope -> head, ty_args, args
              end
            | _ -> head, ty_args, args
          end
        | _ -> head, ty_args, args
      in

      (* small helpers *)
      let int = string_of_int in
      let aux ?(poly=fun _ -> None) ?(omit_to_real=false) head args =
        let env = set_omit_to_real env omit_to_real in
        term_app_aux ~poly ~pp:term ~env fmt (head, args)
      in
      let p ?poly ?omit_to_real ns name =
        aux ?poly ?omit_to_real (Dolmen_std.Id.create ns name) args
      in
      let simple ?poly ?omit_to_real s =
        p ?poly ?omit_to_real Term (N.simple s)
      in

      (* Matching *)
      match V.Term.Cst.builtin head with

      (* Algebraic datatypes *)
      | B.Adt Constructor _ | B.Adt Destructor _ ->
        let poly () = if term_cst_poly env head then Some t_ty else None in
        p ~poly Term (Env.Term_cst.name env head)
      | B.Adt Tester { cstr; _ } ->
        begin match Env.Term_cst.name env cstr with
          | Simple s -> p Term (N.indexed "is" [s])
          | _ -> _cannot_print "expected a simple for a constructor name"
        end

      (* Cast *)
      | B.Coercion ->
        begin match ty_args with
          | None -> assert false (* coercions should not be chainable/associative *)
          | Some [a; b] ->
            begin match V.Ty.view ~expand:true a, V.Ty.view ~expand:true b with

              (* Int-> Real conversion *)
              | App (ah, []), App (bh, [])
                when (match V.Ty.Cst.builtin ah with B.Arith Int -> true | _ -> false) &&
                     (match V.Ty.Cst.builtin bh with B.Arith Real -> true | _ -> false) ->
                begin match args with
                  | [t] when can_omit_to_real env -> term env fmt t
                  | [t] ->
                    if can_omit_to_real env then
                      term env fmt t
                    else
                      begin match extract_int_lit t with
                        | Some n -> aux (Dolmen_std.Id.create (Value Real) (N.simple n)) []
                        | None -> simple "to_real"
                      end
                  | _ -> simple "to_real"
                end

              (* fallback *)
              | _ -> _cannot_print "unhandled builtin"
            end
          | Some _ -> _cannot_print "bad coercion application"
        end

      (* Boolean core *)
      | B.Equal -> simple "="
      | B.Distinct -> simple "distinct"
      | B.Prop blt ->
        begin match blt with
          | T -> assert false (* cannot occur in terms *)
          | True -> simple "true"
          | False -> simple "false"
          | Neg -> simple "not"
          | And -> simple "and"
          | Or -> simple "or"
          | Nor ->
            app_deep ~pp:term env fmt
              (`App (I.create Term (N.simple "not"), [
                   `App_term (I.create Term (N.simple "or"), args)]))
          | Nand ->
            app_deep ~pp:term env fmt
              (`App (I.create Term (N.simple "not"), [
                   `App_term (I.create Term (N.simple "and"), args)]))
          | Xor -> simple "xor"
          | Imply -> simple "=>"
          | Implied ->
            (* just swap the arguments *)
            aux (Dolmen_std.Id.create Term (Dolmen_std.Name.simple "=>")) (List.rev args)
          | Ite -> simple "ite"
          | Equiv -> simple "="
        end

      (* Arrays *)
      | B.Array blt ->
        begin match blt with
          | T -> assert false (* cannot occur in terms *)
          | Const -> simple "const" (* TODO: this is not in the standard technically *)
          | Store -> simple "store"
          | Select -> simple "select"
        end

      (* Arithmetic *)
      | B.Arith blt ->
        begin match blt with
          | Int | Rat | Real -> assert false (* cannot occur in terms *)
          | Integer s -> p (Value Integer) (N.simple s)
          | Decimal s -> p (Value Real) (N.simple s)
          | Lt (`Int | `Real) -> simple ~omit_to_real:true "<"
          | Leq (`Int | `Real) -> simple ~omit_to_real:true "<="
          | Gt (`Int | `Real) -> simple ~omit_to_real:true ">"
          | Geq (`Int | `Real) -> simple ~omit_to_real:true ">="
          | Minus ( `Int | `Real) -> simple "-"
          | Add (`Int | `Real) -> simple ~omit_to_real:true "+"
          | Sub (`Int | `Real) -> simple ~omit_to_real:true "-"
          | Mul (`Int | `Real) -> simple ~omit_to_real:true "*"
          | Div `Real -> simple ~omit_to_real:true "/"
          | Div_e `Int -> simple "div"
          | Modulo_e `Int -> simple "mod"
          | Abs -> simple "abs"
          | Is_int `Real -> simple "is_int"
          | Floor_to_int `Real -> simple "to_int"
          | Divisible ->
            begin match args with
              | [x; y] ->
                begin match V.Term.view y with
                  | App (f, [], []) ->
                    begin match V.Term.Cst.builtin f with
                      | B.Arith Integer s ->
                        let id = Dolmen_std.Id.create Term (N.indexed "divisible" [s]) in
                        aux id [x]
                      | _ -> _cannot_print "bad divisible application"
                    end
                  | _ -> _cannot_print "bad divisible application"
                end
              | _ -> _cannot_print "bad divisible application"
            end
          (* Unsuported builtins *)
          | Div_e `Real | Modulo_e `Real | Is_int `Int
          | Pow _ | Div_t _ | Modulo_t _ | Div_f _ | Modulo_f _
          | Is_rat _ | Floor _ | Ceiling _
          | Truncate _ | Round _ ->
            _cannot_print "unsupported arithmetic builtin"

          (* rational arithmetic *)
          | Rational _
          | Lt `Rat | Leq `Rat | Gt `Rat | Geq `Rat
          | Minus `Rat | Add `Rat | Sub `Rat | Mul `Rat
          | Div `Rat | Div_e `Rat | Modulo_e `Rat
          | Is_int `Rat | Floor_to_int `Rat
            ->
            _cannot_print "rational arithmetic"
        end

      (* Bitvectors *)
      | B.Bitv blt ->
        begin match blt with
          | T _ -> assert false (* cannot occur in terms *)
          | Binary_lit s ->
            p (Value Binary) (N.simple ("#b" ^ s)) (* TODO: see if we can recover hex form ? *)
          | Not _ -> simple "bvnot"
          | And _ -> simple "bvand"
          | Or _ -> simple "bvor"
          | Nand _ -> simple "bvnand"
          | Nor _ -> simple "bvnor"
          | Xor _ -> simple "bvxor"
          | Xnor _ -> simple "bvxnor"
          | Comp _ -> simple "bvcomp"
          | Neg _ -> simple "bvneg"
          | Add _ -> simple "bvadd"
          | Sub _ -> simple "bvsub"
          | Mul _ -> simple "bvmul"
          | Udiv _ -> simple "bvudiv"
          | Urem _ -> simple "bvurem"
          | Sdiv _ -> simple "bvsdiv"
          | Srem _ -> simple "bvsrem"
          | Smod _ -> simple "bvsmod"
          | Shl _ -> simple "bvshl"
          | Lshr _ -> simple "bvlshr"
          | Ashr _ -> simple "bvashr"
          | Ult _ -> simple "bvult"
          | Ule _ -> simple "bvule"
          | Ugt _ -> simple "bvugt"
          | Uge _ -> simple "bvuge"
          | Slt _ -> simple "bvslt"
          | Sle _ -> simple "bvsle"
          | Sgt _ -> simple "bvsgt"
          | Sge _ -> simple "bvsge"
          | Concat _ -> simple "concat"
          | Repeat { n = _; k; } -> p Term (N.indexed "repeat" [int k])
          | Zero_extend { n = _; k; } -> p Term (N.indexed "zero_extend" [int k])
          | Sign_extend { n = _; k; } -> p Term (N.indexed "sign_extend" [int k])
          | Rotate_right { n = _; i; } -> p Term (N.indexed "rotate_right" [int i])
          | Rotate_left { n = _; i; } -> p Term (N.indexed "rotate_left" [int i])
          | Extract { n = _; i; j; } -> p Term (N.indexed "extract" [int i; int j])

          | Overflow_neg { n = _; } -> simple "bvnego"
          | Overflow_div { n = _; } -> simple "bvsdivo"
          | Overflow_add { n = _; signed = true; } -> simple "bvsaddo" (* 2.7 *)
          | Overflow_add { n = _; signed = false; } -> simple "bvuaddo" (* 2.7 *)
          | Overflow_sub { n = _; signed = true; } -> simple "bvssubo" (* 2.7 *)
          | Overflow_sub { n = _; signed = false; } -> simple "bvusubo" (* 2.7 *)
          | Overflow_mul { n = _; signed = true; } -> simple "bvsmulo" (* 2.7 *)
          | Overflow_mul { n = _; signed = false; } -> simple "bvumulo" (* 2.7 *)

          | To_int { n = _; signed = true; } ->
            begin match Config.version with
              | V2_7 -> simple "sbv_to_int"
              | V2_6 | Poly -> _cannot_print "sbv_to_int"
            end
          | To_int { n = _; signed = false; } ->
            begin match Config.version with
              | V2_7 -> simple "ubv_to_int"
              (* bvconv extension
                 TODO: use a flag to enable extensions such as this one ? *)
              | V2_6 | Poly -> simple "bv2nat" (* 2.7: "sbv_to_int" *)
            end
          | Of_int { n } ->
            begin match Config.version with
              | V2_6 | Poly -> p Term (N.indexed "int2bv" [int n])
              | V2_7 -> p Term (N.indexed "int_to_bv" [int n])
            end
        end

      (* Floats *)
      | B.Float blt ->
        begin match blt with
          | T _ | RoundingMode -> assert false (* cannot occur in terms *)
          | Fp _ -> simple "fp"
          | RoundNearestTiesToEven -> simple "RNE"
          | RoundNearestTiesToAway -> simple "RNA"
          | RoundTowardPositive -> simple "RTP"
          | RoundTowardNegative -> simple "RTN"
          | RoundTowardZero -> simple "RTZ"
          | Abs _ -> simple "fp.abs"
          | Neg _ -> simple "fp.neg"
          | Add _ -> simple "fp.add"
          | Sub _ -> simple "fp.sub"
          | Mul _ -> simple "fp.mul"
          | Div _ -> simple "fp.div"
          | Fma _ -> simple "fp.fma"
          | Sqrt _ -> simple "fp.sqrt"
          | Rem _ -> simple "fp.rem"
          | RoundToIntegral _ -> simple "fp.roundToIntegral"
          | Min _ -> simple "fp.min"
          | Max _ -> simple "fp.max"
          | Leq _ -> simple "fp.leq"
          | Lt _ -> simple "fp.lt"
          | Geq _ -> simple "fp.geq"
          | Gt _ -> simple "fp.gt"
          | Eq _ -> simple "fp.eq"
          | IsNormal _ -> simple "fp.isNormal"
          | IsSubnormal _ -> simple "fp.isSubnormal"
          | IsZero _ -> simple "fp.isZero"
          | IsInfinite _ -> simple "fp.isInfinite"
          | IsNaN _ -> simple "fp.isNaN"
          | IsNegative _ -> simple "fp.isNegative"
          | IsPositive _ -> simple "fp.isPositive"
          | To_real _ -> simple "fp.to_real"
          | Plus_infinity { e; s } -> p Term (N.indexed "+oo" [int e; int s])
          | Minus_infinity { e; s } -> p Term (N.indexed "-oo" [int e; int s])
          | Plus_zero { e; s } -> p Term (N.indexed "+zero" [int e; int s])
          | Minus_zero { e; s } -> p Term (N.indexed "-zero" [int e; int s])
          | NaN { e; s } -> p Term (N.indexed "NaN" [int e; int s])
          | Ieee_format_to_fp { e; s } -> p Term (N.indexed "to_fp" [int e; int s])
          | To_fp { e1 = _; s1 = _; e2 = e; s2 = s; } ->
            p Term (N.indexed "to_fp" [int e; int s])
          | Of_real { e; s } -> p Term (N.indexed "to_fp" [int e; int s])
          | Of_sbv { m = _; e; s; } -> p Term (N.indexed "to_fp" [int e; int s])
          | Of_ubv { m = _; e; s; } -> p Term (N.indexed "to_fp_unsigned" [int e; int s])
          | To_ubv { m; e = _; s = _; } -> p Term (N.indexed "fp.to_ubv" [int m])
          | To_sbv { m; e = _; s = _; } -> p Term (N.indexed "fp.to_sbv" [int m])
        end

      (* Strings *)
      | B.Str blt ->
        begin match blt with
          | T -> assert false (* cannot occur in terms *)
          | Raw s -> p (Value String) (N.simple s)
          | Length -> simple "str.len"
          | At -> simple "str.at"
          | To_code -> simple "str.to_code"
          | Of_code -> simple "str.from_code"
          | Is_digit -> simple "str.is_digit"
          | To_int -> simple "str.to_int"
          | Of_int -> simple "str.from_int"
          | Concat -> simple "str.++"
          | Sub -> simple "str.substr"
          | Index_of -> simple "str.indexof"
          | Replace -> simple "str.replace"
          | Replace_all -> simple "str.replace_all"
          | Replace_re -> simple "str.replace_re"
          | Replace_re_all -> simple "str.replace_re_all"
          | Is_prefix -> simple "str.prefixof"
          | Is_suffix -> simple "str.suffixof"
          | Contains -> simple "str.contains"
          | Lexicographic_strict -> simple "str.<"
          | Lexicographic_large -> simple "str.<="
          | In_re -> simple "str.in_re"
        end

      | B.Regexp blt ->
        begin match blt with
          | T -> assert false (* cannot occur in terms *)
          | Empty -> simple "re.none"
          | All -> simple "re.all"
          | Allchar -> simple "r.allchar"
          | Of_string -> simple "str.to_re"
          | Range -> simple "re.range"
          | Concat -> simple "re.++"
          | Union -> simple "re.union"
          | Inter -> simple "re.inter"
          | Star -> simple "re.*"
          | Cross -> simple "re.+"
          | Complement -> simple "re.comp"
          | Diff -> simple "re.diff"
          | Option -> simple "re.opt"
          | Power n -> p Term (N.indexed "re.^" [int n])
          | Loop (n1, n2) -> p Term (N.indexed "re.loop" [int n1; int n2])
        end

      (* generic case + fallback *)
      | B.Base | _ ->
        begin match find_named env head with
          | None ->
            let poly () = if term_cst_poly env head then Some t_ty else None in
            let h = Env.Term_cst.name env head in
            p ~poly Term h
          | Some expr ->
            assert (args = []);
            let f_id = Dolmen_std.Id.create Term (Env.Term_cst.name env head) in
            Format.fprintf fmt "(! %a@ :named %a)"
              (term env) expr (id ~allow_keyword:false env) f_id
        end

    and letin env fmt (l, body) =
      (* reset some env state *)
      let env = set_omit_to_real env false in
      (* actual printing *)
      match l with
      | [] -> term env fmt body
      | binding :: r ->
        let env' = add_binding_to_env env binding in
        Format.fprintf fmt "@[<hv>(let (%a)@ %a)@]"
          (var_binding env' env) binding (letin env') (r, body)

    and letand env fmt (l, body) =
      (* reset some env state *)
      let env = set_omit_to_real env false in
      (* actual printing *)
      let env' = List.fold_left add_binding_to_env env l in
      Format.fprintf fmt "@[<hv>(let (@[<hv>%a@])@ %a)@]"
        (list (var_binding env') env) l (term env') body

    and var_binding var_env t_env fmt (v, t) =
      Format.fprintf fmt "@[<hov 2>(%a@ %a)@]" (term_var var_env) v (term t_env) t

    and term_match env fmt (scrutinee, cases) =
      (* reset some env state *)
      let env = set_omit_to_real env false in
      (* actual printing *)
      Format.fprintf fmt "@[<hv 2>(match@ @[<hov>%a@]@ (%a))@]"
        (term env) scrutinee
        (list match_case env) cases

    and match_case env fmt (pat, arm) =
      let env = add_pattern_to_env env pat in
      Format.fprintf fmt "(@[<h>%a@] @[<hov>%a@])" (pattern env) pat (term env) arm

    and pattern env fmt pat =
      match (pat : _ F.Term.pattern) with
      | Var v -> term_var env fmt v
      | Constructor (c, []) -> term_cst env fmt c
      | Constructor (c, args) ->
        Format.fprintf fmt "(%a %a)"
          (term_cst env) c (list term_var env) args

    and exists env fmt tys ts triggers body =
      match tys with
      | [] -> quant "exists" env fmt (ts, triggers, body)
      | _ :: _ -> _cannot_print "existencial type quantification"

    and forall env fmt tys ts triggers body =
      match tys with
      | [] -> quant "forall" env fmt (ts, triggers, body)
      | _ :: _ ->
        begin match Config.version with
          | Poly ->
            let env = List.fold_left Env.Ty_var.bind env tys in
            Format.fprintf fmt "@[<hv 1>(par@ (%a)@ %a)@]"
              (list ty_var env) tys
              (quant "forall" env) (ts, triggers, body)
          | V2_6 | V2_7 ->
            (* even in 2.7, universal type quantificaiton is restricted to toplevel,
               therefore it's handled by the function for printing assertions *)
            _cannot_print "universal type quantification"
        end

    and quant q env fmt (ts, triggers, body) =
      (* reset some env state *)
      let env = set_omit_to_real env false in
      (* actual printing *)
      match ts, triggers with
      | [], _ :: _ -> _cannot_print "triggers without quantified variables"
      | [], [] -> term env fmt body
      | _ :: _, [] ->
        let env = List.fold_left Env.Term_var.bind env ts in
        Format.fprintf fmt "@[<hov 1>(%s@ (%a)@ %a)@]" q
          (list sorted_var env) ts
          (term env) body
      | _ :: _, _ :: _ ->
        let env = List.fold_left Env.Term_var.bind env ts in
        Format.fprintf fmt "@[<hv 1>(%s (%a)@ (! %a@ %a))@]" q
          (list sorted_var env) ts
          (term env) body
          (list pattern_attr env) triggers

    and pattern_attr env fmt trigger =
      match V.Term.view trigger with
      (* Multi trigggers *)
      | App (c, _, l) when (match V.Term.Cst.builtin c with
          | B.Multi_trigger -> true | _ -> false) ->
        Format.fprintf fmt ":pattern @[<hov 1>(%a)@]" (list term env) l
      (* semantic triggers are ignored for now, might change later *)
      | App (c, _, _) when (match V.Term.Cst.builtin c with
          | B.Semantic_trigger -> true | _ -> false) ->
        ()
      (* *)
      | _ ->
        Format.fprintf fmt ":pattern @[<hov 1>(%a)@]" (term env) trigger

    and match_prop_literal t =
      (* either 'c' or 'not c' with 'c' a constant *)
      match V.Term.view t with
      | App (f, [], [arg]) when (match V.Term.Cst.builtin f with B.Prop Neg -> true | _ -> false) ->
        begin match V.Term.view arg with
          | App (c, [], []) -> `Neg c
          | _ -> `Not_a_prop_literal
        end
      | App (c, [], []) -> `Cst c
      | _ -> `Not_a_prop_literal

    and prop_literal env fmt t =
      match match_prop_literal t with
      | `Cst c ->
        let name = Env.Term_cst.name env c in
        symbol env fmt name
      | `Neg c ->
        let name = Env.Term_cst.name env c in
        Format.fprintf fmt "(not %a)" (symbol env) name
      | `Not_a_prop_literal ->
        _cannot_print "not a prop literal"

    and formula env fmt t =
      term_view env fmt (V.Term.ty t) (V.Term.view t)

    (* Datatypes *)
    (* ********* *)

    let constructor_dec env fmt (cstr, params) =
      match params with
      | [] ->
        Format.fprintf fmt "(%a)" (term_cst env) cstr
      | _ ->
        let aux env fmt (tty, dstr) =
          Format.fprintf fmt "@[<h>(%a %a)@]"
            (term_cst env) dstr (ty env) tty
        in
        Format.fprintf fmt "@[<hv 1>(%a@ %a)@]"
          (term_cst env) cstr
          (list aux env) params

    let datatype_dec env fmt (_, vars, cases) =
      match vars with
      | [] ->
        Format.fprintf fmt "@[<hv 1>(%a)@]" (list constructor_dec env) cases
      | _ ->
        let env = List.fold_left Env.Ty_var.bind env vars in
        Format.fprintf fmt "(par (%a)@ @[<hv 1>(%a))@]"
          (list ty_var env) vars (list constructor_dec env) cases


    (* Attributes *)
    (* ********** *)

    let rec sexpr env fmt t =
      match S.view t with
      | Symbol s -> id ~allow_keyword:true env fmt s
      | App l -> list sexpr env fmt l

    let is_keyword t =
      match S.view t with
      | Symbol { ns = Attr; name = Simple s; }
        when Misc.lex_string Lexer.check_keyword s -> true
      | _ -> false

    let attribute env fmt t =
      match S.view t with
      | Symbol _ when is_keyword t -> sexpr env fmt t
      | App ([k; _]) when is_keyword k -> sexpr env fmt t
      | _ -> _cannot_print "not an attribtue"

    let keyword env fmt t =
      if is_keyword t
      then sexpr env fmt t
      else _cannot_print "not a keyword"


    (* Statements *)
    (* ********** *)

    (* unit/trivial statements *)

    let unit_stmt s (_env : Env.t) fmt () =
      Format.fprintf fmt "(%s)" s

    let check_sat = unit_stmt "check-sat"

    let reset = unit_stmt "reset"
    let reset_assertions = unit_stmt "reset-assertions"

    let get_unsat_core = unit_stmt "get-unsat-core"
    let get_unsat_assumptions = unit_stmt "get-unsat-assumptions"

    let get_proof = unit_stmt "get-proof"
    let get_model = unit_stmt "get-model"

    let get_assertions = unit_stmt "get-assertions"
    let get_assignment = unit_stmt "get-assignment"

    let exit = unit_stmt "exit"

    (* statements with payloads *)

    let echo _env fmt s =
      Format.fprintf fmt "(echo \"%a\")" string s

    let set_logic _env fmt s =
      Format.fprintf fmt "@[<h>(set-logic %a)@]" symbol_aux s

    let set_info env fmt t =
      Format.fprintf fmt "@[<h>(set-info %a)@]" (attribute env) t

    let set_option env fmt t =
      Format.fprintf fmt "@[<h>(set-option %a)@]" (attribute env) t

    let get_info env fmt t =
      Format.fprintf fmt "@[<h>(get-info %a)@]" (keyword env) t

    let get_option env fmt t =
      Format.fprintf fmt "@[<h>(get-option %a)@]" (keyword env) t

    let get_value env fmt l =
      Format.fprintf fmt "@[<hv 2>(get-value (@,@[<hv>%a@]@,))"
        (list term env) l

    let pop _env fmt n =
      if n <= 0
      then raise (Cannot_print "pop with non-positive level")
      else Format.fprintf fmt "@[<h>(pop %d)@]" n

    let push _env fmt n =
      if n <= 0
      then raise (Cannot_print "push with non-positive level")
      else Format.fprintf fmt "@[<h>(push %d)@]" n

    let declare_sort_params env fmt l =
      let env = List.fold_left Env.Ty_var.bind env l in
      List.iter (fun var ->
          Format.fprintf fmt "@[<hov 2>(declare-sort-parameter %a)@]@\n"
            (ty_var env) var
        ) l;
      env

    let declare_sort env fmt c =
      let n = V.Ty.Cst.arity c in
      let name = Env.Ty_cst.name env c in
      Format.fprintf fmt "@[<h>(declare-sort %a %d)@]" (symbol env) name n

    let declare_datatype env fmt ((c, _, _) as dec) =
      Format.fprintf fmt "@[<hov 2>(declare-datatype %a@ %a)@]"
        (ty_cst env) c
        (datatype_dec env) dec

    let declare_datatypes env fmt l =
      let sort_dec env fmt (c, _, _) =
        let n = V.Ty.Cst.arity c in
        Format.fprintf fmt "@[<h>(%a %d)@]" (ty_cst env) c n
      in
      match l with
      | [decl] -> declare_datatype env fmt decl
      | _ ->
        Format.fprintf fmt "@[<v 2>(declare-datatypes@ @[<v 1>(%a)@]@ (%a))@]"
          (list sort_dec env) l
          (list datatype_dec env) l

    let declare_fun env fmt c =
      let name = Env.Term_cst.name env c in
      let c_sig = V.Term.Cst.ty c in
      match V.Sig.view ~expand:false c_sig with
      | Signature ([], [], c_ty) ->
        Format.fprintf fmt "@[<hov 2>(declare-const %a@ %a)@]"
          (symbol env) name (ty env) c_ty
      | Signature ([], params, ret) ->
        Format.fprintf fmt "@[<hov 2>(declare-fun %a@ (%a)@ %a)@]"
          (symbol env) name (list ty env) params (ty env) ret
      | Signature (vars, params, ret) ->
        begin match Config.version with
          | V2_6 -> raise Polymorphic_function_declaration
          | Poly ->
            let env = List.fold_left Env.Ty_var.bind env vars in
            Format.fprintf fmt "@[<hov 2>(declare-fun %a@ (par@ (%a)@ (%a)@ %a))@]"
              (symbol env) name (list ty_var env) vars
              (list ty env) params (ty env) ret
          | V2_7 ->
            let env = declare_sort_params env fmt vars in
            Format.fprintf fmt "@[<hov 2>(declare-fun %a@ (%a)@ %a)@]"
              (symbol env) name (list ty env) params (ty env) ret
        end

    let define_sort env fmt (c, params, body) =
      let env = List.fold_left Env.Ty_var.bind env params in
      Format.fprintf fmt "@[<hov 2>(define-sort %a@ (%a)@ %a)@]"
        (ty_cst env) c
        (list ty_var env) params
        (ty env) body

    let define_fun_aux ~recursive env fmt (f, vars, params, body) =
      let env = List.fold_left Env.Term_var.bind env params in
      match (vars : Env.ty_var list) with
      | [] ->
        Format.fprintf fmt
          "@[<hv 2>(@[<hov 1>%s %a@ (%a) %a@]@ @[<hov>%a@])@]"
          (if recursive then "define-fun-rec" else "define-fun")
          (term_cst env) f
          (list sorted_var env) params
          (ty env) (V.Term.ty body)
          (term env) body
      | _ :: _ ->
        begin match Config.version with
          | V2_6 -> raise Polymorphic_function_definition
          | Poly ->
            Format.fprintf fmt
              "@[<hv 2>(@[<hov 1>%s %a@ (par (%a)@ (%a) %a@]@ @[<hov>%a@]))@]"
              (if recursive then "define-fun-rec" else "define-fun")
              (term_cst env) f
              (list ty_var env) vars
              (list sorted_var env) params
              (ty env) (V.Term.ty body)
              (term env) body
          | V2_7 ->
            let env = declare_sort_params env fmt vars in
            Format.fprintf fmt
              "@[<hv 2>(@[<hov 1>%s %a@ (%a) %a@]@ @[<hov>%a@])@]"
              (if recursive then "define-fun-rec" else "define-fun")
              (term_cst env) f
              (list sorted_var env) params
              (ty env) (V.Term.ty body)
              (term env) body
        end

    let define_fun = define_fun_aux ~recursive:false
    let define_fun_rec = define_fun_aux ~recursive:true

    let define_funs_rec env fmt l =
      let l =
        List.map (fun (f, vars, params, body) ->
            match Config.version, vars with
            | V2_6, _ :: _ -> raise Polymorphic_function_definition
            | Poly, _ :: _
            | _, [] ->
              let env = List.fold_left Env.Ty_var.bind env vars in
              let env = List.fold_left Env.Term_var.bind env params in
              (env, f, vars, params, body)
            | V2_7, _ :: _ ->
              let env = declare_sort_params env fmt vars in
              let env = List.fold_left Env.Term_var.bind env params in
              (env, f, vars, params, body)
          ) l
      in
      let fun_body _env fmt (env, _, _, _, body) = term env fmt body in
      let fun_decl _env fmt (env, f, vars, params, body) =
        match vars with
        | [] ->
          Format.fprintf fmt "@[<hov 1>(%a@ (%a)@ %a)@]"
            (term_cst env) f
            (list sorted_var env) params
            (ty env) (V.Term.ty body)
        | _ :: _ ->
          Format.fprintf fmt "@[<hov 1>(%a (par@ (%a)@ (%a)@ %a))@]"
            (term_cst env) f
            (list ty_var env) vars
            (list sorted_var env) params
            (ty env) (V.Term.ty body)
      in
      Format.fprintf fmt
        "@[<v 2>(define-funs-rec@ @[<v 1>(%a)@]@ @[<v 1>(%a)@])@]"
        (list fun_decl env) l
        (list fun_body env) l

    let assert_ env fmt t =
      match Config.version with
      | V2_6 | Poly ->
        Format.fprintf fmt "@[<hov 2>(assert %a)@]" (formula env) t
      | V2_7 ->
        begin match V.Term.view t with
          | Binder (Forall { type_vars; term_vars; triggers; }, body) ->
            let env = declare_sort_params env fmt type_vars in
            let aux fmt () = forall env fmt [] term_vars triggers body in
            Format.fprintf fmt "@[<hov 2>(assert %a)@]" aux ()
          | _ ->
            Format.fprintf fmt "@[<hov 2>(assert %a)@]" (formula env) t
        end

    let check_sat_assuming env fmt = function
      | [] -> check_sat env fmt ()
      | _ :: _ as l ->
        let pp =
          match Config.version with
          | V2_6 | Poly -> prop_literal
          | V2_7 -> term
        in
        Format.fprintf fmt "@[<hov >(check-sat-assuming (%a))"
          (list pp env) l

  end

end
