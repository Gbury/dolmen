
module Id = Dolmen.Std.Id
module Term = Dolmen.Std.Term

(* Ae arithmetic *)
(* ************************************************************************ *)

module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Arith with type t := Type.T.t
                                      and type ty := Type.Ty.t) = struct

    type _ Type.err +=
      | Expected_arith_type : Type.Ty.t -> Term.t Type.err

    let dispatch1 env (mk_int, mk_real) ast t =
      let ty = T.ty t in
      match Ty.view ty with
      | `Int -> mk_int t
      | `Real -> mk_real t
      | _ -> Type._error env (Ast ast) (Expected_arith_type ty)

    let dispatch2 env (mk_int, mk_real) ast a b =
      let tya = T.ty a in
      match Ty.view tya with
      | `Int -> mk_int a b
      | `Real -> mk_real a b
      | _ -> Type._error env (Ast ast) (Expected_arith_type tya)

    let promote_to_real t =
      let ty = T.ty t in
      match Ty.view ty with
      | `Int -> T.Int.to_real t
      | `Real -> t
      (* this will result in a more precise typing error later, so it's okay *)
      | _ -> t

    let parse_bound env ast =
      match ast.Term.term with
      | Symbol { ns = Term; name = Simple "?" } -> None
      | _ ->
        let t = Type.parse_term env ast in
        Some t

    let parse_in_interval env ~strict_lower ~strict_upper _ (ast, lower, upper) =
      let t = Type.parse_term env ast in
      let t_ty = T.ty t in
      let x = Type.T.Var.mk "x" t_ty in
      let x_t = Type.T.of_var x in
      let mk strict a b =
        match Ty.view t_ty with
        | `Int -> if strict then T.Int.lt a b else T.Int.le a b
        | `Real -> if strict then T.Real.lt a b else T.Real.le a b
        | _ -> Type._error env (Ast ast) (Expected_arith_type t_ty)
      in
      let lower =
        Misc.Options.map2 (mk strict_lower)
          (parse_bound env lower) (Some x_t)
      in
      let upper =
        Misc.Options.map2 (mk strict_upper)
          (Some x_t) (parse_bound env upper)
      in
      match lower, upper with
      | None, None -> Type.T._true
      | None, Some b
      | Some b, None ->
        T.semantic_trigger @@
        Type.T.letin [x, t] b
      | Some b, Some b' ->
        T.semantic_trigger @@
        Type.T.letin [x, t] @@
        Type.T._and [b; b']

      let parse env s =
        match s with
        (* Types *)
        | Type.Builtin Term.Int ->
          Type.builtin_ty (Base.app0 (module Type) env s Ty.int)
        | Type.Builtin Term.Real ->
          Type.builtin_ty (Base.app0 (module Type) env s Ty.real)

        (* Literals *)
        | Type.Id { Id.ns = Value Integer; name = Simple name; } ->
          Type.builtin_term (Base.app0 (module Type) env s (T.int name))
        | Type.Id { Id.ns = Value Real; name = Simple name; } ->
          Type.builtin_term (Base.app0 (module Type) env s (T.real name))
        | Type.Id { Id.ns = Value Hexadecimal; name = Simple name; } ->
          Type.builtin_term (Base.app0 (module Type) env s (T.real name))

        | Type.Id { Id.ns = Term; name = Simple name; } ->
          begin match name with
          | "real_of_int" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.Int.to_real)
          | "int_floor" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.Real.floor_to_int)
          | "real_is_int" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.Real.is_int)
          | "abs_int" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.Int.abs)
          | _ -> `Not_found
          end

        (* Arithmetic *)
        | Type.Builtin Term.Minus ->
          Type.builtin_term (Base.term_app1_ast (module Type) env s
            (dispatch1 env (T.Int.minus, T.Real.minus)))
        | Type.Builtin Term.Add ->
          Type.builtin_term (Base.term_app2_ast (module Type) env s
            (dispatch2 env (T.Int.add, T.Real.add)))
        | Type.Builtin Term.Sub ->
          Type.builtin_term (Base.term_app2_ast (module Type) env s
            (dispatch2 env (T.Int.sub, T.Real.sub)))
        | Type.Builtin Term.Mult ->
          Type.builtin_term (Base.term_app2_ast (module Type) env s
            (dispatch2 env (T.Int.mul, T.Real.mul)))
        | Type.Builtin Term.Div ->
          Type.builtin_term (Base.term_app2_ast (module Type) env s
            (dispatch2 env (T.Int.div_e, T.Real.div)))
        | Type.Builtin Term.Mod ->
          Type.builtin_term (Base.term_app2 (module Type) env s T.Int.rem_e)
        | Type.Builtin Term.Int_pow ->
          Type.builtin_term (Base.term_app2 (module Type) env s T.Int.pow)
        | Type.Builtin Term.Real_pow ->
          Type.builtin_term (fun ast args ->
            Base.term_app2 (module Type) env s
              (fun a b ->
                 let a' = promote_to_real a in
                 let b' = promote_to_real b in
                 T.Real.pow a' b') ast args)

      | Type.Builtin Term.Lt ->
        Type.builtin_term (fun ast args ->
            match args with
            | [Term.{
                term = App (
                    { term = Builtin (Lt | Leq); _ }, [_; lr_st]
                  ); _
              } as l_st; r_st] ->
              Base.term_app_list (module Type) env s
                Type.T._and ast [l_st; Term.lt lr_st r_st]
            | _ ->
              Base.term_app2_ast (module Type) env s
                (dispatch2 env (T.Int.lt, T.Real.lt)) ast args
          )

      | Type.Builtin Term.Leq ->
        Type.builtin_term (fun ast args ->
            match args with
            | [Term.{
                term = App (
                    { term = Builtin (Lt | Leq); _ }, [_; lr_st]
                  ); _
              } as l_st; r_st] ->
              Base.term_app_list (module Type) env s
                Type.T._and ast [l_st; Term.leq lr_st r_st]
            | _ ->
              Base.term_app2_ast (module Type) env s
                (dispatch2 env (T.Int.le, T.Real.le)) ast args
          )

      | Type.Builtin Term.Gt ->
        Type.builtin_term (fun ast args ->
            match args with
            | [Term.{
                term = App (
                    { term = Builtin (Gt | Geq); _ }, [_; lr_st]
                  ); _
              } as l_st; r_st] ->
              Base.term_app_list (module Type) env s
                Type.T._and ast [l_st; Term.gt lr_st r_st]
            | _ ->
              Base.term_app2_ast (module Type) env s
                (dispatch2 env (T.Int.gt, T.Real.gt)) ast args
          )

      | Type.Builtin Term.Geq ->
        Type.builtin_term (fun ast args ->
            match args with
            | [Term.{
                term = App (
                    { term = Builtin (Gt | Geq); _ }, [_; lr_st]
                  ); _
              } as l_st; r_st] ->
              Base.term_app_list (module Type) env s
                Type.T._and ast [l_st; Term.geq lr_st r_st]
            | _ ->
              Base.term_app2_ast (module Type) env s
                (dispatch2 env (T.Int.ge, T.Real.ge)) ast args
          )

      (* In_interval semantic trigger *)
      | Type.Builtin (Term.In_interval (strict_lower, strict_upper)) ->
        Type.builtin_term (
          Base.make_op3 (module Type) env s
            (parse_in_interval env ~strict_lower ~strict_upper)
        )

      (* Catch-all *)
      | _ -> `Not_found

  end
end

(* Smtlib arithmetic (integer and reals) *)
(* ************************************************************************ *)

module Smtlib2 = struct

  (* Classification of terms w.r.t. linearity, as per the specification
     of the SMTLIB. Note that there exists 2 "versions" of linearity in the
     SMTLIB: most of the linear arithmetic logics (e.g. AUFLIRA, LIA, LRA,
     QF_LIA, QF_UFLIA, QF_UFLRA, QF_LRA, UFLRA)
     specify linear terms as terms of the form c, ( * x c), ( * c x) where:
       - x is a free constant of sort Int or Real and
       - c is an integer or rational coefficient, respectively.
     Whereas some logics (e.g. AUFLIA, QF_AUFLIA), use the following:
       - x is a free constant or a term with top symbol not in Ints, and
       - c is a term of the form n or (- n) for some numeral n.

     Note that in this context "symbol" actually means a constant or
     quantified variable.


     Additionally, difference logic is a hell of specification. There are
     about as many specs of difference logic than logics that use it.
     Currently, there are three different difference logics:
     - integer difference logic, as per the spec of QF_IDL
     - real difference logic, as per the spec of QF_RDL
     - UFIDL difference logic, as per the spec of QF_UFIDL

     There are therefore a few different "variants" of arithmetic that one
     can typecheck for:
     - regular arithmetic where everything is allowed
     - lenient linear arithmetic (as specified by AUFLIA)
     - strict linear arithmetic (as specified by other logics)
     - integer difference logic
     - real difference logic
     - difference logic in UFIDL
  *)
  type config =
    | Regular
    | Linear of [ `Large | `Strict ]
    | Difference of [ `IDL | `RDL | `UFIDL ]

  let print_config fmt = function
    | Regular -> Format.fprintf fmt "regular"
    | Linear `Large -> Format.fprintf fmt "linear (large)"
    | Linear `Strict -> Format.fprintf fmt "linear"
    | Difference `IDL -> Format.fprintf fmt "IDL"
    | Difference `RDL -> Format.fprintf fmt "RDL"
    | Difference `UFIDL -> Format.fprintf fmt "UFIDL"

  (* In order to establish the needed classification and correctly raise warnings
     or errors, we first need a fine-grained view of terms according to an arithmetic
     point of view, which is what the following represent. *)
  type view =
    | Numeral of string
    | Decimal of string
    | Negation of Term.t
    | Addition of Term.t list
    | Subtraction of Term.t list
    | Division of Term.t * Term.t
    | Complex_arith
    | Variable of Id.t
    | Constant of Id.t
    | Top_symbol_not_in_arith

  (* Results of filters that enforce restrictions such as linearity. Due to the
     confusion created by the 2 different specs, some breach of restrictions
     can be classified as warnings so that they can be recovered from. *)
  type filter_res =
    | Ok
    | Warn of string
    | Error of string

  (* Low-level arithmetic view of terms *)
  module View(Type : Tff_intf.S) = struct

    (* View of arithmetic expressions *)
    type t = view

    let print fmt (t : t) =
      match t with
      | Numeral s -> Format.fprintf fmt "the numeral '%s'" s
      | Decimal s -> Format.fprintf fmt "the decimal '%s'" s
      | Negation _ -> Format.fprintf fmt "a negation"
      | Addition _ -> Format.fprintf fmt "an addition"
      | Subtraction _ -> Format.fprintf fmt "a subtraction"
      | Division _ -> Format.fprintf fmt "a division"
      | Complex_arith -> Format.fprintf fmt "a complex arithmetic expression"
      | Variable v -> Format.fprintf fmt "the quantified variable %a" Id.print v
      | Constant c -> Format.fprintf fmt "the constant symbol %a" Id.print c
      | Top_symbol_not_in_arith ->
        Format.fprintf fmt "an arbitrary (not arithmetic) expression"

    let expect_error v v' expected =
      Format.asprintf "expects %s but was given:\n- %a\n- %a"
        expected print v print v'

    let rec view ~parse version env (t : Term.t) =
      match t.term with
      | Symbol { ns = Value Integer; name = Simple name } -> Numeral name
      | Symbol { ns = Value Real; name = Simple name } -> Decimal name

      | App ({ term = Symbol { Id.ns = Term; name = Simple "-"; }; _ }, [e])
        -> Negation e

      | App ({ term = Symbol { Id.ns = Term; name = Simple "+"; }; _ }, ((_ :: _) as args))
        -> Addition args

      | App ({ term = Symbol { Id.ns = Term; name = Simple "-"; }; _ }, ((_ :: _) as args))
        -> Subtraction args

      | App ({ term = Symbol { Id.ns = Term; name = Simple "/"; }; _ }, [a; b])
        -> Division (a, b)

      | Symbol id -> view_id ~parse version env id []
      | App ({ term = Symbol id; _}, args) -> view_id ~parse version env id args

      | Builtin b | App ({ term = Builtin b; _ }, _) ->
        begin match parse version env (Type.Builtin b) with
          | #Type.builtin_res -> Complex_arith
          | #Type.not_found -> Top_symbol_not_in_arith
        end

      (* Catch-all *)
      | _ -> Top_symbol_not_in_arith

    (* We here use the fact that smtlib forbids shadowing, hence the order
       of the lookups does not matter.
       Additionally, we do not want the lookup to go through all the
       builtins as it is not necessary, hence the manual unfolding of
       the Type.find_bound function. *)
    and view_id ~parse version env id args =
      match Type.find_var env id with
      | `Letin (env, e, _, _) -> view ~parse version env e
      | #Type.var ->
        begin match args with
          | [] -> Variable id
          | _ -> Top_symbol_not_in_arith
        end
      | #Type.not_found ->
        begin match Type.find_global env id with
          | #Type.cst ->
            begin match args with
              | [] -> Constant id
              | _ -> Top_symbol_not_in_arith
            end
          | #Type.not_found ->
            begin match parse version env (Type.Id id) with
              | #Type.builtin_res -> Complex_arith
              | #Type.not_found -> Top_symbol_not_in_arith
            end
        end

    let rec difference_count view t :
      [ `Ok of Id.t * int | `Error of string ] =
      match (view t : view) with
      | Variable v -> `Ok (v, 1)
      | Constant c -> `Ok (c, 1)
      | Addition l -> difference_count_list view l
      | v -> `Error (
          Format.asprintf "addition in real difference logic expects \
                           either variables/constants or an addition of \
                           variables/constants, but was here given %a" print v)

    and difference_count_list view = function
      | h :: r ->
        begin match difference_count view h with
          | (`Error _) as res -> res
          | `Ok (symb, n) ->
            difference_count_list_aux view n symb r
          end
      | [] -> assert false

    and difference_count_list_aux view n s = function
      | [] -> `Ok (s, n)
      | t :: r ->
        begin match difference_count view t with
          | (`Error _) as res -> res
          | `Ok (s', n') ->
            if Id.equal s s' then
              difference_count_list_aux view (n + n') s r
            else
              `Error (
                Format.asprintf "addition in real difference logic expects
                                 n-th times the same variable/constant, but was
                                 here applied to %a and %a which are different"
                  Id.print s Id.print s')
        end


  end

  module Classification = struct

    (* We can now classify terms according to the following kinds:
       - coefficients, i.e. integer or rational literals, as per the spec above.
       Coefficients consists of:
         + raw literals (i.e. in the Id.Value namespace)
         + the negation of (i.e. application of term symbol "-" to) a raw literal
         + the division of (i.e. an application of term symbol "/" to) of an
           integer coeficient by a raw literal different from zero.
       - complex arithmetic expressions, i.e. expressions whose top-level symbol
         is a builtin symbol of arithmetic (e.g. '+', '-', '*', ...), but that
         are not coefficients.
       - nullary constant symbols (standalone, or applied to the empty list),
         and quantified variables
       - all other expressions *)
    type t =
      | Int_coefficient
      | Rat_coefficient
      | Complex_arith
      | Variable_or_constant
      | Top_symbol_not_in_arith

    let print fmt (t: t) =
      match t with
      | Int_coefficient -> Format.fprintf fmt "an integer coefficient"
      | Rat_coefficient -> Format.fprintf fmt "a rational coefficient"
      | Complex_arith -> Format.fprintf fmt "a complex arithmetic expression"
      | Variable_or_constant -> Format.fprintf fmt "a symbol (or quantified variable)"
      | Top_symbol_not_in_arith ->
        Format.fprintf fmt "an arbitrary expression with top symbol not in Arithmetic"

    let expect_error c c' expected =
      Format.asprintf "expects %s but was given:\n- %a\n- %a"
        expected print c print c'

    let rec classify view t =
      match (view t: view) with
      | Numeral _ -> Int_coefficient
      | Decimal _ -> Rat_coefficient
      | Addition _ -> Complex_arith
      | Subtraction _ -> Complex_arith
      | Negation t' ->
        begin match (view t': view) with
          | Numeral _ -> Int_coefficient
          | Decimal _ -> Rat_coefficient
          | _ -> Complex_arith
        end
      | Division (numerator, denominator) ->
        begin match (classify view numerator, view denominator) with
          | Int_coefficient, Numeral s when s <> "0"
            -> Rat_coefficient
          | _ -> Complex_arith
        end
      | Complex_arith -> Complex_arith
      | Variable _ | Constant _ -> Variable_or_constant
      | Top_symbol_not_in_arith -> Top_symbol_not_in_arith


  end

  (* Filter for arithmetic *)
  module Filter(Type : Tff_intf.S) = struct

    module V = View(Type)

    let bad_arity operator logic n =
      Error (Format.asprintf
               "%s in %s must have exactly %d arguments" operator logic n)

    let forbidden operator logic =
      Error (Format.asprintf "%s is not allowed in %s" operator logic)

    (* Filter helpers / Individual restriction filters *)

    let minus_dl parse version env = function
      | [ a ] ->
        begin match V.view ~parse version env a with
          | Numeral _
          | Decimal _ -> Ok
          | v ->
            Error (Format.asprintf
                     "unary subtraction in difference logic expects \
                      a literal, but was given %a"
                     V.print v)
        end
      | _ -> bad_arity "unary subtraction" "difference logic" 1

    let sub_idl parse version env = function
      | [ a; b ] ->
        begin match Classification.classify (V.view ~parse version env) a,
                    Classification.classify (V.view ~parse version env) b with
        | Variable_or_constant, Variable_or_constant -> Ok
        | v, v' ->
          Error (Format.asprintf "subtraction in difference logic %s"
                   (Classification.expect_error v v' "two constants/variables"))
        end
      | _ -> bad_arity "subtraction" "integer difference logic" 2

    let sub_rdl parse version env = function
      | [ a; b ] ->
        begin match V.difference_count (V.view ~parse version env) a,
                    V.difference_count (V.view ~parse version env) b with
        | `Error msg, _
        | _, `Error msg
          -> Error msg
        | `Ok (_, n), `Ok (_, n') ->
          if n = n' then Ok
          else Error (
              Format.asprintf "subtraction in real difference logic \
                               expects both sides to be sums of the same \
                               length, but here the sums have lengths \
                               %d and %d" n n')
        end
      | _ -> bad_arity "subtraction" "real difference logic" 2

    let op_ufidl parse version env args =
      let rec aux non_numeral_seen = function
        | [] -> Ok
        | t :: r ->
          begin match V.view ~parse version env t with
            | Numeral _ -> aux non_numeral_seen r
            | _ ->
              if non_numeral_seen then
                Error (
                  Format.asprintf
                    "addition/subtraction in difference logic \
                     (in UFIDL/QF_UFIDL) expects all but at most \
                     one of its arguments to be numerals")
              else
                aux true r
          end
      in
      aux false args

    let add_rdl parse version env args =
      match V.difference_count_list (V.view ~parse version env) args with
      | `Ok _ -> Ok
      | `Error msg -> Error msg

    let div_linear parse version env = function
      | [ a; b ] ->
        begin match Classification.classify (V.view ~parse version env) a with
          | Int_coefficient ->
            begin match V.view ~parse version env b with
              | Numeral "0" ->
                Error (Format.asprintf "division in linear arithmetic \
                                        expects a non-zero denominator")
              | Numeral _ -> Ok
              | v ->
                Error (Format.asprintf "division in linear arithmetic \
                                        expects a constant positive \
                                        integer literal as denominator, \
                                        but was given %a"
                         V.print v)
            end
          | _ ->
            let v = V.view ~parse version env a in
            let msg =
              Format.asprintf "division in linear arithmetic \
                               expects as first argument an integer \
                               coeficient, i.e. either an integer  \
                               numeral or the negation of one, but \
                               here was given %a" V.print v
            in
            begin match v with
              (* Allow "xxx.0" rationals with a warning *)
              | Decimal s when Misc.Strings.is_suffix s ~suffix:".0" -> Warn msg
              | _ -> Error msg
            end
        end
      | _ -> bad_arity "division" "linear arithmetic" 2

    let mul_linear_msg ~strict c c' =
      Format.asprintf "multiplication in %slinear arithmetic %s"
        (if strict then "strict " else "")
        (Classification.expect_error c c'
           (if strict
            then "an integer or rational literal and a symbol (variable or constant)"
            else "an integer or rational literal and a non-arithmetic expression"))

    let mul_linear ~strict parse version env = function
      | [ a; b ] ->
        begin match Classification.classify (V.view ~parse version env) a,
                    Classification.classify (V.view ~parse version env) b with
        | (Int_coefficient | Rat_coefficient), Variable_or_constant
        | Variable_or_constant, (Int_coefficient | Rat_coefficient)
          -> Ok
        | ((Int_coefficient | Rat_coefficient) as c), (Top_symbol_not_in_arith as c')
        | (Top_symbol_not_in_arith as c), ((Int_coefficient | Rat_coefficient) as c')
          ->
          if strict then Warn (mul_linear_msg ~strict c c') else Ok
        | ((Int_coefficient | Rat_coefficient) as c),
          ((Int_coefficient | Rat_coefficient | Complex_arith) as c')
        | (Complex_arith as c'), ((Int_coefficient | Rat_coefficient) as c) ->
          Warn (mul_linear_msg ~strict c c')
        | c, c' ->
          Error (mul_linear_msg ~strict c c')
        end
      | _ -> bad_arity "multiplication" "linear arithmetic" 2

    let comp_idl parse version env = function
      | [ a; b ] ->
        begin match V.view ~parse version env a,
                    V.view ~parse version env b with
        | (Variable _ | Constant _),
          (Variable _ | Constant _) -> Ok
        (* If the first argument is a subtraction, it must have passed
           the sub_wrapper filter, which means both its side must be
           constants, so no need to check it here again. *)
        | Subtraction _, Numeral _ -> Ok
        | Subtraction _, Negation _ -> Ok
        (* error case *)
        | v, v' ->
          Error (Format.asprintf "comparison in integer difference logic %s"
                   (V.expect_error v v'
                      "a substraction on the left and a (possibly negated) \
                       integer literal on the right"))
        end
      | _ -> bad_arity "comparison" "integer difference logic" 2

    let comp_rdl parse version env = function
      | [ a; b ] ->
        begin match V.view ~parse version env a,
                    V.view ~parse version env b with
        | (Variable _ | Constant _),
          (Variable _ | Constant _) -> Ok
        (* If the first argument is a subtraction, it must have passed
           the sub_wrapper filter, which means both its side must be
           constants, so no need to check it here again. *)
        | Subtraction _, Numeral _ -> Ok
        | Subtraction _, Negation _ -> Ok
        (* Syntactic sugar *)
        | Subtraction [x; y], Division _ ->
          begin match V.difference_count (V.view ~parse version env) x,
                      V.difference_count (V.view ~parse version env) y with
          | `Error msg, _
          | _, `Error msg -> Error msg
          | `Ok (_, n), `Ok (_, n') ->
            (* since the sub filter passed, we should have n = n' *)
            if n = 1 && n' = 1 then Ok
            else
              Error (
                Format.asprintf "in real difference logic, when comparing \
                                 the result of a subtraction with a rational \
                                 number, each side of the subtraction can only \
                                 contain a single variable/constant, but here there
                                 was %d" n)
          end
        (* Error case *)
        | v, v' ->
          Error (
            Format.asprintf "comparison in difference logic %s"
              (V.expect_error v v'
                 "a subtraction on the left and a (possibly negated) \
                  integer literal on the right"))
        end
      | _ -> bad_arity "comparison" "real difference logic" 2


    (* Global filters *)

    let minus config parse version env args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> Ok
        | Difference (`IDL | `RDL) ->
          minus_dl parse version env args
        | Difference `UFIDL ->
          forbidden "unary subtraction" "difference logic (QF_UFIDL variant)"

    let sub config parse version env args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> Ok
        | Difference `IDL -> sub_idl parse version env args
        | Difference `RDL -> sub_rdl parse version env args
        | Difference `UFIDL -> op_ufidl parse version env args

    let add config parse version env args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> Ok
        | Difference `IDL -> forbidden "addition" "integer difference logic"
        | Difference `RDL -> add_rdl parse version env args
        | Difference `UFIDL -> op_ufidl parse version env args

    let mul config parse version env args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear `Strict -> mul_linear ~strict:true parse version env args
        | Linear `Large -> mul_linear ~strict:false parse version env args
        | Difference `IDL -> forbidden "multiplication" "integer difference logic"
        | Difference `RDL -> forbidden "multiplication" "real difference logic"
        | Difference `UFIDL -> forbidden "multiplication" "difference logic (QF_UFIDL variant)"

    let div config parse version env args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> div_linear parse version env args
        | Difference `IDL -> forbidden "division" "integer difference logic"
        | Difference `RDL -> div_linear parse version env args
        | Difference `UFIDL -> forbidden "division" "difference logic (QF_UFIDL variant)"

    let ediv config version _args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> forbidden "euclidean division" "linear arithmetic"
        | Difference _ -> forbidden "euclidean division" "difference logic"

    let mod_ config version _args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> forbidden "mod" "linear arithmetic"
        | Difference _ -> forbidden "mod" "difference logic"

    let abs config version _args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> forbidden "abs" "linear arithmetic"
        | Difference _ -> forbidden "abs" "difference logic"

    let comp config parse version env args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> Ok
        | Difference `IDL -> comp_idl parse version env args
        | Difference `RDL -> comp_rdl parse version env args
        | Difference `UFIDL -> Ok

    let divisible config version _args =
      match (version : Dolmen.Smtlib2.version) with
      | `Response _ ->
        (* Allow all operations in responses, since we will evaluate them *)
        Ok
      | `Script _ ->
        match config with
        | Regular -> Ok
        | Linear _ -> forbidden "divisible" "linear arithmetic"
        | Difference _ -> forbidden "divisible" "difference logic"

  end

  (* Integer arithmetics *)

  module Int = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Int with type t := Type.T.t
                                          and type cst := Type.T.Const.t) = struct

      module F = Filter(Type)

      type _ Type.warn +=
        | Restriction : config * string -> Term.t Type.warn

      type _ Type.err +=
        | Forbidden : config * string -> Term.t Type.err

      let check config env filter ast args =
        match filter args with
        | Ok -> ()
        | Warn msg -> Type._warn env (Ast ast) (Restriction (config, msg))
        | Error msg -> Type._error env (Ast ast) (Forbidden (config, msg))

      let check1 config env filter ast a = check config env filter ast [a]
      let check2 config env filter ast a b = check config env filter ast [a; b]

      let rec parse ~config version env s =
        match s with
        (* type *)
        | Type.Id { Id.ns = Sort; name = Simple "Int"; } ->
          Type.builtin_ty (Base.app0 (module Type) env s Ty.int)
        (* values *)
        | Type.Id { Id.ns = Value Integer; name = Simple name; } ->
          Type.builtin_term (Base.app0 (module Type) env s (T.mk name))
        (* terms *)
        | Type.Id { Id.ns = Term; name = Simple name; } ->
          begin match name with
            | "-" ->
              Type.builtin_term (fun ast args -> match args with
                  | [x] ->
                    check config env (F.minus config (parse ~config) version env) ast args;
                    T.minus (Type.parse_term env x)
                  | _ ->
                    Base.term_app_left (module Type) env s T.sub ast args
                      ~check:(check config env (F.sub config (parse ~config) version env))
                )
            | "+" ->
              Type.builtin_term (Base.term_app_left (module Type) env s T.add
                    ~check:(check config env (F.add config (parse ~config) version env)))
            | "*" ->
              Type.builtin_term (Base.term_app_left (module Type) env s T.mul
                       ~check:(check config env (F.mul config (parse ~config) version env)))
            | "div" ->
              Type.builtin_term (Base.term_app_left (module Type) env s T.div
                                   ~check:(check config env (F.ediv config version)))
                ~meta:(`Partial (fun _ _ _ -> T.div'))
            | "mod" ->
              Type.builtin_term (Base.term_app2 (module Type) env s T.rem
                                   ~check:(check2 config env (F.mod_ config version)))
                ~meta:(`Partial (fun _ _ _ -> T.rem'))
            | "abs" ->
              Type.builtin_term (Base.term_app1 (module Type) env s T.abs
                    ~check:(check1 config env (F.abs config version)))
            | "<=" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.le
                    ~check:(check config env (F.comp config (parse ~config) version env)))
            | "<" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.lt
                    ~check:(check config env (F.comp config (parse ~config) version env)))
            | ">=" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.ge
                    ~check:(check config env (F.comp config (parse ~config) version env)))
            | ">" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.gt
                    ~check:(check config env (F.comp config (parse ~config) version env)))

            | "div0" -> `Reserved (`Model (
                "completing interpretation of division by zero in models",
                `Partial (fun _ _ _ -> T.div')))
            | "mod0" -> `Reserved (`Model (
                "completing interpretation of modulo by zero in models",
                `Partial (fun _ _ _ -> T.rem')))

            | _ -> `Not_found
          end
        | Type.Id { Id.ns = Term; name = Indexed { basename; indexes; }; } ->
          Base.parse_indexed basename indexes
            ~k:(function () -> `Not_found)
            ~err:(Base.bad_ty_index_arity (module Type) env) (function
                | "divisible" ->
                  `Unary (function n ->
                      Type.builtin_term (Base.term_app1 (module Type) env s (T.divisible n)
                               ~check:(check1 config env (F.divisible config version)))
                    )
                | _ -> `Not_indexed
              )
        | _ -> `Not_found

    end
  end

  module Real = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real with type t := Type.T.t
                                           and type cst := Type.T.Const.t) = struct

      module F = Filter(Type)

      type _ Type.warn +=
        | Restriction : config * string -> Term.t Type.warn

      type _ Type.err +=
        | Forbidden : config * string -> Term.t Type.err

      let check config env filter ast args =
        match filter args with
        | Ok -> ()
        | Warn msg -> Type._warn env (Ast ast) (Restriction (config, msg))
        | Error msg -> Type._error env (Ast ast) (Forbidden (config, msg))

      let rec parse ~config version env s =
        match s with
        (* type *)
        | Type.Id { Id.ns = Sort; name = Simple "Real"; } ->
          Type.builtin_ty (Base.app0 (module Type) env s Ty.real)
        (* values *)
        | Type.Id { Id.ns = Value (Integer | Real); name = Simple name; } ->
          Type.builtin_term (Base.app0 (module Type) env s (T.mk name))
        (* terms *)
        | Type.Id { Id.ns = Term; name = Simple name; } ->
          begin match name with
            | "-" -> Type.builtin_term (fun ast args ->
                match args with
                | [x] ->
                  check config env (F.minus config (parse ~config) version env) ast args;
                  T.minus (Type.parse_term env x)
                | _ ->
                  Base.term_app_left (module Type) env s T.sub ast args
                    ~check:(check config env (F.sub config (parse ~config) version env))
              )
            | "+" ->
              Type.builtin_term (Base.term_app_left (module Type) env s T.add
                       ~check:(check config env (F.add config (parse ~config) version env)))
            | "*" ->
              Type.builtin_term (Base.term_app_left (module Type) env s T.mul
                       ~check:(check config env (F.mul config (parse ~config) version env)))
            | "/" ->
              Type.builtin_term (Base.term_app_left (module Type) env s T.div
                       ~check:(check config env (F.div config (parse ~config) version env)))
                ~meta:(`Partial (fun _ _ _ -> T.div'))
            | "<=" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.le
                       ~check:(check config env (F.comp config (parse ~config) version env)))
            | "<" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.lt
                       ~check:(check config env (F.comp config (parse ~config) version env)))
            | ">=" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.ge
                       ~check:(check config env (F.comp config (parse ~config) version env)))
            | ">" ->
              Type.builtin_term (Base.term_app_chain (module Type) env s T.gt
                       ~check:(check config env (F.comp config (parse ~config) version env)))

            | "/0" -> `Reserved (`Model (
                "completing interpretation of division by zero in models",
                `Partial (fun _ _ _ -> T.div')))

            | _ -> `Not_found
          end
        | _ -> `Not_found

    end
  end

  module Real_Int = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real_Int with type t := Type.T.t
                                               and type ty := Type.Ty.t
                                               and type Int.cst := Type.T.Const.t
                                               and type Real.cst := Type.T.Const.t) = struct

      module F = Filter(Type)

      type _ Type.warn +=
        | Restriction : config * string -> Term.t Type.warn

      type _ Type.err +=
        | Forbidden : config * string -> Term.t Type.err
        | Expected_arith_type : Type.Ty.t -> Term.t Type.err

      let check config env filter ast args =
        match filter args with
        | Ok -> ()
        | Warn msg -> Type._warn env (Ast ast) (Restriction (config, msg))
        | Error msg -> Type._error env (Ast ast) (Forbidden (config, msg))

      let check1 config env filter ast a = check config env filter ast [a]
      let check2 config env filter ast a b = check config env filter ast [a; b]

      let dispatch1 env (mk_int, mk_real) ast t =
        match Ty.view @@ T.ty t with
        | `Int -> mk_int t
        | `Real -> mk_real t
        | _ -> Type._error env (Ast ast) (Expected_arith_type (T.ty t))

      let dispatch2 env (mk_int, mk_real) ast a b =
        match Ty.view @@ T.ty a, Ty.view @@ T.ty b with
        | `Int, `Int -> mk_int a b
        | `Int, `Real -> mk_real (T.Int.to_real a) b
        | `Real, `Int -> mk_real a (T.Int.to_real b)
        | `Real, `Real -> mk_real a b
        | (`Int | `Real), _ -> Type._error env (Ast ast) (Expected_arith_type (T.ty b))
        | _, (`Int | `Real) -> Type._error env (Ast ast) (Expected_arith_type (T.ty a))
        | _, _ -> Type._error env (Ast ast) (Expected_arith_type (T.ty a))

      let promote_int_to_real _env mk_real _ast a b =
        match Ty.view @@ T.ty a, Ty.view @@ T.ty b with
        | `Int, `Int -> mk_real (T.Int.to_real a) (T.Int.to_real b)
        | _ -> mk_real a b

      let rec parse ~config version env s =
        match s with

        (* type *)
        | Type.Id { Id.ns = Sort; name = Simple "Int"; } ->
          Type.builtin_ty (Base.app0 (module Type) env s Ty.int)
        | Type.Id { Id.ns = Sort; name = Simple "Real"; } ->
          Type.builtin_ty (Base.app0 (module Type) env s Ty.real)

        (* values *)
        | Type.Id { Id.ns = Value Integer; name = Simple name; } ->
          Type.builtin_term (Base.app0 (module Type) env s (T.Int.mk name))
        | Type.Id { Id.ns = Value Real; name = Simple name; } ->
          Type.builtin_term (Base.app0 (module Type) env s (T.Real.mk name))

        (* terms *)
        | Type.Id { Id.ns = Term; name = Simple name; } ->
          begin match name with
            | "-" -> Type.builtin_term (fun ast args ->
                match args with
                | [_] ->
                  Base.term_app1_ast (module Type) env s
                    (dispatch1 env (T.Int.minus, T.Real.minus)) ast args
                    ~check:(check1 config env (F.minus config (parse ~config) version env))
                | _ ->
                  Base.term_app_left_ast (module Type) env s
                    (dispatch2 env (T.Int.sub, T.Real.sub)) ast args
                    ~check:(check config env (F.sub config (parse ~config) version env))
              )
            | "+" ->
              Type.builtin_term (Base.term_app_left_ast (module Type) env s
                       (dispatch2 env (T.Int.add, T.Real.add))
                       ~check:(check config env (F.add config (parse ~config) version env)))
            | "*" ->
              Type.builtin_term (Base.term_app_left_ast (module Type) env s
                       (dispatch2 env (T.Int.mul, T.Real.mul))
                       ~check:(check config env (F.mul config (parse ~config) version env)))
            | "div" ->
              Type.builtin_term (Base.term_app_left (module Type) env s T.Int.div
                                   ~check:(check config env (F.ediv config version)))
                ~meta:(`Partial (fun _ _ _ -> T.Int.div'))
            | "mod" ->
              Type.builtin_term (Base.term_app2 (module Type) env s T.Int.rem
                                   ~check:(check2 config env (F.mod_ config version)))
                ~meta:(`Partial (fun _ _ _ -> T.Int.rem'))
            | "abs" ->
              Type.builtin_term (Base.term_app1 (module Type) env s T.Int.abs
                       ~check:(check1 config env (F.abs config version)))
            | "/" ->
              Type.builtin_term (Base.term_app_left_ast (module Type) env s
                       (promote_int_to_real env T.Real.div)
                       ~check:(check config env (F.div config (parse ~config) version env)))
                ~meta:(`Partial (fun _ _ _ -> T.Real.div'))
            | "<=" ->
              Type.builtin_term (Base.term_app_chain_ast (module Type) env s
                       (dispatch2 env (T.Int.le, T.Real.le))
                       ~check:(check config env (F.comp config (parse ~config) version env)))
            | "<" ->
              Type.builtin_term (Base.term_app_chain_ast (module Type) env s
                       (dispatch2 env (T.Int.lt, T.Real.lt))
                       ~check:(check config env (F.comp config (parse ~config) version env)))
            | ">=" ->
              Type.builtin_term (Base.term_app_chain_ast (module Type) env s
                       (dispatch2 env (T.Int.ge, T.Real.ge))
                       ~check:(check config env (F.comp config (parse ~config) version env)))
            | ">" ->
              Type.builtin_term (Base.term_app_chain_ast (module Type) env s
                       (dispatch2 env (T.Int.gt, T.Real.gt))
                       ~check:(check config env (F.comp config (parse ~config) version env)))
            | "to_real" ->
              Type.builtin_term (Base.term_app1 (module Type) env s T.Int.to_real)
            | "to_int" ->
              Type.builtin_term (Base.term_app1 (module Type) env s T.Real.floor_to_int)
            | "is_int" ->
              Type.builtin_term (Base.term_app1 (module Type) env s T.Real.is_int)


            | "/0" ->
              `Reserved (`Model (
                "completing interpretation of division by zero in models",
                `Partial (fun _ _ _ -> T.Real.div')))
            | "div0" ->
              `Reserved (`Model (
                "completing interpretation of division by zero in models",
                `Partial (fun _ _ _ -> T.Int.div')))
            | "mod0" ->
              `Reserved (`Model (
                "completing interpretation of modulo by zero in models",
                `Partial (fun _ _ _ -> T.Int.rem')))

            | _ -> `Not_found
          end
        | Type.Id { Id.ns = Term; name = Indexed { basename; indexes; }; } ->
          Base.parse_indexed basename indexes
            ~k:(function () -> `Not_found)
            ~err:(Base.bad_ty_index_arity (module Type) env) (function
                | "divisible" ->
                  `Unary (function n ->
                      Type.builtin_term (Base.term_app1 (module Type) env s (T.Int.divisible n)
                               ~check:(check1 config env (F.divisible config version))))
                | _ -> `Not_indexed
              )

        | _ -> `Not_found

    end

  end

end

(* TPTP arithmetic *)
(* ************************************************************************ *)

module Tptp = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Tff_Arith with type t := Type.T.t
                                        and type ty := Type.Ty.t) = struct

    type _ Type.err +=
      | Expected_arith_type : Type.Ty.t -> Term.t Type.err
      | Cannot_apply_to : Type.Ty.t -> Term.t Type.err

    let _invalid env ast ty _ =
      Type._error env (Ast ast) (Cannot_apply_to ty)

    let dispatch1 env (mk_int, mk_rat, mk_real) ast t =
      let ty = T.ty t in
      if Ty.(equal int) ty then mk_int t
      else if Ty.(equal rat) ty then mk_rat t
      else if Ty.(equal real) ty then mk_real t
      else begin
        Type._error env (Ast ast) (Expected_arith_type ty)
      end

    let dispatch2 env (mk_int, mk_rat, mk_real) ast a b =
      let ty = T.ty a in
      if Ty.(equal int) ty then mk_int a b
      else if Ty.(equal rat) ty then mk_rat a b
      else if Ty.(equal real) ty then mk_real a b
      else begin
        Type._error env (Ast ast) (Expected_arith_type ty)
      end

    let parse _version env s =
      match s with

      (* type *)
      | Type.Id { Id.ns = Term; name = Simple "$int"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.int)
      | Type.Id { Id.ns = Term; name = Simple "$rat"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.rat)
      | Type.Id { Id.ns = Term; name = Simple "$real"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.real)

      (* Literals *)
      | Type.Id { Id.ns = Value Integer; name = Simple name; } ->
        Type.builtin_term (Base.app0 (module Type) env s (T.int name))
      | Type.Id { Id.ns = Value Rational; name = Simple name; } ->
        Type.builtin_term (Base.app0 (module Type) env s (T.rat name))
      | Type.Id { Id.ns = Value Real; name = Simple name; } ->
        Type.builtin_term (Base.app0 (module Type) env s (T.real name))

      (* terms *)
      | Type.Id { Id.ns = Term; name = Simple "$less"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.lt, T.Rat.lt, T.Real.lt)))
      | Type.Id { Id.ns = Term; name = Simple "$lesseq"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.le, T.Rat.le, T.Real.le)))
      | Type.Id { Id.ns = Term; name = Simple "$greater"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.gt, T.Rat.gt, T.Real.gt)))
      | Type.Id { Id.ns = Term; name = Simple "$greatereq"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.ge, T.Rat.ge, T.Real.ge)))
      | Type.Id { Id.ns = Term; name = Simple "$uminus"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.minus, T.Rat.minus, T.Real.minus)))
      | Type.Id { Id.ns = Term; name = Simple "$sum"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.add, T.Rat.add, T.Real.add)))
      | Type.Id { Id.ns = Term; name = Simple "$difference"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.sub, T.Rat.sub, T.Real.sub)))
      | Type.Id { Id.ns = Term; name = Simple "$product"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.mul, T.Rat.mul, T.Real.mul)))
      | Type.Id { Id.ns = Term; name = Simple "$quotient"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s (fun ast a b ->
            (dispatch2 env (_invalid env ast Ty.int, T.Rat.div, T.Real.div)) ast a b
          ))
      | Type.Id { Id.ns = Term; name = Simple "$quotient_e"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.div_e, T.Rat.div_e, T.Real.div_e)))
      | Type.Id { Id.ns = Term; name = Simple "$remainder_e"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.rem_e, T.Rat.rem_e, T.Real.rem_e)))
      | Type.Id { Id.ns = Term; name = Simple "$quotient_t"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.div_t, T.Rat.div_t, T.Real.div_t)))
      | Type.Id { Id.ns = Term; name = Simple "$remainder_t"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.rem_t, T.Rat.rem_t, T.Real.rem_t)))
      | Type.Id { Id.ns = Term; name = Simple "$quotient_f"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.div_f, T.Rat.div_f, T.Real.div_f)))
      | Type.Id { Id.ns = Term; name = Simple "$remainder_f"; } ->
        Type.builtin_term (Base.term_app2_ast (module Type) env s
                 (dispatch2 env (T.Int.rem_f, T.Rat.rem_f, T.Real.rem_f)))
      | Type.Id { Id.ns = Term; name = Simple "$floor"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.floor, T.Rat.floor, T.Real.floor)))
      | Type.Id { Id.ns = Term; name = Simple "$ceiling"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.ceiling, T.Rat.ceiling, T.Real.ceiling)))
      | Type.Id { Id.ns = Term; name = Simple "$truncate"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.truncate, T.Rat.truncate, T.Real.truncate)))
      | Type.Id { Id.ns = Term; name = Simple "$round"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.round, T.Rat.round, T.Real.round)))
      | Type.Id { Id.ns = Term; name = Simple "$is_int"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.is_int, T.Rat.is_int, T.Real.is_int)))
      | Type.Id { Id.ns = Term; name = Simple "$is_rat"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.is_rat, T.Rat.is_rat, T.Real.is_rat)))
      | Type.Id { Id.ns = Term; name = Simple "$to_int"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.to_int, T.Rat.to_int, T.Real.to_int)))
      | Type.Id { Id.ns = Term; name = Simple "$to_rat"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.to_rat, T.Rat.to_rat, T.Real.to_rat)))
      | Type.Id { Id.ns = Term; name = Simple "$to_real"; } ->
        Type.builtin_term (Base.term_app1_ast (module Type) env s
                 (dispatch1 env (T.Int.to_real, T.Rat.to_real, T.Real.to_real)))

      (* Catch-all *)
      | _ -> `Not_found

  end

end

(* Zf arithmetic *)
(* ************************************************************************ *)

module Zf = struct

  module Thf
      (Type : Thf_intf.S)
      (Ty : Dolmen.Intf.Ty.Zf_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Zf_Arith with type t := Type.T.t) = struct

    let parse env s =
      match s with
      (* Types *)
      | Type.Builtin Term.Int ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.int)

      (* Literals *)
      | Type.Id { Id.ns = Value Integer; name = Simple name; } ->
        Type.builtin_term (Base.app0 (module Type) env s (T.int name))

      (* Arithmetic *)
      | Type.Builtin Term.Minus ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.Int.minus)
      | Type.Builtin Term.Add ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.Int.add)
      | Type.Builtin Term.Sub ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.Int.sub)
      | Type.Builtin Term.Mult ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.Int.mul)
      | Type.Builtin Term.Lt ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.Int.lt)
      | Type.Builtin Term.Leq ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.Int.le)
      | Type.Builtin Term.Gt ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.Int.gt)
      | Type.Builtin Term.Geq ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.Int.ge)

      (* Catch-all *)
      | _ -> `Not_found

  end
end
