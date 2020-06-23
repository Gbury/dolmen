
open Dolmen

(* Smtlib arithmetic (integer and reals) *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Int = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Int with type t := Type.T.t) = struct

      let parse _version env s =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          `Ty (Base.app0 (module Type) env "Int" Ty.int)
        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          `Term (Base.app0 (module Type) env name (T.mk name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name with
            | "-" ->
              `Term (fun ast args -> match args with
                  | [x] -> T.minus (Type.parse_term env x)
                  | _ -> Base.term_app_left (module Type) env "-" T.sub ast args
                )
            | "+" -> `Term (Base.term_app_left (module Type) env "+" T.add)
            | "*" -> `Term (Base.term_app_left (module Type) env "*" T.mul)
            | "div" -> `Term (Base.term_app_left (module Type) env "div" T.div)
            | "mod" -> `Term (Base.term_app2 (module Type) env "mod" T.rem)
            | "abs" -> `Term (Base.term_app1 (module Type) env "abs" T.abs)
            | "<=" -> `Term (Base.term_app_chain (module Type) env "<=" T.le)
            | "<" -> `Term (Base.term_app_chain (module Type) env "<" T.lt)
            | ">=" -> `Term (Base.term_app_chain (module Type) env ">=" T.ge)
            | ">" -> `Term (Base.term_app_chain (module Type) env ">" T.gt)
            | _ -> Base.parse_id name
                     ~k:(function _ -> `Not_found)
                     ~err:(Base.bad_ty_index_arity (module Type) env)
                     [
                       "divisible", `Unary (function n ->
                           `Term (Base.term_app1 (module Type) env
                                    "divisible" (T.divisible n)));
                     ]
          end
        | _ -> `Not_found

    end
  end

  module Real = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real with type t := Type.T.t) = struct

      let parse _version env s =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          `Ty (Base.app0 (module Type) env "Real" Ty.real)
        (* values *)
        | Type.Id { Id.ns = Id.Value (Id.Integer | Id.Real); name; } ->
          `Term (Base.app0 (module Type) env name (T.mk name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name with
            | "-" -> `Term (fun ast args ->
                match args with
                | [x] -> T.minus (Type.parse_term env x)
                | _ -> Base.term_app_left (module Type) env "-" T.sub ast args
              )
            | "+" -> `Term (Base.term_app_left (module Type) env "+" T.add)
            | "*" -> `Term (Base.term_app_left (module Type) env "*" T.mul)
            | "/" -> `Term (Base.term_app_left (module Type) env "/" T.div)
            | "<=" -> `Term (Base.term_app_chain (module Type) env "<=" T.le)
            | "<" -> `Term (Base.term_app_chain (module Type) env "<" T.lt)
            | ">=" -> `Term (Base.term_app_chain (module Type) env ">=" T.ge)
            | ">" -> `Term (Base.term_app_chain (module Type) env ">" T.gt)
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
                                               and type ty := Type.Ty.t) = struct

      type _ Type.err +=
        | Expected_arith_type : Type.Ty.t -> Term.t Type.err

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

      let parse _version env s =
        match s with

        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          `Ty (Base.app0 (module Type) env "Int" Ty.int)
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          `Ty (Base.app0 (module Type) env "Real" Ty.real)

        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          `Term (Base.app0 (module Type) env name (T.Int.mk name))
        | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
          `Term (Base.app0 (module Type) env name (T.Real.mk name))

        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name with
            | "-" -> `Term (fun ast args ->
                match args with
                | [_] ->
                  Base.term_app1_ast (module Type) env "-"
                    (dispatch1 env (T.Int.minus, T.Real.minus))
                    ast args
                | _ ->
                  Base.term_app_left_ast (module Type) env "-"
                    (dispatch2 env (T.Int.sub, T.Real.sub))
                    ast args
              )
            | "+" ->
              `Term (Base.term_app_left_ast (module Type) env "+"
                       (dispatch2 env (T.Int.add, T.Real.add)))
            | "*" ->
              `Term (Base.term_app_left_ast (module Type) env "*"
                       (dispatch2 env (T.Int.mul, T.Real.mul)))
            | "div" ->
              `Term (Base.term_app_left (module Type) env "div" T.Int.div)
            | "mod" ->
              `Term (Base.term_app2 (module Type) env "mod" T.Int.rem)
            | "abs" ->
              `Term (Base.term_app1 (module Type) env "abs" T.Int.abs)
            | "/" ->
              `Term (Base.term_app_left_ast (module Type) env "/"
                       (promote_int_to_real env T.Real.div))
            | "<=" ->
              `Term (Base.term_app_chain_ast (module Type) env "<="
                       (dispatch2 env (T.Int.le, T.Real.le)))
            | "<" ->
              `Term (Base.term_app_chain_ast (module Type) env "<"
                       (dispatch2 env (T.Int.lt, T.Real.lt)))
            | ">=" ->
              `Term (Base.term_app_chain_ast (module Type) env ">="
                       (dispatch2 env (T.Int.ge, T.Real.ge)))
            | ">" ->
              `Term (Base.term_app_chain_ast (module Type) env ">"
                       (dispatch2 env (T.Int.gt, T.Real.gt)))
            | _ -> Base.parse_id name
                     ~k:(function _ -> `Not_found)
                     ~err:(Base.bad_ty_index_arity (module Type) env)
                     [
                       "divisible", `Unary (function n ->
                           `Term (Base.term_app1 (module Type) env
                                    "divisible" (T.Int.divisible n)));
                     ]
          end
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
      (T : Dolmen.Intf.Term.Tptp_Arith with type t := Type.T.t
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
      | Type.Id { Id.ns = Id.Term; name = "$int"; } ->
        `Ty (Base.app0 (module Type) env "$int" Ty.int)
      | Type.Id { Id.ns = Id.Term; name = "$rat"; } ->
        `Ty (Base.app0 (module Type) env "$rat" Ty.rat)
      | Type.Id { Id.ns = Id.Term; name = "$real"; } ->
        `Ty (Base.app0 (module Type) env "$real" Ty.real)

      (* Literals *)
      | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
        `Term (Base.app0 (module Type) env name (T.int name))
      | Type.Id { Id.ns = Id.Value Id.Rational; name; } ->
        `Term (Base.app0 (module Type) env name (T.rat name))
      | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
        `Term (Base.app0 (module Type) env name (T.real name))

      (* terms *)
      | Type.Id { Id.ns = Id.Term; name = "$less"; } ->
        `Term (Base.term_app2_ast (module Type) env "$less"
                 (dispatch2 env (T.Int.lt, T.Rat.lt, T.Real.lt)))
      | Type.Id { Id.ns = Id.Term; name = "$lesseq"; } ->
        `Term (Base.term_app2_ast (module Type) env "$lesseq"
                 (dispatch2 env (T.Int.le, T.Rat.le, T.Real.le)))
      | Type.Id { Id.ns = Id.Term; name = "$greater"; } ->
        `Term (Base.term_app2_ast (module Type) env "$greater"
                 (dispatch2 env (T.Int.gt, T.Rat.gt, T.Real.gt)))
      | Type.Id { Id.ns = Id.Term; name = "$greatereq"; } ->
        `Term (Base.term_app2_ast (module Type) env "$greatereq"
                 (dispatch2 env (T.Int.ge, T.Rat.ge, T.Real.ge)))
      | Type.Id { Id.ns = Id.Term; name = "$uminus"; } ->
        `Term (Base.term_app1_ast (module Type) env "$uminus"
                 (dispatch1 env (T.Int.minus, T.Rat.minus, T.Real.minus)))
      | Type.Id { Id.ns = Id.Term; name = "$sum"; } ->
        `Term (Base.term_app2_ast (module Type) env "$sum"
                 (dispatch2 env (T.Int.add, T.Rat.add, T.Real.add)))
      | Type.Id { Id.ns = Id.Term; name = "$difference"; } ->
        `Term (Base.term_app2_ast (module Type) env "$difference"
                 (dispatch2 env (T.Int.sub, T.Rat.sub, T.Real.sub)))
      | Type.Id { Id.ns = Id.Term; name = "$product"; } ->
        `Term (Base.term_app2_ast (module Type) env "$product"
                 (dispatch2 env (T.Int.mul, T.Rat.mul, T.Real.mul)))
      | Type.Id { Id.ns = Id.Term; name = "$quotient"; } ->
        `Term (Base.term_app2_ast (module Type) env "$quotient" (fun ast a b ->
            (dispatch2 env (_invalid env ast Ty.int, T.Rat.div, T.Real.div)) ast a b
          ))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_e"; } ->
        `Term (Base.term_app2_ast (module Type) env "$quotient_e"
                 (dispatch2 env (T.Int.div_e, T.Rat.div_e, T.Real.div_e)))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_e"; } ->
        `Term (Base.term_app2_ast (module Type) env "$remainder_e"
                 (dispatch2 env (T.Int.rem_e, T.Rat.rem_e, T.Real.rem_e)))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_t"; } ->
        `Term (Base.term_app2_ast (module Type) env "$quotient_t"
                 (dispatch2 env (T.Int.div_t, T.Rat.div_t, T.Real.div_t)))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_t"; } ->
        `Term (Base.term_app2_ast (module Type) env "$remainder_t"
                 (dispatch2 env (T.Int.rem_t, T.Rat.rem_t, T.Real.rem_t)))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_f"; } ->
        `Term (Base.term_app2_ast (module Type) env "$quotient_f"
                 (dispatch2 env (T.Int.div_f, T.Rat.div_f, T.Real.div_f)))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_f"; } ->
        `Term (Base.term_app2_ast (module Type) env "$remainder_f"
                 (dispatch2 env (T.Int.rem_f, T.Rat.rem_f, T.Real.rem_f)))
      | Type.Id { Id.ns = Id.Term; name = "$floor"; } ->
        `Term (Base.term_app1_ast (module Type) env "$floor"
                 (dispatch1 env (T.Int.floor, T.Rat.floor, T.Real.floor)))
      | Type.Id { Id.ns = Id.Term; name = "$ceiling"; } ->
        `Term (Base.term_app1_ast (module Type) env "$ceiling"
                 (dispatch1 env (T.Int.ceiling, T.Rat.ceiling, T.Real.ceiling)))
      | Type.Id { Id.ns = Id.Term; name = "$truncate"; } ->
        `Term (Base.term_app1_ast (module Type) env "$truncate"
                 (dispatch1 env (T.Int.truncate, T.Rat.truncate, T.Real.truncate)))
      | Type.Id { Id.ns = Id.Term; name = "$round"; } ->
        `Term (Base.term_app1_ast (module Type) env "$round"
                 (dispatch1 env (T.Int.round, T.Rat.round, T.Real.round)))
      | Type.Id { Id.ns = Id.Term; name = "$is_int"; } ->
        `Term (Base.term_app1_ast (module Type) env "$is_int"
                 (dispatch1 env (T.Int.is_int, T.Rat.is_int, T.Real.is_int)))
      | Type.Id { Id.ns = Id.Term; name = "$is_rat"; } ->
        `Term (Base.term_app1_ast (module Type) env "$is_rat"
                 (dispatch1 env (T.Int.is_rat, T.Rat.is_rat, T.Real.is_rat)))
      | Type.Id { Id.ns = Id.Term; name = "$to_int"; } ->
        `Term (Base.term_app1_ast (module Type) env "$to_int"
                 (dispatch1 env (T.Int.to_int, T.Rat.to_int, T.Real.to_int)))
      | Type.Id { Id.ns = Id.Term; name = "$to_rat"; } ->
        `Term (Base.term_app1_ast (module Type) env "$to_rat"
                 (dispatch1 env (T.Int.to_rat, T.Rat.to_rat, T.Real.to_rat)))
      | Type.Id { Id.ns = Id.Term; name = "$to_real"; } ->
        `Term (Base.term_app1_ast (module Type) env "$to_real"
                 (dispatch1 env (T.Int.to_real, T.Rat.to_real, T.Real.to_real)))

      (* Catch-all *)
      | _ -> `Not_found

  end

end

(* Ae arithmetic *)
(* ************************************************************************ *)

module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Arith with type t := Type.T.t
                                        and type ty := Type.Ty.t) = struct

  end
end
