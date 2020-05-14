
open Dolmen

(* Smtlib arithmetic (integer and reals) *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Int = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Int with type t := Type.T.t) = struct

      let app1 env name mk =
        Base.make_op1 (module Type) env name (fun _ t ->
            Type.Term (mk (Type.parse_term env t)))

      let app2 env name mk =
        Base.make_op2 (module Type) env name (fun _ (a, b) ->
            Type.Term (mk (Type.parse_term env a) (Type.parse_term env b)))

      let app_left env name mk =
        Base.make_assoc (module Type) env name (fun _ l ->
            Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env name mk =
        Base.make_chain (module Type) env name (fun _ l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let split_id = Dolmen_std.Misc.split_on_char '\000'

      let parse _version env s =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          Some (Base.make_op0 (module Type) env "Int"
                  (fun _ () -> Type.Ty Ty.int))
        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          Some (Base.make_op0 (module Type) env name
                  (fun _ () -> Type.Term (T.int name)))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name with
            | "-" ->
              Some (fun ast args -> match args with
                  | [x] -> Type.Term (T.minus (Type.parse_term env x))
                  | _ -> app_left env "-" T.sub ast args
                )
            | "+" -> Some (app_left env "+" T.add)
            | "*" -> Some (app_left env "*" T.mul)
            | "div" -> Some (app_left env "div" T.div)
            | "mod" -> Some (app2 env "mod" T.rem)
            | "abs" -> Some (app1 env "abs" T.abs)
            | "<=" -> Some (app_chain env "<=" T.le)
            | "<" -> Some (app_chain env "<" T.lt)
            | ">=" -> Some (app_chain env ">=" T.ge)
            | ">" -> Some (app_chain env ">" T.gt)
            | _ -> begin match split_id name with
                | "divisible" :: r ->
                  begin match r with
                    | [n] ->
                      Some (Base.make_op1 (module Type) env "divisible"
                              (fun _ t -> Type.Term
                                  (T.divisible n (Type.parse_term env t))))
                    | _ ->
                      Some (fun ast _args ->
                          Type._error env (Ast ast)
                            (Type.Bad_op_arity ("divisible", 1, List.length r)))
                  end
                | _ -> None
              end
          end
        | _ -> None

    end
  end

  module Real = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real with type t := Type.T.t) = struct

      let app_left env name mk =
        Base.make_assoc (module Type) env name (fun _ l ->
            Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env name mk =
        Base.make_chain (module Type) env name (fun _ l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let parse _version env s =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          Some (Base.make_op0 (module Type) env "Real"
                  (fun _ () -> Type.Ty Ty.real))
        (* values *)
        | Type.Id { Id.ns = Id.Value (Id.Integer | Id.Real); name; } ->
          Some (Base.make_op0 (module Type) env name
                  (fun _ () -> Type.Term (T.real name)))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name with
            | "-" -> Some (fun ast args ->
                match args with
                | [x] -> Type.Term (T.minus (Type.parse_term env x))
                | _ -> app_left env "-" T.sub ast args
              )
            | "+" -> Some (app_left env "+" T.add)
            | "*" -> Some (app_left env "*" T.mul)
            | "/" -> Some (app_left env "/" T.div)
            | "<=" -> Some (app_chain env "<=" T.le)
            | "<" -> Some (app_chain env "<" T.lt)
            | ">=" -> Some (app_chain env ">=" T.ge)
            | ">" -> Some (app_chain env ">" T.gt)
            | _ -> None
          end
        | _ -> None

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
        let ty = T.ty t in
        if Ty.(equal int) ty then mk_int t
        else if Ty.(equal real) ty then mk_real t
        else begin
          Type._error env (Ast ast) (Expected_arith_type ty)
        end

      let dispatch2 env (mk_int, mk_real) ast a b =
        let a_ty = T.ty a in
        let b_ty = T.ty b in
        if Ty.(equal int) a_ty then
          if Ty.(equal real) b_ty then
            mk_real (T.Int.to_real a) b
          else
            mk_int a b
        else if Ty.(equal real) a_ty then
          if Ty.(equal int) b_ty then
            mk_real a (T.Int.to_real b)
          else
            mk_real a b
        else begin
          Type._error env (Ast ast) (Expected_arith_type a_ty)
        end

      let promote_int_to_real _env mk_real _ast a b =
        let a_ty = T.ty a in
        let b_ty = T.ty b in
        let c, d =
          if Ty.(equal int a_ty) && Ty.(equal int b_ty) then
            (T.Int.to_real a), (T.Int.to_real b)
          else
            a, b
        in
        mk_real c d

      let app1 env name mk =
        Base.make_op1 (module Type) env name (fun ast t ->
            Type.Term (mk ast (Type.parse_term env t)))

      let app2 env name mk =
        Base.make_op2 (module Type) env name (fun ast (a, b) ->
            Type.Term (mk ast (Type.parse_term env a) (Type.parse_term env b)))

      let app_left env name mk =
        Base.make_assoc (module Type) env name (fun ast l ->
            Type.Term (Base.fold_left_assoc (mk ast)
                         (List.map (Type.parse_term env) l)))

      let app_chain env name mk =
        Base.make_chain (module Type) env name (fun ast l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) (mk ast) l')
          )

      let split_id = Dolmen_std.Misc.split_on_char '\000'

      let wrap f = fun _ -> f

      let parse _version env s =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          Some (Base.make_op0 (module Type) env "Int"
                  (fun _ () -> Type.Ty Ty.int))
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          Some (Base.make_op0 (module Type) env "Real"
                  (fun _ () -> Type.Ty Ty.real))
        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          Some (Base.make_op0 (module Type) env name
                  (fun _ () -> Type.Term (T.Int.int name)))
        | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
          Some (Base.make_op0 (module Type) env name
                  (fun _ () -> Type.Term (T.Real.real name)))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name with
            | "-" -> Some (fun ast args ->
                match args with
                | [_] ->
                  app1 env "-"
                    (dispatch1 env (T.Int.minus, T.Real.minus))
                    ast args
                | _ ->
                  app_left env "-"
                    (dispatch2 env (T.Int.sub, T.Real.sub))
                    ast args
              )
            | "+" ->
              Some (app_left env "+"
                      (dispatch2 env (T.Int.add, T.Real.add)))
            | "*" ->
              Some (app_left env "*"
                      (dispatch2 env (T.Int.mul, T.Real.mul)))
            | "div" -> Some (app_left env "div" (wrap T.Int.div))
            | "mod" -> Some (app2 env "mod" (wrap T.Int.rem))
            | "abs" -> Some (app1 env "abs" (wrap T.Int.abs))
            | "/" ->
              Some (app_left env "/"
                      (promote_int_to_real env T.Real.div))
            | "<=" ->
              Some (app_chain env "<="
                      (dispatch2 env (T.Int.le, T.Real.le)))
            | "<" ->
              Some (app_chain env "<"
                      (dispatch2 env (T.Int.lt, T.Real.lt)))
            | ">=" ->
              Some (app_chain env ">="
                      (dispatch2 env (T.Int.ge, T.Real.ge)))
            | ">" ->
              Some (app_chain env ">"
                      (dispatch2 env (T.Int.gt, T.Real.gt)))
            | _ -> begin match split_id name with
                | "divisible" :: r ->
                  begin match r with
                    | [n] ->
                      Some (Base.make_op1 (module Type) env "divisible"
                              (fun _ t -> Type.Term
                                  (T.Int.divisible n (Type.parse_term env t))))
                    | _ ->
                      Some (fun ast _ ->
                          Type._error env (Ast ast)
                            (Type.Bad_op_arity ("divisible", 1, List.length r)))
                  end
                | _ -> None
              end
          end
        | _ -> None

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

    let app1 env name mk =
      Base.make_op1 (module Type) env name
        (fun ast t -> Type.Term (mk ast (Type.parse_term env t)))

    let app2 env name mk =
      Base.make_op2 (module Type) env name (fun ast (a, b) ->
          Type.Term (mk ast (Type.parse_term env a) (Type.parse_term env b)))

    let parse _version env s =
      match s with
      (* type *)
      | Type.Id { Id.ns = Id.Term; name = "$int"; } ->
        Some (Base.make_op0 (module Type) env "$int"
                (fun _ () -> Type.Ty Ty.int))
      | Type.Id { Id.ns = Id.Term; name = "$rat"; } ->
        Some (Base.make_op0 (module Type) env "$rat"
                (fun _ () -> Type.Ty Ty.rat))
      | Type.Id { Id.ns = Id.Term; name = "$real"; } ->
        Some (Base.make_op0 (module Type) env "$real"
                (fun _ () -> Type.Ty Ty.real))
      (* Literals *)
      | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
        Some (Base.make_op0 (module Type) env name
                (fun _ () -> Type.Term (T.int name)))
      | Type.Id { Id.ns = Id.Value Id.Rational; name; } ->
        Some (Base.make_op0 (module Type) env name
                (fun _ () -> Type.Term (T.rat name)))
      | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
        Some (Base.make_op0 (module Type) env name
                (fun _ () -> Type.Term (T.real name)))
      (* terms *)
      | Type.Id { Id.ns = Id.Term; name = "$less"; } ->
        Some (app2 env "$less"
                (dispatch2 env (T.Int.lt, T.Rat.lt, T.Real.lt)))
      | Type.Id { Id.ns = Id.Term; name = "$lesseq"; } ->
        Some (app2 env "$lesseq"
                (dispatch2 env (T.Int.le, T.Rat.le, T.Real.le)))
      | Type.Id { Id.ns = Id.Term; name = "$greater"; } ->
        Some (app2 env "$greater"
                (dispatch2 env (T.Int.gt, T.Rat.gt, T.Real.gt)))
      | Type.Id { Id.ns = Id.Term; name = "$greatereq"; } ->
        Some (app2 env "$greatereq"
                (dispatch2 env (T.Int.ge, T.Rat.ge, T.Real.ge)))
      | Type.Id { Id.ns = Id.Term; name = "$uminus"; } ->
        Some (app1 env "$uminus"
                (dispatch1 env (T.Int.minus, T.Rat.minus, T.Real.minus)))
      | Type.Id { Id.ns = Id.Term; name = "$sum"; } ->
        Some (app2 env "$sum"
                (dispatch2 env (T.Int.add, T.Rat.add, T.Real.add)))
      | Type.Id { Id.ns = Id.Term; name = "$difference"; } ->
        Some (app2 env "$difference"
                (dispatch2 env (T.Int.sub, T.Rat.sub, T.Real.sub)))
      | Type.Id { Id.ns = Id.Term; name = "$product"; } ->
        Some (app2 env "$product"
                (dispatch2 env (T.Int.mul, T.Rat.mul, T.Real.mul)))
      | Type.Id { Id.ns = Id.Term; name = "$quotient"; } ->
        Some (fun ast args ->
            app2 env "$quotient"
              (dispatch2 env (_invalid env ast Ty.int, T.Rat.div, T.Real.div))
              ast args
          )
      | Type.Id { Id.ns = Id.Term; name = "$quotient_e"; } ->
        Some (app2 env "$quotient_e"
                (dispatch2 env (T.Int.div_e, T.Rat.div_e, T.Real.div_e)))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_e"; } ->
        Some (app2 env "$remainder_e"
                (dispatch2 env (T.Int.rem_e, T.Rat.rem_e, T.Real.rem_e)))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_t"; } ->
        Some (app2 env "$quotient_t"
                (dispatch2 env (T.Int.div_t, T.Rat.div_t, T.Real.div_t)))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_t"; } ->
        Some (app2 env "$remainder_t"
                (dispatch2 env (T.Int.rem_t, T.Rat.rem_t, T.Real.rem_t)))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_f"; } ->
        Some (app2 env "$quotient_f"
                (dispatch2 env (T.Int.div_f, T.Rat.div_f, T.Real.div_f)))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_f"; } ->
        Some (app2 env "$remainder_f"
                (dispatch2 env (T.Int.rem_f, T.Rat.rem_f, T.Real.rem_f)))
      | Type.Id { Id.ns = Id.Term; name = "$floor"; } ->
        Some (app1 env "$floor"
                (dispatch1 env (T.Int.floor, T.Rat.floor, T.Real.floor)))
      | Type.Id { Id.ns = Id.Term; name = "$ceiling"; } ->
        Some (app1 env "$ceiling"
                (dispatch1 env (T.Int.ceiling, T.Rat.ceiling, T.Real.ceiling)))
      | Type.Id { Id.ns = Id.Term; name = "$truncate"; } ->
        Some (app1 env "$truncate"
                (dispatch1 env (T.Int.truncate, T.Rat.truncate, T.Real.truncate)))
      | Type.Id { Id.ns = Id.Term; name = "$round"; } ->
        Some (app1 env "$round"
                (dispatch1 env (T.Int.round, T.Rat.round, T.Real.round)))
      | Type.Id { Id.ns = Id.Term; name = "$is_int"; } ->
        Some (app1 env "$is_int"
                (dispatch1 env (T.Int.is_int, T.Rat.is_int, T.Real.is_int)))
      | Type.Id { Id.ns = Id.Term; name = "$is_rat"; } ->
        Some (app1 env "$is_rat"
                (dispatch1 env (T.Int.is_rat, T.Rat.is_rat, T.Real.is_rat)))
      | Type.Id { Id.ns = Id.Term; name = "$to_int"; } ->
        Some (app1 env "$to_int"
                (dispatch1 env (T.Int.to_int, T.Rat.to_int, T.Real.to_int)))
      | Type.Id { Id.ns = Id.Term; name = "$to_rat"; } ->
        Some (app1 env "$to_rat"
                (dispatch1 env (T.Int.to_rat, T.Rat.to_rat, T.Real.to_rat)))
      | Type.Id { Id.ns = Id.Term; name = "$to_real"; } ->
        Some (app1 env "$to_real"
                (dispatch1 env (T.Int.to_real, T.Rat.to_real, T.Real.to_real)))

      (* Catch-all *)
      | _ -> None

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
