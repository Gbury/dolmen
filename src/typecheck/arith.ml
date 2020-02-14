
open Dolmen

(* Smtlib arithmetic (integer and reals) *)
(* ************************************************************************ *)

module Smtlib = struct

  module Int = struct

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Int with type t := Type.T.t) = struct

      let app1 env ast args name mk =
        Base.make_op1 (module Type) env ast name args
          (fun t -> Type.Term (mk (Type.parse_term env t)))

      let app2 env ast args name mk =
        Base.make_op2 (module Type) env ast name args
          (fun (a, b) ->
             Type.Term (mk (Type.parse_term env a) (Type.parse_term env b)))

      let app_left env ast args name mk =
        Base.make_assoc (module Type) env ast name args
          (fun l -> Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env ast args name mk =
        Base.make_chain (module Type) env ast name args (fun l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let split_id = Dolmen_std.Misc.split_on_char '\000'

      let parse env ast s args =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          Base.make_op0 (module Type) env ast "Int" args
            (fun () -> Type.Ty Ty.int)
        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.int name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name, args with
            | "-", [x] ->
              Some (Type.Term (T.minus (Type.parse_term env x)))
            | "-", _ -> app_left env ast args "-" T.sub
            | "+", _ -> app_left env ast args "+" T.add
            | "*", _ -> app_left env ast args "*" T.mul
            | "div", _ -> app_left env ast args "div" T.div
            | "mod", _ -> app2 env ast args "mod" T.rem
            | "abs", _ -> app1 env ast args "abs" T.abs
            | "<=", _ -> app_chain env ast args "<=" T.le
            | "<", _ -> app_chain env ast args "<" T.lt
            | ">=", _ -> app_chain env ast args ">=" T.ge
            | ">", _ -> app_chain env ast args ">" T.gt
            | _ -> begin match split_id name with
                | "divisible" :: r ->
                  begin match r with
                    | [n] ->
                      Base.make_op1 (module Type) env ast "divisible" args
                        (fun t -> Type.Term (T.divisible n (Type.parse_term env t)))
                    | _ ->
                      let err = Type.Bad_op_arity ("divisible", 1, List.length r) in
                      raise (Type.Typing_error (err, env, ast))
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

      let app_left env ast args name mk =
        Base.make_assoc (module Type) env ast name args
          (fun l -> Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env ast args name mk =
        Base.make_chain (module Type) env ast name args (fun l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let parse env ast s args =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          Base.make_op0 (module Type) env ast "Real" args
            (fun () -> Type.Ty Ty.real)
        (* values *)
        | Type.Id { Id.ns = Id.Value (Id.Integer | Id.Real); name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.real name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name, args with
            | "-", [x] ->
              Some (Type.Term (T.minus (Type.parse_term env x)))
            | "-", _ -> app_left env ast args "-" T.sub
            | "+", _ -> app_left env ast args "+" T.add
            | "*", _ -> app_left env ast args "*" T.mul
            | "/", _ -> app_left env ast args "/" T.div
            | "<=", _ -> app_chain env ast args "<=" T.le
            | "<", _ -> app_chain env ast args "<" T.lt
            | ">=", _ -> app_chain env ast args ">=" T.ge
            | ">", _ -> app_chain env ast args ">" T.gt
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

      type Type.err +=
        | Expected_arith_type of Type.Ty.t

      let dispatch1 env ast (mk_int, mk_real) t =
        let ty = T.ty t in
        if Ty.(equal int) ty then mk_int t
        else if Ty.(equal real) ty then mk_real t
        else begin
          let err = Expected_arith_type ty in
          raise (Type.Typing_error (err, env, ast))
        end

      let dispatch2 env ast (mk_int, mk_real) a b =
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
          let err = Expected_arith_type a_ty in
          raise (Type.Typing_error (err, env, ast))
        end

      let promote_int_to_real _env _ast mk_real a b =
        let a_ty = T.ty a in
        let b_ty = T.ty b in
        let c, d =
          if Ty.(equal int a_ty) && Ty.(equal int b_ty) then
            (T.Int.to_real a), (T.Int.to_real b)
          else
            a, b
        in
        mk_real c d

      let app1 env ast args name mk =
        Base.make_op1 (module Type) env ast name args
        (fun t -> Type.Term (mk (Type.parse_term env t)))

      let app2 env ast args name mk =
        Base.make_op2 (module Type) env ast name args
          (fun (a, b) ->
             Type.Term (mk (Type.parse_term env a) (Type.parse_term env b)))

      let app_left env ast args name mk =
        Base.make_assoc (module Type) env ast name args
          (fun l -> Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env ast args name mk =
        Base.make_chain (module Type) env ast name args (fun l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let split_id = Dolmen_std.Misc.split_on_char '\000'

      let parse env ast s args =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          Base.make_op0 (module Type) env ast "Int" args
            (fun () -> Type.Ty Ty.int)
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          Base.make_op0 (module Type) env ast "Real" args
            (fun () -> Type.Ty Ty.real)
        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.Int.int name))
        | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.Real.real name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name, args with
            | "-", [_] ->
              app1 env ast args "-"
                (dispatch1 env ast (T.Int.minus, T.Real.minus))
            | "-", _ ->
              app_left env ast args "-"
                (dispatch2 env ast (T.Int.sub, T.Real.sub))
            | "+", _ ->
              app_left env ast args "+"
                (dispatch2 env ast (T.Int.add, T.Real.add))
            | "*", _ ->
              app_left env ast args "*"
                (dispatch2 env ast (T.Int.mul, T.Real.mul))
            | "div", _ -> app_left env ast args "div" T.Int.div
            | "mod", _ -> app2 env ast args "mod" T.Int.rem
            | "abs", _ -> app1 env ast args "abs" T.Int.abs
            | "/", _ ->
              app_left env ast args "/"
                (promote_int_to_real env ast T.Real.div)
            | "<=", _ ->
              app_chain env ast args "<="
                (dispatch2 env ast (T.Int.le, T.Real.le))
            | "<", _ ->
              app_chain env ast args "<"
                (dispatch2 env ast (T.Int.lt, T.Real.lt))
            | ">=", _ ->
              app_chain env ast args ">="
                (dispatch2 env ast (T.Int.ge, T.Real.ge))
            | ">", _ ->
              app_chain env ast args ">"
                (dispatch2 env ast (T.Int.gt, T.Real.gt))
            | _ -> begin match split_id name with
                | "divisible" :: r ->
                  begin match r with
                    | [n] ->
                      Base.make_op1 (module Type) env ast "divisible" args
                        (fun t -> Type.Term (T.Int.divisible n (Type.parse_term env t)))
                    | _ ->
                      let err = Type.Bad_op_arity ("divisible", 1, List.length r) in
                      raise (Type.Typing_error (err, env, ast))
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

    type Type.err +=
      | Expected_arith_type of Type.Ty.t
      | Cannot_apply_to of Type.Ty.t

    let _invalid env ast ty _ =
      raise (Type.Typing_error (Cannot_apply_to ty, env, ast))

    let dispatch1 env ast (mk_int, mk_rat, mk_real) t =
      let ty = T.ty t in
      if Ty.(equal int) ty then mk_int t
      else if Ty.(equal rat) ty then mk_rat t
      else if Ty.(equal real) ty then mk_real t
      else begin
        let err = Expected_arith_type ty in
        raise (Type.Typing_error (err, env, ast))
      end

    let dispatch2 env ast (mk_int, mk_rat, mk_real) a b =
      let ty = T.ty a in
      if Ty.(equal int) ty then mk_int a b
      else if Ty.(equal rat) ty then mk_rat a b
      else if Ty.(equal real) ty then mk_real a b
      else begin
        let err = Expected_arith_type ty in
        raise (Type.Typing_error (err, env, ast))
      end

    let app1 env ast args name mk =
      Base.make_op1 (module Type) env ast name args
        (fun t -> Type.Term (mk (Type.parse_term env t)))

    let app2 env ast args name mk =
      Base.make_op2 (module Type) env ast name args
        (fun (a, b) ->
           Type.Term (mk (Type.parse_term env a) (Type.parse_term env b)))

    let parse env ast s args =
      match s with
      (* type *)
      | Type.Id { Id.ns = Id.Term; name = "$int"; } ->
        Base.make_op0 (module Type) env ast "$int" args
            (fun () -> Type.Ty Ty.int)
      | Type.Id { Id.ns = Id.Term; name = "$rat"; } ->
        Base.make_op0 (module Type) env ast "$rat" args
            (fun () -> Type.Ty Ty.rat)
      | Type.Id { Id.ns = Id.Term; name = "$real"; } ->
        Base.make_op0 (module Type) env ast "$real" args
          (fun () -> Type.Ty Ty.real)
      (* Literals *)
      | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
        Base.make_op0 (module Type) env ast name args
          (fun () -> Type.Term (T.int name))
      | Type.Id { Id.ns = Id.Value Id.Rational; name; } ->
        Base.make_op0 (module Type) env ast name args
          (fun () -> Type.Term (T.rat name))
      | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
        Base.make_op0 (module Type) env ast name args
          (fun () -> Type.Term (T.real name))
      (* terms *)
      | Type.Id { Id.ns = Id.Term; name = "$less"; } ->
        app2 env ast args "$less"
          (dispatch2 env ast (T.Int.lt, T.Rat.lt, T.Real.lt))
      | Type.Id { Id.ns = Id.Term; name = "$lesseq"; } ->
        app2 env ast args "$lesseq"
          (dispatch2 env ast (T.Int.le, T.Rat.le, T.Real.le))
      | Type.Id { Id.ns = Id.Term; name = "$greater"; } ->
        app2 env ast args "$greater"
          (dispatch2 env ast (T.Int.gt, T.Rat.gt, T.Real.gt))
      | Type.Id { Id.ns = Id.Term; name = "$greatereq"; } ->
        app2 env ast args "$greatereq"
          (dispatch2 env ast (T.Int.ge, T.Rat.ge, T.Real.ge))
      | Type.Id { Id.ns = Id.Term; name = "$uminus"; } ->
        app1 env ast args "$uminus"
          (dispatch1 env ast (T.Int.minus, T.Rat.minus, T.Real.minus))
      | Type.Id { Id.ns = Id.Term; name = "$sum"; } ->
        app2 env ast args "$sum"
          (dispatch2 env ast (T.Int.add, T.Rat.add, T.Real.add))
      | Type.Id { Id.ns = Id.Term; name = "$difference"; } ->
        app2 env ast args "$difference"
          (dispatch2 env ast (T.Int.sub, T.Rat.sub, T.Real.sub))
      | Type.Id { Id.ns = Id.Term; name = "$product"; } ->
        app2 env ast args "$product"
          (dispatch2 env ast (T.Int.mul, T.Rat.mul, T.Real.mul))
      | Type.Id { Id.ns = Id.Term; name = "$quotient"; } ->
        app2 env ast args "$quotient"
          (dispatch2 env ast (_invalid env ast Ty.int, T.Rat.div, T.Real.div))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_e"; } ->
        app2 env ast args "$quotient_e"
          (dispatch2 env ast (T.Int.div_e, T.Rat.div_e, T.Real.div_e))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_e"; } ->
        app2 env ast args "$remainder_e"
          (dispatch2 env ast (T.Int.rem_e, T.Rat.rem_e, T.Real.rem_e))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_t"; } ->
        app2 env ast args "$quotient_t"
          (dispatch2 env ast (T.Int.div_t, T.Rat.div_t, T.Real.div_t))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_t"; } ->
        app2 env ast args "$remainder_t"
          (dispatch2 env ast (T.Int.rem_t, T.Rat.rem_t, T.Real.rem_t))
      | Type.Id { Id.ns = Id.Term; name = "$quotient_f"; } ->
        app2 env ast args "$quotient_f"
          (dispatch2 env ast (T.Int.div_f, T.Rat.div_f, T.Real.div_f))
      | Type.Id { Id.ns = Id.Term; name = "$remainder_f"; } ->
        app2 env ast args "$remainder_f"
          (dispatch2 env ast (T.Int.rem_f, T.Rat.rem_f, T.Real.rem_f))
      | Type.Id { Id.ns = Id.Term; name = "$floor"; } ->
        app1 env ast args "$floor"
          (dispatch1 env ast (T.Int.floor, T.Rat.floor, T.Real.floor))
      | Type.Id { Id.ns = Id.Term; name = "$ceiling"; } ->
        app1 env ast args "$ceiling"
          (dispatch1 env ast (T.Int.ceiling, T.Rat.ceiling, T.Real.ceiling))
      | Type.Id { Id.ns = Id.Term; name = "$truncate"; } ->
        app1 env ast args "$truncate"
          (dispatch1 env ast (T.Int.truncate, T.Rat.truncate, T.Real.truncate))
      | Type.Id { Id.ns = Id.Term; name = "$round"; } ->
        app1 env ast args "$round"
          (dispatch1 env ast (T.Int.round, T.Rat.round, T.Real.round))
      | Type.Id { Id.ns = Id.Term; name = "$is_int"; } ->
        app1 env ast args "$is_int"
          (dispatch1 env ast (T.Int.is_int, T.Rat.is_int, T.Real.is_int))
      | Type.Id { Id.ns = Id.Term; name = "$is_rat"; } ->
        app1 env ast args "$is_rat"
          (dispatch1 env ast (T.Int.is_rat, T.Rat.is_rat, T.Real.is_rat))
      | Type.Id { Id.ns = Id.Term; name = "$to_int"; } ->
        app1 env ast args "$to_int"
          (dispatch1 env ast (T.Int.to_int, T.Rat.to_int, T.Real.to_int))
      | Type.Id { Id.ns = Id.Term; name = "$to_rat"; } ->
        app1 env ast args "$to_rat"
          (dispatch1 env ast (T.Int.to_rat, T.Rat.to_rat, T.Real.to_rat))
      | Type.Id { Id.ns = Id.Term; name = "$to_real"; } ->
        app1 env ast args "$to_real"
          (dispatch1 env ast (T.Int.to_real, T.Rat.to_real, T.Real.to_real))

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
