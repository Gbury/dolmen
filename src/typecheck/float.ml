
module Id = Dolmen.Id
module Ast = Dolmen.Term

(* Smtlib Floating Point *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Float with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Float with type t := Type.T.t
                                          and type ty := Type.Ty.t) = struct

    module F = T.Float

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Term.t Type.err
      | Invalid_hex_char : char -> Dolmen.Term.t Type.err
      | Bitvector_litteral_expected : Dolmen.Term.t Type.err
      | Bitvector_of_size_one_expected : int -> Dolmen.Term.t Type.err
      | To_fp_incorrect_args : Dolmen.Term.t Type.err

    let parse_int env ast s =
      match int_of_string s with
      | i when i > 0 -> i
      | _ ->
        Type._error env (Ast ast)
          (Type.Expected ("a positive integer", None))
      | exception Failure _ ->
        Type._error env (Ast ast)
          (Type.Expected ("a positive integer", None))

    let parse_binary env ast s =
      match Misc.Bitv.parse_binary s with
      | s -> s
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_bin_char c)

    let parse_hexa env ast s =
      match Misc.Bitv.parse_binary s with
      | s -> s
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_hex_char c)

    let parse_bitv_lit env ast arg =
      match arg.Ast.term with
      | Ast.Symbol { Id.ns = Id.Value Id.Binary; name; } ->
        parse_binary env ast name
      | Ast.Symbol { Id.ns = Id.Value Id.Hexadecimal; name; } ->
        parse_hexa env ast name
      | _ ->
        Type._error env (Ast ast) Bitvector_litteral_expected

    let split_id = Dolmen_std.Misc.split_on_char '\000'

    let parse_id env id l k =
      let rec aux h r r_l = function
        | [] -> k (h :: r)
        | (s, n, f) :: l' ->
          if String.equal s h then begin
            if r_l = n then f r
            else begin
              Some (fun ast _args ->
                  Type._error env (Ast ast) (Type.Bad_op_arity (s, n, r_l))
                )
            end
          end else
            aux h r r_l l'
      in
      match split_id id with
      | h :: r -> aux h r (List.length r) l
      | r -> k r

    let app0 env name mk =
      Base.make_op0 (module Type) env name
        (fun ast () -> Type.Term (mk ast))

    let app1 env name mk =
      Base.make_op1 (module Type) env name (fun ast t ->
          Type.Term (mk ast (Type.parse_term env t)))

    let app2 env name mk =
      Base.make_op2 (module Type) env name (fun ast (a, b) ->
          Type.Term (mk ast (Type.parse_term env a) (Type.parse_term env b)))

    let app3 env name mk =
      Base.make_op3 (module Type) env name (fun ast (a, b, c) ->
          Type.Term (mk ast (Type.parse_term env a)
                       (Type.parse_term env b)
                       (Type.parse_term env c)))

    let app4 env name mk =
      Base.make_op4 (module Type) env name (fun ast (a, b, c, d) ->
          Type.Term (mk ast (Type.parse_term env a) (Type.parse_term env b)
                       (Type.parse_term env c) (Type.parse_term env d)))

    let app_chain env name mk =
      Base.make_chain (module Type) env name (fun ast l ->
          let l' = List.map (Type.parse_term env) l in
          Type.Term (Base.map_chain (module Type) (mk ast) l')
        )

    let fp_type env e s =
      Base.make_op0 (module Type) env "FloatingPoint"
        (fun _ast () -> Type.Ty (Ty.float e s))

    let app0_param2 env name mk = function
      | [n_e;n_s] ->
        Some (app0 env name (fun ast ->
            mk (parse_int env ast n_e) (parse_int env ast n_s)))
      | _ -> assert false

    let app2_param2 env name mk = function
      | [n_e;n_s] ->
        Some (app2 env name (fun ast ->
            mk (parse_int env ast n_e) (parse_int env ast n_s)))
      | _ -> assert false

    let app2_param1 env name mk = function
      | [n_m] ->
        Some (app2 env name (fun ast -> mk (parse_int env ast n_m)))
      | _ -> assert false

    let to_fp env lse = Some (fun ast args ->
      let e,s = match lse with
        | [e;s] -> (parse_int env ast e, parse_int env ast s)
        (* to_fp is specified below with 2 parameters *)
        | _ -> assert false
      in
      let args = List.map (Type.parse_term env) args in
      let return t = Type.Term t in
      match args with
      | [ a ] -> return (F.ieee_format_to_fp e s a)
      | [ rm; b ] -> begin
          match Ty.view @@ T.ty b with
          | `Real -> return (F.real_to_fp e s rm b)
          | `Bitv _ -> return (F.ubv_to_fp e s rm b)
          | `Float (_,_) -> return (F.fp_to_fp e s rm b)
          | _ -> Type._error env (Ast ast) To_fp_incorrect_args
        end
      | _ -> Type._error env (Ast ast) To_fp_incorrect_args
      )

    let wrap f = fun _ -> f

    let parse _version env s =
      match s with
      (* sort *)
      | Type.Id { Id.ns = Id.Sort; name; } ->
        parse_id env name [
          "FloatingPoint", 2, (function
              | [n_e;n_s] -> Some (fun ast args ->
                  fp_type env
                    (parse_int env ast n_e)
                    (parse_int env ast n_s)
                    ast args
                )
              | _ -> assert false);
        ] (function
            | ["Float16"] -> Some (fp_type env 5 11)
            | ["Float32"] -> Some (fp_type env 8 24)
            | ["Float64"] -> Some (fp_type env 11 53)
            | ["Float128"] -> Some (fp_type env 15 113)
            | ["RoundingMode"] ->
              Some (Base.make_op0 (module Type) env "RoundingMode"
                      (fun _ () -> Type.Ty Ty.roundingMode))
            | _ -> None)
      (* terms *)
      | Type.Id { Id.ns = Id.Term; name; } ->
        parse_id env name [
          "+oo", 2, app0_param2 env "plus_infinity" F.plus_infinity;
          "-oo", 2, app0_param2 env "minus_infinity" F.minus_infinity;
          "+zero", 2, app0_param2 env "plus_zero" F.plus_zero;
          "-zero", 2, app0_param2 env "minus_zero" F.minus_zero;
          "NaN", 2, app0_param2 env "nan" F.nan;
          "to_fp", 2, to_fp env;
          "to_fp_unsigned", 2, app2_param2 env "ubv_to_fp" F.ubv_to_fp;
          "fp.to_ubv", 1, app2_param1 env "to_ubv" F.to_ubv;
          "fp.to_sbv", 1, app2_param1 env "to_sbv" F.to_sbv;
        ] (function
            | ["fp"] ->
              Some (Base.make_op3 (module Type) env "fp" (fun ast (a,b,c) ->
                   let sa = parse_bitv_lit env ast a in
                   if String.length sa <> 1 then begin
                     Type._error env (Ast ast)
                       (Bitvector_of_size_one_expected (String.length sa))
                   end;
                   Type.Term (F.fp sa
                                (parse_bitv_lit env ast b)
                                (parse_bitv_lit env ast c))
                ))
            | ["roundNearestTiesToEven"] | ["RNE"] ->
              Some (app0 env "roundNearestTiesToEven"
                      (wrap F.roundNearestTiesToEven))
            | ["roundNearestTiesToAway"] | ["RNA"] ->
              Some (app0 env "roundNearestTiesToAway"
                      (wrap F.roundNearestTiesToAway))
            | ["roundTowardPositive"] | ["RTP"] ->
              Some (app0 env "roundTowardPositive"
                      (wrap F.roundTowardPositive))
            | ["roundTowardNegative"] | ["RTN"] ->
              Some (app0 env "roundTowardNegative"
                      (wrap F.roundTowardNegative))
            | ["roundTowardZero"] | ["RTZ"] ->
              Some (app0 env "roundTowardZero"
                      (wrap F.roundTowardZero))
            | ["fp.abs"] -> Some (app1 env "fp.abs" (wrap F.fp_abs))
            | ["fp.neg"] -> Some (app1 env "fp.neg" (wrap F.fp_neg))
            | ["fp.add"] -> Some (app3 env "fp.add" (wrap F.fp_add))
            | ["fp.sub"] -> Some (app3 env "fp.sub" (wrap F.fp_sub))
            | ["fp.mul"] -> Some (app3 env "fp.mul" (wrap F.fp_mul))
            | ["fp.div"] -> Some (app3 env "fp.div" (wrap F.fp_div))
            | ["fp.fma"] -> Some (app4 env "fp.fma" (wrap F.fp_fma))
            | ["fp.sqrt"] -> Some (app2 env "fp.sqrt" (wrap F.fp_sqrt))
            | ["fp.rem"] -> Some (app2 env "fp.rem" (wrap F.fp_rem))
            | ["fp.roundToIntegral"] ->
              Some (app2 env "fp.roundToIntegral" (wrap F.fp_roundToIntegral))
            | ["fp.min"] -> Some (app2 env "fp.min" (wrap F.fp_min))
            | ["fp.max"] -> Some (app2 env "fp.max" (wrap F.fp_max))
            | ["fp.leq"] -> Some (app_chain env "fp.leq" (wrap F.fp_leq))
            | ["fp.lt"] -> Some (app_chain env "fp.lt" (wrap F.fp_lt))
            | ["fp.geq"] -> Some (app_chain env "fp.geq" (wrap F.fp_geq))
            | ["fp.gt"] -> Some (app_chain env "fp.gt" (wrap F.fp_gt))
            | ["fp.eq"] -> Some (app_chain env "fp.eq" (wrap F.fp_eq))
            | ["fp.isNormal"] ->
              Some (app1 env "fp.isNormal" (wrap F.fp_isNormal))
            | ["fp.isSubnormal"] ->
              Some (app1 env "fp.isSubnormal" (wrap F.fp_isSubnormal))
            | ["fp.isZero"] ->
              Some (app1 env "fp.isZero" (wrap F.fp_isZero))
            | ["fp.isInfinite"] ->
              Some (app1 env "fp.isInfinite" (wrap F.fp_isInfinite))
            | ["fp.isNaN"] ->
              Some (app1 env "fp.isNaN" (wrap F.fp_isNaN))
            | ["fp.isNegative"] ->
              Some (app1 env "fp.isNegative" (wrap F.fp_isNegative))
            | ["fp.isPositive"] ->
              Some (app1 env "fp.isPositive" (wrap F.fp_isPositive))
            | ["fp.to_real"] ->
              Some (app1 env "fp.to_real" (wrap F.to_real))
            | _ -> None
          )
      | _ -> None

      end

end
