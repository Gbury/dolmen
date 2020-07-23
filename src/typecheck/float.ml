
module Id = Dolmen.Std.Id
module Ast = Dolmen.Std.Term

(* Smtlib Floating Point *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Float with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Float with type t := Type.T.t
                                          and type ty := Type.Ty.t) = struct

    module B = T.Bitv
    module R = T.Real
    module F = T.Float

    type _ Type.warn +=
      | Real_lit : Ast.t Type.warn
      | Bitv_extended_lit : Ast.t Type.warn

    type _ Type.err +=
      | Invalid_bin_char : char -> Ast.t Type.err
      | Invalid_hex_char : char -> Ast.t Type.err
      | Invalid_dec_char : char -> Ast.t Type.err

    let parse_int env ast s =
      match int_of_string s with
      | i when i >= 0 -> i
      | _ ->
        Type._error env (Ast ast)
          (Type.Expected ("a positive integer", None))
      | exception Failure _ ->
        Type._error env (Ast ast)
          (Type.Expected ("a positive integer", None))

    let parse_binary env s ast =
      match Misc.Bitv.parse_binary s with
      | s -> B.mk s
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_bin_char c)

    let parse_hexa env s ast =
      match Misc.Bitv.parse_hexa s with
      | s -> B.mk s
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_hex_char c)

    let parse_extended_lit env s n =
      Base.make_op0 (module Type) env s (fun ast () ->
          assert (String.length s >= 2);
          let n = parse_int env ast n in
          match Misc.Bitv.parse_decimal s n with
          | s -> B.mk s
          | exception Misc.Bitv.Invalid_char c ->
            Type._error env (Ast ast) (Invalid_dec_char c)
        )

    let indexed1 env mk i_s ast =
      let i = parse_int env ast i_s in
      mk i

    let indexed2 env mk i_s j_s ast =
      let i = parse_int env ast i_s in
      let j = parse_int env ast j_s in
      mk i j

    let to_fp env e s = `Term (fun ast args ->
        let e = parse_int env ast e in
        let s = parse_int env ast s in
        let args = List.map (Type.parse_term env) args in
        match args with
        | [ a ] -> F.ieee_format_to_fp e s a
        | [ rm; b ] -> begin
            match Ty.view @@ T.ty b with
            | `Real -> F.real_to_fp e s rm b
            | `Bitv _ -> F.sbv_to_fp e s rm b
            | `Float (_,_) -> F.to_fp e s rm b
            | _ -> Type._error env (Ast ast) (
                Type.Expected ("a real, bitvector or float", Some (Term b)))
          end
        | _ -> Type._error env (Ast ast)
                 (Type.Bad_op_arity ("to_fp", [1; 2], List.length args))
      )

    let parse _version env s =
      match s with

      (* sort *)
      | Type.Id ({ Id.ns = Id.Sort; _ } as id) ->
        Base.parse_id id [
          "BitVec", `Unary (function n_s ->
              `Ty (Base.app0_ast (module Type) env "BitVec"
                     (indexed1 env Ty.bitv n_s)));
          "FloatingPoint", `Binary (fun n_e n_s ->
              `Ty (Base.app0_ast (module Type) env "FloatingPoint"
                     (indexed2 env Ty.float n_e n_s)));
        ] ~err:(Base.bad_ty_index_arity (module Type) env)
          ~k:(function
            | ["Float16"] ->
              `Ty (Base.app0 (module Type) env "Float16" (Ty.float 5 11))
            | ["Float32"] ->
              `Ty (Base.app0 (module Type) env "Float32" (Ty.float 8 24))
            | ["Float64"] ->
              `Ty (Base.app0 (module Type) env "Float64" (Ty.float 11 53))
            | ["Float128"] ->
              `Ty (Base.app0 (module Type) env "Float128" (Ty.float 15 113))
            | ["RoundingMode"] ->
              `Ty (Base.app0 (module Type) env "RoundingMode" Ty.roundingMode)
            | _ ->
              `Not_found)

      (* Bitvector litterals *)
      | Type.Id { Id.ns = Id.Value Id.Binary; name; } ->
        `Term (Base.app0_ast (module Type) env name (parse_binary env name))
      | Type.Id { Id.ns = Id.Value Id.Hexadecimal; name; } ->
        `Term (Base.app0_ast (module Type) env name (parse_hexa env name))
      (* Added with a warning for compatibility *)
      | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
        `Term (fun ast args ->
            Type._warn env (Ast ast) Real_lit;
            Base.app0 (module Type) env name (R.mk name) ast args)

      (* terms *)
      | Type.Id ({ Id.ns = Id.Term; name; } as id) ->
        begin match name with
          | "fp" ->
            `Term (Base.term_app3 (module Type) env name F.fp)
          | "RNE" | "roundNearestTiesToEven" ->
            `Term (Base.app0 (module Type) env name F.roundNearestTiesToEven)
          | "RNA" | "roundNearestTiesToAway" ->
            `Term (Base.app0 (module Type) env name F.roundNearestTiesToAway)
          | "RTP" | "roundTowardPositive" ->
            `Term (Base.app0 (module Type) env name F.roundTowardPositive)
          | "RTN" | "roundTowardNegative" ->
            `Term (Base.app0 (module Type) env name F.roundTowardNegative)
          | "RTZ" | "roundTowardZero" ->
            `Term (Base.app0 (module Type) env name F.roundTowardZero)
          | "fp.abs" ->
            `Term (Base.term_app1 (module Type) env name F.abs)
          | "fp.neg" ->
            `Term (Base.term_app1 (module Type) env name F.neg)
          | "fp.add" ->
            `Term (Base.term_app3 (module Type) env name F.add)
          | "fp.sub" ->
            `Term (Base.term_app3 (module Type) env name F.sub)
          | "fp.mul" ->
            `Term (Base.term_app3 (module Type) env name F.mul)
          | "fp.div" ->
            `Term (Base.term_app3 (module Type) env name F.div)
          | "fp.fma" ->
            `Term (Base.term_app4 (module Type) env name F.fma)
          | "fp.sqrt" ->
            `Term (Base.term_app2 (module Type) env name F.sqrt)
          | "fp.rem" ->
            `Term (Base.term_app2 (module Type) env name F.rem)
          | "fp.roundToIntegral" ->
            `Term (Base.term_app2 (module Type) env name F.roundToIntegral)
          | "fp.min" ->
            `Term (Base.term_app2 (module Type) env name F.min)
          | "fp.max" ->
            `Term (Base.term_app2 (module Type) env name F.max)
          | "fp.leq" ->
            `Term (Base.term_app_chain (module Type) env name F.leq)
          | "fp.lt" ->
            `Term (Base.term_app_chain (module Type) env name F.lt)
          | "fp.geq" ->
            `Term (Base.term_app_chain (module Type) env name F.geq)
          | "fp.gt" ->
            `Term (Base.term_app_chain (module Type) env name F.gt)
          | "fp.eq" ->
            `Term (Base.term_app_chain (module Type) env name F.eq)
          | "fp.isNormal" ->
            `Term (Base.term_app1 (module Type) env name F.isNormal)
          | "fp.isSubnormal" ->
            `Term (Base.term_app1 (module Type) env name F.isSubnormal)
          | "fp.isZero" ->
            `Term (Base.term_app1 (module Type) env name F.isZero)
          | "fp.isInfinite" ->
            `Term (Base.term_app1 (module Type) env name F.isInfinite)
          | "fp.isNaN" ->
            `Term (Base.term_app1 (module Type) env name F.isNaN)
          | "fp.isNegative" ->
            `Term (Base.term_app1 (module Type) env name F.isNegative)
          | "fp.isPositive" ->
            `Term (Base.term_app1 (module Type) env name F.isPositive)
          | "fp.to_real" ->
            `Term (Base.term_app1 (module Type) env name F.to_real)
          | _ -> Base.parse_id id [
              "+oo", `Binary (fun e s ->
                  `Term (Base.app0_ast (module Type) env "plus_infinity"
                           (indexed2 env F.plus_infinity e s)));
              "-oo", `Binary (fun e s ->
                  `Term (Base.app0_ast (module Type) env "minus_infinity"
                           (indexed2 env F.minus_infinity e s)));
              "+zero", `Binary (fun e s ->
                  `Term (Base.app0_ast (module Type) env "plus_zero"
                           (indexed2 env F.plus_zero e s)));
              "-zero", `Binary (fun e s ->
                  `Term (Base.app0_ast (module Type) env "minus_zero"
                           (indexed2 env F.minus_zero e s)));
              "NaN", `Binary (fun e s ->
                  `Term (Base.app0_ast (module Type) env "nan"
                           (indexed2 env F.nan e s)));
              "to_fp", `Binary (to_fp env);
              "to_fp_unsigned", `Binary (fun e s ->
                  `Term (Base.term_app2_ast (module Type) env "ubv_to_fp"
                           (indexed2 env F.ubv_to_fp e s)));
              "fp.to_ubv", `Unary (fun n ->
                  `Term (Base.term_app2_ast (module Type) env "to_ubv"
                           (indexed1 env F.to_ubv n)));
              "fp.to_sbv", `Unary (fun n ->
                  `Term (Base.term_app2_ast (module Type) env "to_sbv"
                           (indexed1 env F.to_sbv n)));
            ] ~err:(Base.bad_term_index_arity (module Type) env)
              ~k:(function
                  | [s; n] when (String.length s >= 2 &&
                                 s.[0] = 'b' && s.[1] = 'v') ->
                    `Term (fun ast args ->
                        Type._warn env (Ast ast) Bitv_extended_lit;
                        parse_extended_lit env s n ast args)
                  | _ -> `Not_found)
        end
      | _ -> `Not_found

  end

end
