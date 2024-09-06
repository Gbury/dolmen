
module Id = Dolmen.Std.Id
module Ast = Dolmen.Std.Term

(* Ae floating point *)
(* ************************************************************************ *)

module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Float with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Float with type t := Type.T.t
                                      and type ty := Type.Ty.t) = struct

    module F = T.Float

    let parse env s =
      match s with

      (* sorts *)
      | Type.Id { ns = Sort; name = Simple "fpa_rounding_mode"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.roundingMode)

      (* terms *)
      | Type.Id { ns = Term; name = Simple name; } ->
        begin match name with
          | "NearestTiesToEven" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundNearestTiesToEven)
          | "NearestTiesToAway" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundNearestTiesToAway)
          | "Up" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundTowardPositive)
          | "Down" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundTowardNegative)
          | "ToZero" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundTowardZero)
          | _ -> `Not_found
        end

      | _ -> `Not_found
  end
end

(* Smtlib Floating Point *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Float with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Float with type t := Type.T.t
                                          and type ty := Type.Ty.t
                                          and type cst := Type.T.Const.t) = struct

    module B = T.Bitv
    module R = T.Real
    module F = T.Float

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

    let meta_to_bv mk = fun _ params ret_ty ->
      let err () = mk 0 (0,0) in
      match params with
      | [_; x] ->
        begin match Ty.view (Type.T.Var.ty x) with
          | `Float (e, s) ->
            begin match Ty.view ret_ty with
              | `Bitv n -> mk n (e, s)
              | _ -> err ()
            end
          | _ -> err ()
        end
      | _ -> err ()

    let indexed1 env mk i_s ast =
      let i = parse_int env ast i_s in
      mk i

    let indexed2 env mk i_s j_s ast =
      let i = parse_int env ast i_s in
      let j = parse_int env ast j_s in
      mk i j

    let to_fp env symbol e s = Type.builtin_term (fun ast args ->
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
                 (Type.Bad_op_arity (symbol, [1; 2], List.length args))
      )

    let parse version env s =
      match s with

      (* sorts *)
      | Type.Id { ns = Sort; name = Simple "Float16"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s (Ty.float 5 11))
      | Type.Id { ns = Sort; name = Simple "Float32"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s (Ty.float 8 24))
      | Type.Id { ns = Sort; name = Simple "Float64"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s (Ty.float 11 53))
      | Type.Id { ns = Sort; name = Simple "Float128"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s (Ty.float 15 113))
      | Type.Id { ns = Sort; name = Simple "RoundingMode"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.roundingMode)

      (* indexed sorts *)
      | Type.Id { ns = Sort; name = Indexed { basename; indexes; } } ->
        Base.parse_indexed basename indexes (function
            | "BitVec" -> `Unary (function n_s ->
                Type.builtin_ty (Base.app0_ast (module Type) env s
                       (indexed1 env Ty.bitv n_s)))
            | "FloatingPoint" -> `Binary (fun n_e n_s ->
                Type.builtin_ty (Base.app0_ast (module Type) env s
                       (indexed2 env Ty.float n_e n_s)))
            | _ -> `Not_indexed)
          ~err:(Base.bad_ty_index_arity (module Type) env)
          ~k:(function () -> `Not_found)


      (* Bitvector litterals *)
      | Type.Id { ns = Value Binary; name = Simple name; } ->
        Type.builtin_term (Base.app0_ast (module Type) env s (parse_binary env name))
      | Type.Id { ns = Value Hexadecimal; name = Simple name; } ->
        Type.builtin_term (Base.app0_ast (module Type) env s (parse_hexa env name))

      (* terms *)
      | Type.Id { ns = Term; name = Simple name; } ->
        begin match name with
          | "fp" ->
            Type.builtin_term (Base.term_app3 (module Type) env s F.fp)
          | "RNE" | "roundNearestTiesToEven" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundNearestTiesToEven)
          | "RNA" | "roundNearestTiesToAway" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundNearestTiesToAway)
          | "RTP" | "roundTowardPositive" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundTowardPositive)
          | "RTN" | "roundTowardNegative" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundTowardNegative)
          | "RTZ" | "roundTowardZero" ->
            Type.builtin_term (Base.app0 (module Type) env s F.roundTowardZero)
          | "fp.abs" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.abs)
          | "fp.neg" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.neg)
          | "fp.add" ->
            Type.builtin_term (Base.term_app3 (module Type) env s F.add)
          | "fp.sub" ->
            Type.builtin_term (Base.term_app3 (module Type) env s F.sub)
          | "fp.mul" ->
            Type.builtin_term (Base.term_app3 (module Type) env s F.mul)
          | "fp.div" ->
            Type.builtin_term (Base.term_app3 (module Type) env s F.div)
          | "fp.fma" ->
            Type.builtin_term (Base.term_app4 (module Type) env s F.fma)
          | "fp.sqrt" ->
            Type.builtin_term (Base.term_app2 (module Type) env s F.sqrt)
          | "fp.rem" ->
            Type.builtin_term (Base.term_app2 (module Type) env s F.rem)
          | "fp.roundToIntegral" ->
            Type.builtin_term (Base.term_app2 (module Type) env s F.roundToIntegral)
          | "fp.min" ->
            Type.builtin_term (Base.term_app2 (module Type) env s F.min)
              ~meta:(`Partial (fun _ _ ret_ty ->
                  match Ty.view ret_ty with
                  | `Float es -> F.min' es
                  | _ -> F.min' (1, 1)))
          | "fp.max" ->
            Type.builtin_term (Base.term_app2 (module Type) env s F.max)
              ~meta:(`Partial (fun _ _ ret_ty ->
                  match Ty.view ret_ty with
                  | `Float es -> F.max' es
                  | _ -> F.max' (1, 1)))
          | "fp.leq" ->
            Type.builtin_term (Base.term_app_chain (module Type) env s F.leq)
          | "fp.lt" ->
            Type.builtin_term (Base.term_app_chain (module Type) env s F.lt)
          | "fp.geq" ->
            Type.builtin_term (Base.term_app_chain (module Type) env s F.geq)
          | "fp.gt" ->
            Type.builtin_term (Base.term_app_chain (module Type) env s F.gt)
          | "fp.eq" ->
            Type.builtin_term (Base.term_app_chain (module Type) env s F.eq)
          | "fp.isNormal" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.isNormal)
          | "fp.isSubnormal" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.isSubnormal)
          | "fp.isZero" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.isZero)
          | "fp.isInfinite" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.isInfinite)
          | "fp.isNaN" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.isNaN)
          | "fp.isNegative" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.isNegative)
          | "fp.isPositive" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.isPositive)
          | "fp.to_real" ->
            Type.builtin_term (Base.term_app1 (module Type) env s F.to_real)
          | "fp.to_ubv" ->
            begin match version with
              | `Response _ ->
                `Reserved (`Model (
                  "completing interpretation of fp.to_bv in models",
                  `Partial (meta_to_bv F.to_ubv')))
              | `Script _ ->
                (* the regular case is handled later, because fp.to_ubv is
                   an indexed identifier. *)
                `Not_found
            end
          | "fp.to_sbv" ->
            begin match version with
              | `Response _ ->
                `Reserved (`Model (
                  "completing interpretation of fp.to_sbv in models",
                  `Partial (meta_to_bv F.to_sbv')))
              | `Script _ ->
                (* the regular case is handled later, because fp.to_sbv is
                   an indexed identifier. *)
                `Not_found
            end
          | _ -> `Not_found
        end
      | Type.Id { ns = Term; name = Indexed { basename; indexes; } } as symbol ->
        Base.parse_indexed basename indexes (function
            | "+oo" -> `Binary (fun e s ->
                Type.builtin_term (Base.app0_ast (module Type) env symbol
                         (indexed2 env F.plus_infinity e s)))
            | "-oo" -> `Binary (fun e s ->
                Type.builtin_term (Base.app0_ast (module Type) env symbol
                         (indexed2 env F.minus_infinity e s)))
            | "+zero" -> `Binary (fun e s ->
                Type.builtin_term (Base.app0_ast (module Type) env symbol
                         (indexed2 env F.plus_zero e s)))
            | "-zero" -> `Binary (fun e s ->
                Type.builtin_term (Base.app0_ast (module Type) env symbol
                         (indexed2 env F.minus_zero e s)))
            | "NaN" -> `Binary (fun e s ->
                Type.builtin_term (Base.app0_ast (module Type) env symbol
                         (indexed2 env F.nan e s)))
            | "to_fp" -> `Binary (to_fp env symbol)
            | "to_fp_unsigned" -> `Binary (fun e s ->
                Type.builtin_term
                  (Base.term_app2_ast (module Type) env symbol
                     (indexed2 env F.ubv_to_fp e s)))
            | "fp.to_ubv" -> `Unary (fun n ->
                Type.builtin_term
                  (Base.term_app2_ast (module Type) env symbol
                     (indexed1 env F.to_ubv n))
                  ~meta:(`Partial (meta_to_bv F.to_ubv')))
            | "fp.to_sbv" -> `Unary (fun n ->
                Type.builtin_term
                  (Base.term_app2_ast (module Type) env symbol
                     (indexed1 env F.to_sbv n))
                  ~meta:(`Partial (meta_to_bv F.to_sbv')))
            | _ -> `Not_indexed)
          ~err:(Base.bad_term_index_arity (module Type) env)
          ~k:(function () -> `Not_found)

      | _ -> `Not_found

end

end
