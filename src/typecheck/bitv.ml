
module Id = Dolmen.Std.Id

(* Ae Bitvector *)
(* ************************************************************************ *)
module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Bitv with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Bitv with type t := Type.T.t) = struct

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Std.Term.t Type.err
      | Expected_nat_lit : Dolmen.Std.Term.t -> Dolmen.Std.Term.t Type.err

    let parse_binary env s ast =
      match String.iter Misc.Bitv.check_bin s with
      | () -> T.mk s
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_bin_char c)

    let int_of_term env ast a =
      match a.Dolmen_std.Term.term with
      | Symbol { ns = Value Integer; name = Simple name; } ->
        (* the namespace guarantees that the `name` is an integer literal
           therefore `int_of_string` should not raise an exception *)
        let i = int_of_string name in
        if i >= 0 then i else
          Type._error env (Ast ast) (Expected_nat_lit a)
      | _ ->
        Type._error env (Ast ast) (Expected_nat_lit a)

    let app_int_term env symbol mk =
      Base.make_op2 (module Type) env symbol
        (fun ast (a, b) ->
           mk (int_of_term env ast a) (Type.parse_term env b))

    let parse env s =
      match s with

      (* Bitvector sort *)
      | Type.Builtin (Bitv n) ->
        Type.builtin_ty (Base.app0 (module Type) env s (Ty.bitv n))

      (* Bitvector litterals *)
      | Type.Id { ns = Value Bitvector; name = Simple name; } ->
        Type.builtin_term (Base.app0_ast (module Type) env s (parse_binary env name))

      (* Bitvector operators *)
      | Type.Builtin Bitv_concat ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.concat)
      | Type.Builtin (Bitv_extract (l, r)) ->
        Type.builtin_term (Base.term_app1 (module Type) env s (T.extract r l))

      (* terms *)
      | Type.Id { ns = Term; name = Simple "repeat"; } ->
        Type.builtin_term (app_int_term env s T.repeat)
      | Type.Id { ns = Term; name = Simple "zero_extend"; } ->
        Type.builtin_term (app_int_term env s T.zero_extend)
      | Type.Id { ns = Term; name = Simple "sign_extend"; } ->
        Type.builtin_term (app_int_term env s T.sign_extend)
      | Type.Id { ns = Term; name = Simple "rotate_right"; } ->
        Type.builtin_term (app_int_term env s T.rotate_right)
      | Type.Id { ns = Term; name = Simple "rotate_left"; } ->
        Type.builtin_term (app_int_term env s T.rotate_left)

      | Type.Id { ns = Term; name = Simple "bvnot"; } ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.not)
      | Type.Id { ns = Term; name = Simple "bvand"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.and_)
      | Type.Id { ns = Term; name = Simple "bvor"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.or_)
      | Type.Id { ns = Term; name = Simple "bvnand"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.nand)
      | Type.Id { ns = Term; name = Simple "bvnor"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.nor)
      | Type.Id { ns = Term; name = Simple "bvxor"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.xor)
      | Type.Id { ns = Term; name = Simple "bvxnor"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.xnor)

      | Type.Id { ns = Term; name = Simple "bvcomp"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.comp)

      | Type.Id { ns = Term; name = Simple "bvneg"; } ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.neg)
      | Type.Id { ns = Term; name = Simple "bvadd"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.add)
      | Type.Id { ns = Term; name = Simple "bvsub"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sub)
      | Type.Id { ns = Term; name = Simple "bvmul"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.mul)

      | Type.Id { ns = Term; name = Simple "bvudiv"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.udiv)
      | Type.Id { ns = Term; name = Simple "bvurem"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.urem)

      | Type.Id { ns = Term; name = Simple "bvsdiv"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sdiv)
      | Type.Id { ns = Term; name = Simple "bvsrem"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.srem)
      | Type.Id { ns = Term; name = Simple "bvsmod"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.smod)

      | Type.Id { ns = Term; name = Simple "bvshl"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.shl)
      | Type.Id { ns = Term; name = Simple "bvlshr"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.lshr)
      | Type.Id { ns = Term; name = Simple "bvashr"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ashr)

      | Type.Id { ns = Term; name = Simple "bvult"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ult)
      | Type.Id { ns = Term; name = Simple "bvule"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ule)
      | Type.Id { ns = Term; name = Simple "bvugt"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ugt)
      | Type.Id { ns = Term; name = Simple "bvuge"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.uge)

      | Type.Id { ns = Term; name = Simple "bvslt"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.slt)
      | Type.Id { ns = Term; name = Simple "bvsle"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sle)
      | Type.Id { ns = Term; name = Simple "bvsgt"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sgt)
      | Type.Id { ns = Term; name = Simple "bvsge"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sge)

      | _ -> `Not_found

  end
end

(* Smtlib Bitvector *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Bitv with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Bitv with type t := Type.T.t) = struct

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_hex_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_dec_char : char -> Dolmen.Std.Term.t Type.err

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
      | s -> T.mk s
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_bin_char c)

    let parse_hexa env s ast =
      match Misc.Bitv.parse_hexa s with
      | s -> T.mk s
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_hex_char c)

    let parse_extended_lit env symbol s n =
      Base.make_op0 (module Type) env symbol (fun ast () ->
          assert (String.length s >= 2);
          let n = parse_int env ast n in
          match Misc.Bitv.parse_decimal s n with
          | s -> T.mk s
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

    let parse _version env s =
      match s with

      (* Bitvector sort *)
      | Type.Id { Id.ns = Sort; name = Indexed { basename; indexes; }; } as symbol ->
        Base.parse_indexed basename indexes (function
            | "BitVec" -> `Unary (function n_s ->
                Type.builtin_ty (Base.app0_ast (module Type) env symbol (fun ast ->
                    Ty.bitv (parse_int env ast n_s))))
            | _ -> `Not_indexed
          ) ~k:(fun _ -> `Not_found)
          ~err:(Base.bad_ty_index_arity (module Type) env)

      (* Bitvector litterals *)
      | Type.Id { ns = Value Binary; name = Simple name; } ->
        Type.builtin_term (Base.app0_ast (module Type) env s (parse_binary env name))
      | Type.Id { ns = Value Hexadecimal; name = Simple name; } ->
        Type.builtin_term (Base.app0_ast (module Type) env s (parse_hexa env name))

      (* terms *)
      | Type.Id { ns = Term; name = Simple "bvnot"; } ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.not)
      | Type.Id { ns = Term; name = Simple "bvand"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.and_)
      | Type.Id { ns = Term; name = Simple "bvor"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.or_)
      | Type.Id { ns = Term; name = Simple "bvnand"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.nand)
      | Type.Id { ns = Term; name = Simple "bvnor"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.nor)
      | Type.Id { ns = Term; name = Simple "bvxor"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.xor)
      | Type.Id { ns = Term; name = Simple "bvxnor"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.xnor)

      | Type.Id { ns = Term; name = Simple "bvcomp"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.comp)

      | Type.Id { ns = Term; name = Simple "bvneg"; } ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.neg)
      | Type.Id { ns = Term; name = Simple "bvadd"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.add)
      | Type.Id { ns = Term; name = Simple "bvsub"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sub)
      | Type.Id { ns = Term; name = Simple "bvmul"; } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.mul)

      | Type.Id { ns = Term; name = Simple "bvudiv"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.udiv)
      | Type.Id { ns = Term; name = Simple "bvurem"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.urem)

      | Type.Id { ns = Term; name = Simple "bvsdiv"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sdiv)
      | Type.Id { ns = Term; name = Simple "bvsrem"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.srem)
      | Type.Id { ns = Term; name = Simple "bvsmod"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.smod)

      | Type.Id { ns = Term; name = Simple "bvshl"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.shl)
      | Type.Id { ns = Term; name = Simple "bvlshr"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.lshr)
      | Type.Id { ns = Term; name = Simple "bvashr"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ashr)

      | Type.Id { ns = Term; name = Simple "bvult"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ult)
      | Type.Id { ns = Term; name = Simple "bvule"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ule)
      | Type.Id { ns = Term; name = Simple "bvugt"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.ugt)
      | Type.Id { ns = Term; name = Simple "bvuge"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.uge)

      | Type.Id { ns = Term; name = Simple "bvslt"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.slt)
      | Type.Id { ns = Term; name = Simple "bvsle"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sle)
      | Type.Id { ns = Term; name = Simple "bvsgt"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sgt)
      | Type.Id { ns = Term; name = Simple "bvsge"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.sge)

      | Type.Id { ns = Term; name = Simple "concat"; } ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.concat)

      (* indexed terms *)
      | Type.Id { ns = Term; name = Indexed { basename; indexes; } } as symbol ->
        Base.parse_indexed basename indexes (function
            | s when (String.length s >= 2 && s.[0] = 'b' && s.[1] = 'v') ->
              `Unary (fun n -> Type.builtin_term (parse_extended_lit env symbol s n))
            | "repeat" -> `Unary (function i_s ->
                Type.builtin_term (Base.term_app1_ast (module Type) env symbol
                         (indexed1 env T.repeat i_s)))
            | "zero_extend" -> `Unary (function i_s ->
                Type.builtin_term (Base.term_app1_ast (module Type) env symbol
                         (indexed1 env T.zero_extend i_s)))
            | "sign_extend" -> `Unary (function i_s ->
                Type.builtin_term (Base.term_app1_ast (module Type) env symbol
                         (indexed1 env T.sign_extend i_s)))
            | "rotate_right" -> `Unary (function i_s ->
                Type.builtin_term (Base.term_app1_ast (module Type) env symbol
                         (indexed1 env T.rotate_right i_s)))
            | "rotate_left" -> `Unary (function i_s ->
                Type.builtin_term (Base.term_app1_ast (module Type) env symbol
                         (indexed1 env T.rotate_left i_s)))
            | "extract" -> `Binary (fun i_s j_s ->
                Type.builtin_term (Base.term_app1_ast (module Type) env symbol
                         (indexed2 env T.extract i_s j_s)))
            | _ -> `Not_indexed)
          ~err:(Base.bad_term_index_arity (module Type) env)
          ~k:(function () -> `Not_found)

      | _ -> `Not_found

  end
end

