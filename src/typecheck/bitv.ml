
module Id = Dolmen.Std.Id

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

    let parse_extended_lit env s n =
      Base.make_op0 (module Type) env s (fun ast () ->
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
      | Type.Id ({ Id.ns = Id.Sort; _ } as id) ->
        Base.parse_id id [
          "BitVec", `Unary (function n_s ->
              `Ty (Base.app0_ast (module Type) env "BitVec" (fun ast ->
                  Ty.bitv (parse_int env ast n_s))));
        ] ~err:(Base.bad_ty_index_arity (module Type) env)
          ~k:(fun _ -> `Not_found)

      (* Bitvector litterals *)
      | Type.Id { Id.ns = Id.Value Id.Binary; name; } ->
        `Term (Base.app0_ast (module Type) env name (parse_binary env name))
      | Type.Id { Id.ns = Id.Value Id.Hexadecimal; name; } ->
        `Term (Base.app0_ast (module Type) env name (parse_hexa env name))

      (* terms *)
      | Type.Id ({ Id.ns = Id.Term; _ } as id) ->
        Base.parse_id id [
          "repeat", `Unary (function i_s ->
              `Term (Base.term_app1_ast (module Type) env "repeat"
                       (indexed1 env T.repeat i_s)));
          "zero_extend", `Unary (function i_s ->
              `Term (Base.term_app1_ast (module Type) env "zero_extend"
                       (indexed1 env T.zero_extend i_s)));
          "sign_extend", `Unary (function i_s ->
              `Term (Base.term_app1_ast (module Type) env "sign_extend"
                       (indexed1 env T.sign_extend i_s)));
          "rotate_right", `Unary (function i_s ->
              `Term (Base.term_app1_ast (module Type) env "rotate_right"
                       (indexed1 env T.rotate_right i_s)));
          "rotate_left", `Unary (function i_s ->
              `Term (Base.term_app1_ast (module Type) env "rotate_left"
                       (indexed1 env T.rotate_left i_s)));
          "extract", `Binary (fun i_s j_s ->
              `Term (Base.term_app1_ast (module Type) env "extract"
                       (indexed2 env T.extract i_s j_s)));
        ] ~err:(Base.bad_term_index_arity (module Type) env)
          ~k:(function
              | [s; n] when (String.length s >= 2 &&
                             s.[0] = 'b' && s.[1] = 'v') ->
                `Term (parse_extended_lit env s n)

              | ["bvnot"] ->
                `Term (Base.term_app1 (module Type) env "bvnot" T.not)
              | ["bvand"] ->
                `Term (Base.term_app_left (module Type) env "bvand" T.and_)
              | ["bvor"] ->
                `Term (Base.term_app_left (module Type) env "bvor" T.or_)
              | ["bvnand"] ->
                `Term (Base.term_app2 (module Type) env "bvnand" T.nand)
              | ["bvnor"] ->
                `Term (Base.term_app2 (module Type) env "bvnor" T.nor)
              | ["bvxor"] ->
                `Term (Base.term_app_left (module Type) env "bvxor" T.xor)
              | ["bvxnor"] ->
                `Term (Base.term_app_left (module Type) env "bvxnor" T.xnor)

              | ["bvcomp"] ->
                `Term (Base.term_app2 (module Type) env "bvcomp" T.comp)

              | ["bvneg"] ->
                `Term (Base.term_app1 (module Type) env "bvneg" T.neg)
              | ["bvadd"] ->
                `Term (Base.term_app_left (module Type) env "bvadd" T.add)
              | ["bvsub"] ->
                `Term (Base.term_app2 (module Type) env "bvsub" T.sub)
              | ["bvmul"] ->
                `Term (Base.term_app_left (module Type) env "bvmul" T.mul)

              | ["bvudiv"] ->
                `Term (Base.term_app2 (module Type) env "bvudiv" T.udiv)
              | ["bvurem"] ->
                `Term (Base.term_app2 (module Type) env "bvurem" T.urem)

              | ["bvsdiv"] ->
                `Term (Base.term_app2 (module Type) env "bvsdiv" T.sdiv)
              | ["bvsrem"] ->
                `Term (Base.term_app2 (module Type) env "bvsrem" T.srem)
              | ["bvsmod"] ->
                `Term (Base.term_app2 (module Type) env "bvsmod" T.smod)

              | ["bvshl"] ->
                `Term (Base.term_app2 (module Type) env "bvshl" T.shl)
              | ["bvlshr"] ->
                `Term (Base.term_app2 (module Type) env "bvlshr" T.lshr)
              | ["bvashr"] ->
                `Term (Base.term_app2 (module Type) env "bvashr" T.ashr)

              | ["bvult"] ->
                `Term (Base.term_app2 (module Type) env "bvult" T.ult)
              | ["bvule"] ->
                `Term (Base.term_app2 (module Type) env "bvule" T.ule)
              | ["bvugt"] ->
                `Term (Base.term_app2 (module Type) env "bvugt" T.ugt)
              | ["bvuge"] ->
                `Term (Base.term_app2 (module Type) env "bvuge" T.uge)

              | ["bvslt"] ->
                `Term (Base.term_app2 (module Type) env "bvslt" T.slt)
              | ["bvsle"] ->
                `Term (Base.term_app2 (module Type) env "bvsle" T.sle)
              | ["bvsgt"] ->
                `Term (Base.term_app2 (module Type) env "bvsgt" T.sgt)
              | ["bvsge"] ->
                `Term (Base.term_app2 (module Type) env "bvsge" T.sge)

              | ["concat"] ->
                `Term (Base.term_app2 (module Type) env "concat" T.concat)
              | _ -> `Not_found
            )
      | _ -> `Not_found

  end
end

