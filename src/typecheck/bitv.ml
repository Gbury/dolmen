
module Id = Dolmen.Id

(* Smtlib Bitvector *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Bitv with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Bitv with type t = Type.T.t) = struct

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Term.t Type.err
      | Invalid_hex_char : char -> Dolmen.Term.t Type.err
      | Invalid_dec_char : char -> Dolmen.Term.t Type.err

    let parse_int env ast s =
      try int_of_string s
      with Failure _ ->
        Type._error env (Ast ast) (Type.Expected ("an integer", None))

    let parse_binary env ast s =
      match Misc.Bitv.parse_binary s with
      | s -> Type.Term (T.mk_bitv s)
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_bin_char c)

    let parse_hexa env ast s =
      match Misc.Bitv.parse_binary s with
      | s -> Type.Term (T.mk_bitv s)
      | exception Misc.Bitv.Invalid_char c ->
        Type._error env (Ast ast) (Invalid_hex_char c)

    let parse_extended_lit env s n =
      Base.make_op0 (module Type) env s (fun ast () ->
          assert (String.length s >= 2);
          let n = parse_int env ast n in
          match Misc.Bitv.parse_decimal s n with
          | s -> Type.Term (T.mk_bitv s)
          | exception Misc.Bitv.Invalid_char c ->
            Type._error env (Ast ast) (Invalid_dec_char c)
        )

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

    let app1 env name mk =
      Base.make_op1 (module Type) env name (fun _ t ->
          Type.Term (mk (Type.parse_term env t)))

    let app2 env name mk =
      Base.make_op2 (module Type) env name (fun _ (a, b) ->
          Type.Term (mk (Type.parse_term env a) (Type.parse_term env b)))

    let app_left env name mk =
      Base.make_assoc (module Type) env name (fun _ l ->
          Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

    let parse _version env s =
      match s with
      (* sort *)
      | Type.Id { Id.ns = Id.Sort; name; } ->
        parse_id env name [
          "BitVec", 1, (function
              | [n_s] ->
                Some (Base.make_op0 (module Type) env "BitVec"
                        (fun ast () -> Type.Ty (Ty.bitv (parse_int env ast n_s))))
              | _ -> assert false);
        ] (fun _ -> None)
      (* values *)
      | Type.Id { Id.ns = Id.Value Id.Binary; name; } ->
        Some (Base.make_op0 (module Type) env name (fun ast () ->
            parse_binary env ast name))
      | Type.Id { Id.ns = Id.Value Id.Hexadecimal; name; } ->
        Some (Base.make_op0 (module Type) env name (fun ast () ->
            parse_hexa env ast name))
      (* terms *)
      | Type.Id { Id.ns = Id.Term; name; } ->
        parse_id env name [
          "repeat", 1, (function
              | [i_s] ->
                Some (Base.make_op1 (module Type) env "repeat" (fun ast b ->
                    let i = parse_int env ast i_s in
                    Type.Term (T.bitv_repeat i (Type.parse_term env b))
                  ))
              | _ -> assert false);
          "zero_extend", 1, (function
              | [i_s] ->
                Some (Base.make_op1 (module Type) env "zero_extend" (fun ast b ->
                    let i = parse_int env ast i_s in
                    Type.Term (T.zero_extend i (Type.parse_term env b))
                  ))
              | _ -> assert false);
          "sign_extend", 1, (function
              | [i_s] ->
                Some (Base.make_op1 (module Type) env "sign_extend" (fun ast b ->
                    let i = parse_int env ast i_s in
                    Type.Term (T.sign_extend i (Type.parse_term env b))
                  ))
              | _ -> assert false);
          "rotate_right", 1, (function
              | [i_s] ->
                Some (Base.make_op1 (module Type) env "rotate_right" (fun ast b ->
                    let i = parse_int env ast i_s in
                    Type.Term (T.rotate_right i (Type.parse_term env b))
                  ))
              | _ -> assert false);
          "rotate_left", 1, (function
              | [i_s] ->
                Some (Base.make_op1 (module Type) env "rotate_left" (fun ast b ->
                    let i = parse_int env ast i_s in
                    Type.Term (T.rotate_left i (Type.parse_term env b))
                  ))
              | _ -> assert false);
          "extract", 2, (function
              | [ i_s; j_s ] ->
                Some (Base.make_op1 (module Type) env "extract" (fun ast b ->
                    let i = parse_int env ast i_s in
                    let j = parse_int env ast j_s in
                    let b_t = Type.parse_term env b in
                    Type.Term (T.bitv_extract i j b_t)
                  ))
              | _ -> assert false);
        ] (function
            | [s; n] when (String.length s >= 2 &&
                           s.[0] = 'b' && s.[1] = 'v') ->
              Some (parse_extended_lit env s n)

            | ["bvnot"] -> Some (app1 env "bvnot" T.bvnot)
            | ["bvand"] -> Some (app_left env "bvand" T.bvand)
            | ["bvor"] -> Some (app_left env "bvor" T.bvor)
            | ["bvnand"] -> Some (app2 env "bvnand" T.bvnand)
            | ["bvnor"] -> Some (app2 env "bvnor" T.bvnor)
            | ["bvxor"] -> Some (app_left env "bvxor" T.bvxor)
            | ["bvxnor"] -> Some (app_left env "bvxnor" T.bvxnor)
            | ["bvcomp"] -> Some (app2 env "bvcomp" T.bvcomp)

            | ["bvneg"] -> Some (app1 env "bvneg" T.bvneg)
            | ["bvadd"] -> Some (app_left env "bvadd" T.bvadd)
            | ["bvsub"] -> Some (app2 env "bvsub" T.bvsub)
            | ["bvmul"] -> Some (app_left env "bvmul" T.bvmul)
            | ["bvudiv"] -> Some (app2 env "bvudiv" T.bvudiv)
            | ["bvurem"] -> Some (app2 env "bvurem" T.bvurem)
            | ["bvshl"] -> Some (app2 env "bvshl" T.bvshl)
            | ["bvlshr"] -> Some (app2 env "bvlshr" T.bvlshr)
            | ["bvult"] -> Some (app2 env "bvult" T.bvult)
            | ["concat"] -> Some (app2 env "concat" T.bitv_concat)
            | _ -> None
          )
      | _ -> None

  end
end

