
open Dolmen

(* Smtlib arrays *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Bitv with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Bitv with type t = Type.T.t) = struct

    type Type.err +=
      | Invalid_bin_char of char
      | Invalid_hex_char of char

    let check_bin env ast = function
      | '0' | '1' -> ()
      | c ->
        let err = Invalid_bin_char c in
        raise (Type.Typing_error (err, env, ast))

    let hex_to_bin env ast = function
      | '0' -> "0000"
      | '1' -> "0001"
      | '2' -> "0010"
      | '3' -> "0011"
      | '4' -> "0100"
      | '5' -> "0101"
      | '6' -> "0110"
      | '7' -> "0111"
      | '8' -> "1000"
      | '9' -> "1001"
      | 'a' | 'A' -> "1010"
      | 'b' | 'B' -> "1011"
      | 'c' | 'C' -> "1100"
      | 'd' | 'D' -> "1101"
      | 'e' | 'E' -> "1110"
      | 'f' | 'F' -> "1111"
      | c ->
        let err = Invalid_hex_char c in
        raise (Type.Typing_error (err, env, ast))

    let parse_binary env ast s =
      assert (String.length s > 2 && s.[0] = '#' && s.[1] = 'b');
      let s' = String.sub s 2 (String.length s - 2) in
      String.iter (check_bin env ast) s';
      Type.Term (T.mk_bitv s')

    let parse_hexa env ast s =
      assert (String.length s > 2 && s.[0] = '#' && s.[1] = 'x');
      let b = Bytes.create ((String.length s - 2) * 4) in
      String.iteri (fun i c ->
          Bytes.blit_string (hex_to_bin env ast c) 0 b (i * 4) 4
        ) s;
      let s' = Bytes.to_string b in
      Type.Term (T.mk_bitv s')

    let parse_extended_lit env ast s n =
      assert (String.length s >= 2);
      let s' = String.sub s 2 (String.length s - 2) in
      match int_of_string s' with
      | exception Failure _ -> None
      | x ->
        let x_s = Format.asprintf "%x" x in
        let m = (String.length s' * 4) in
        let b = Bytes.create m in
        String.iteri (fun i c ->
            Bytes.blit_string (hex_to_bin env ast c) 0 b (i * 4) 4
          ) x_s;
        let s'' =
          if n <= m then Bytes.sub_string b 0 n
          else
            let b' = Bytes.extend b (n - m) 0 in
            Bytes.fill b' 0 (n - m) '0';
            Bytes.to_string b'
        in
        Some (Type.Term (T.mk_bitv s''))

    let split_id = Dolmen_std.Misc.split_on_char '\000'

    let parse_id env ast id l k =
      let rec aux h r r_l = function
        | [] -> k (h :: r)
        | (s, n, f) :: l' ->
          if String.equal s h then begin
            if r_l = n then f r
            else begin
              let err = Type.Bad_op_arity (s, n, r_l) in
              raise (Type.Typing_error (err, env, ast))
            end
          end else
            aux h r r_l l'
      in
      match split_id id with
      | h :: r -> aux h r (List.length r) l
      | r -> k r

    let parse_int env ast s =
      try int_of_string s
      with Failure _ ->
        let err = Type.Expected ("an integer", None) in
        raise (Type.Typing_error (err, env, ast))

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

    let parse _version env ast s args =
      match s with
      (* sort *)
      | Type.Id { Id.ns = Id.Sort; name; } ->
        parse_id env ast name [
          "BitVec", 1, (function
              | [n_s] ->
                Base.make_op0 (module Type) env ast "BitVec" args
                  (fun () -> Type.Ty (Ty.bitv (parse_int env ast n_s)))
              | _ -> assert false);
        ] (fun _ -> None)
      (* values *)
      | Type.Id { Id.ns = Id.Value Id.Binary; name; } ->
        Base.make_op0 (module Type) env ast name args
          (fun () -> parse_binary env ast name)
      | Type.Id { Id.ns = Id.Value Id.Hexadecimal; name; } ->
        Base.make_op0 (module Type) env ast name args
          (fun () -> parse_hexa env ast name)
      (* terms *)
      | Type.Id { Id.ns = Id.Term; name; } ->
        parse_id env ast name [
          "repeat", 1, (function
              | [i_s] ->
                let i = parse_int env ast i_s in
                Base.make_op1 (module Type) env ast "repeat" args
                  (fun b -> Type.Term (T.bitv_repeat i (Type.parse_term env b)))
              | _ -> assert false);
          "zero_extend", 1, (function
              | [i_s] ->
                let i = parse_int env ast i_s in
                Base.make_op1 (module Type) env ast "zero_extend" args
                  (fun b -> Type.Term (T.zero_extend i (Type.parse_term env b)))
              | _ -> assert false);
          "sign_extend", 1, (function
              | [i_s] ->
                let i = parse_int env ast i_s in
                Base.make_op1 (module Type) env ast "sign_extend" args
                  (fun b -> Type.Term (T.sign_extend i (Type.parse_term env b)))
              | _ -> assert false);
          "rotate_right", 1, (function
              | [i_s] ->
                let i = parse_int env ast i_s in
                Base.make_op1 (module Type) env ast "rotate_right" args
                  (fun b -> Type.Term (T.rotate_right i (Type.parse_term env b)))
              | _ -> assert false);
          "rotate_left", 1, (function
              | [i_s] ->
                let i = parse_int env ast i_s in
                Base.make_op1 (module Type) env ast "rotate_left" args
                  (fun b -> Type.Term (T.rotate_left i (Type.parse_term env b)))
              | _ -> assert false);
          "extract", 2, (function
              | [ i_s; j_s ] ->
                let i = parse_int env ast i_s in
                let j = parse_int env ast j_s in
                Base.make_op1 (module Type) env ast "extract" args
                  (fun b ->
                     let b_t = Type.parse_term env b in
                     Type.Term (T.bitv_extract i j b_t)
                  )
              | _ -> assert false);
        ] (function
            | [s; n] when (String.length s >= 2 &&
                           s.[0] = 'b' && s.[1] = 'v') ->
              parse_extended_lit env ast s (parse_int env ast n)

            | ["bvnot"] ->
              app1 env ast args "bvnot" T.bvnot
            | ["bvand"] ->
              app_left env ast args "bvand" T.bvand
            | ["bvor"] ->
              app_left env ast args "bvor" T.bvor
            | ["bvnand"] ->
              app2 env ast args "bvnand" T.bvnand
            | ["bvnor"] ->
              app2 env ast args "bvnor" T.bvnor
            | ["bvxor"] ->
              app_left env ast args "bvxor" T.bvxor
            | ["bvxnor"] ->
              app_left env ast args "bvxnor" T.bvxnor
            | ["bvcomp"] ->
              app2 env ast args "bvcomp" T.bvcomp

            | ["bvneg"] ->
              app1 env ast args "bvneg" T.bvneg
            | ["bvadd"] ->
              app_left env ast args "bvadd" T.bvadd
            | ["bvsub"] ->
              app2 env ast args "bvsub" T.bvsub
            | ["bvmul"] ->
              app_left env ast args "bvmul" T.bvmul
            | ["bvudiv"] ->
              app2 env ast args "bvudiv" T.bvudiv
            | ["bvurem"] ->
              app2 env ast args "bvurem" T.bvurem
            | ["bvshl"] ->
              app2 env ast args "bvshl" T.bvshl
            | ["bvlshr"] ->
              app2 env ast args "bvlshr" T.bvlshr
            | ["bvult"] ->
              app2 env ast args "bvult" T.bvult
            | ["concat"] ->
              app2 env ast args "concat" T.bitv_concat
            | _ -> None
          )
      | _ -> None

  end
end

