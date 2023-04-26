
module Id = Dolmen.Std.Id

(* Smtlib Unicode Strings *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_String with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_String with type t := Type.T.t) = struct

    type _ Type.err +=
      | Invalid_hexadecimal : string -> Dolmen.Std.Term.t Type.err
      | Invalid_string_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_escape_sequence : string * int -> Dolmen.Std.Term.t Type.err

    let int_of_string_hexa s = int_of_string ("0x" ^ s)

    let parse_uchar_hexa env ast s =
      if String.length s < 3 then raise Exit;
      if not (s.[0] = '#' && s.[1] = 'x') then raise Exit;
      let s' = String.sub s 2 (String.length s - 2) in
      match int_of_string_hexa s' with
      | i when 0 <= i && i <= 0x2FFFF ->
        let b = Buffer.create 5 in
        let e = Uutf.encoder (`UTF_8) (`Buffer b) in
        let u = Uchar.of_int i in
        begin match Uutf.encode e (`Uchar u) with
          | `Ok -> Buffer.contents b
          | `Partial -> assert false
        end
      | _ ->
        Type._error env (Ast ast) (Invalid_hexadecimal s)
      | exception Failure _ ->
        Type._error env (Ast ast) (Invalid_hexadecimal s)

    let parse_ustring env ast s =
      let b = Buffer.create (String.length s) in
      let e = Uutf.encoder (`UTF_8) (`Buffer b) in
      let encode x =
        match Uutf.encode e x with
        | `Ok -> ()
        | `Partial -> assert false (* should not happen with a buffer dst *)
      in
      let rec aux i =
        if i >= String.length s then begin
          encode `End;
          Buffer.contents b
        end else begin
          match s.[i] with
          (* escape sequence *)
          | '\\' ->
            begin try
                if i + 1 >= String.length s then begin
                  let () = encode (`Uchar (Uchar.of_char '\\')) in
                  aux (i + 1)
                end else if s.[i + 1] <> 'u' then begin
                  let () = encode (`Uchar (Uchar.of_char '\\')) in
                  let () = encode (`Uchar (Uchar.of_char s.[i + 1])) in
                  aux (i + 2)
                end else
                  match s.[i + 2] with
                  | '{' ->
                    let j = String.index_from s (i + 3) '}' in
                    let n = j - (i + 3) in
                    assert (n > 0);
                    if n > 5 then raise Exit;
                    let s' = String.sub s (i + 3) n in
                    let u = Uchar.of_int (int_of_string_hexa s') in
                    let () = encode (`Uchar u) in
                    aux (j + 1)
                  | _ ->
                    let s' = String.sub s (i + 2) 4 in
                    let u = Uchar.of_int (int_of_string_hexa s') in
                    let () = encode (`Uchar u) in
                    aux (i + 6)
              with
              | Exit
              | Failure _
              | Invalid_argument _ ->
                Type._error env (Ast ast) (Invalid_escape_sequence (s, i))
            end
          (* US ASCII chars allowed *)
          | ' ' .. '~' as c ->
            encode (`Uchar (Uchar.of_char c));
            aux (i + 1)
          (* Error *)
          | c -> Type._error env (Ast ast) (Invalid_string_char c)
        end
      in
      aux 0

    let parse _version env s =
      match s with
      (* Types *)
      | Type.Id { ns = Sort; name = Simple "Int"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.int)
      | Type.Id { ns = Sort; name = Simple "String"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.string)
      | Type.Id { ns = Sort; name = Simple "RegLan"; } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.string_reg_lang)

      (* String literals *)
      | Type.Id { Id.ns = Value String; name = Simple name; } as symbol ->
        Type.builtin_term (fun ast args ->
            let s = parse_ustring env ast name in
            Base.app0 (module Type) env symbol (T.String.of_ustring s) ast args
          )

      (* Terms *)
      | Type.Id { ns = Term; name = Simple name; } ->
        begin match name with

          (* String Functions *)
          | "str.len" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.length)
          | "str.at" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.at)

          | "str.is_digit" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.is_digit)
          | "str._to_code" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.to_code)
          | "str.from_code" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.of_code)
          | "str.to_int" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.to_int)
          | "str.from_int" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.of_int)

          | "str.++" ->
            Type.builtin_term (Base.term_app_left (module Type) env s T.String.concat)
          | "str.substr" ->
            Type.builtin_term (Base.term_app3 (module Type) env s T.String.sub)

          | "str.indexof" ->
            Type.builtin_term (Base.term_app3 (module Type) env s T.String.index_of)
          | "str.replace" ->
            Type.builtin_term (Base.term_app3 (module Type) env s T.String.replace)
          | "str.replace_all" ->
            Type.builtin_term (Base.term_app3 (module Type) env s T.String.replace_all)
          | "str.replace_re" ->
            Type.builtin_term (Base.term_app3 (module Type) env s T.String.replace_re)
          | "str.replace_re_all" ->
            Type.builtin_term (Base.term_app3 (module Type) env s T.String.replace_re_all)

          | "str.prefixof" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.is_prefix)
          | "str.suffixof" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.is_suffix)
          | "str.contains" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.contains)

          | "str.<" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.lt)
          | "str.<=" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.leq)

          (* String/RegLan functions *)
          | "str.to_re" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.RegLan.of_string)
          | "re.range" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.RegLan.range)
          | "str.in_re" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.in_re)

          (* RegLan functions *)
          | "re.none" ->
            Type.builtin_term (Base.app0 (module Type) env s T.String.RegLan.empty)
          | "re.all" ->
            Type.builtin_term (Base.app0 (module Type) env s T.String.RegLan.all)
          | "re.allchar" ->
            Type.builtin_term (Base.app0 (module Type) env s T.String.RegLan.allchar)
          | "re.++" ->
            Type.builtin_term (Base.term_app_left (module Type) env s T.String.RegLan.concat)
          | "re.union" ->
            Type.builtin_term (Base.term_app_left (module Type) env s T.String.RegLan.union)
          | "re.inter" ->
            Type.builtin_term (Base.term_app_left (module Type) env s T.String.RegLan.inter)
          | "re.*" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.RegLan.star)
          | "re.comp" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.RegLan.complement)
          | "re.diff" ->
            Type.builtin_term (Base.term_app2 (module Type) env s T.String.RegLan.diff)
          | "re.+" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.RegLan.cross)
          | "re.opt" ->
            Type.builtin_term (Base.term_app1 (module Type) env s T.String.RegLan.option)

          | _ -> `Not_found
        end

      (* Indexed identifiers *)
      | Type.Id { ns = Term; name = Indexed { basename; indexes; }; } as symbol ->
        Base.parse_indexed basename indexes (function
            | "char" -> `Unary (fun s -> Type.builtin_term (fun ast args ->
                let s' = parse_uchar_hexa env ast s in
                Base.app0 (module Type) env symbol (T.String.of_ustring s') ast args
              ))
            | "re.^" -> `Unary (fun s ->
                let n = int_of_string s in
                Type.builtin_term (Base.term_app1 (module Type) env symbol (T.String.RegLan.power n))
              )
            | "re.loop" -> `Binary (fun s s' ->
                let n1 = int_of_string s in
                let n2 = int_of_string s' in
                Type.builtin_term (Base.term_app1 (module Type) env symbol (T.String.RegLan.loop n1 n2))
              )
            | _ -> `Not_indexed
          ) ~err:(Base.bad_term_index_arity (module Type) env)
          ~k:(fun _ -> `Not_found)

      | _ -> `Not_found

  end

end
