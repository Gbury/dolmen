
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
      | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
        `Ty (Base.app0 (module Type) env "String" Ty.int)
      | Type.Id { Id.ns = Id.Sort; name = "String"; } ->
        `Ty (Base.app0 (module Type) env "String" Ty.string)
      | Type.Id { Id.ns = Id.Sort; name = "RegLan"; } ->
        `Ty (Base.app0 (module Type) env "RegLan" Ty.string_reg_lang)

      (* String literals *)
      | Type.Id { Id.ns = Value String; name; } ->
        `Term (fun ast args ->
            let s = parse_ustring env ast name in
            Base.app0 (module Type) env name (T.String.of_ustring s) ast args
          )

      (* Terms *)
      | Type.Id ({ Id.ns = Id.Term; name; } as id) ->
        begin match name with

          (* String Functions *)
          | "str.len" ->
            `Term (Base.term_app1 (module Type) env name T.String.length)
          | "str.at" ->
            `Term (Base.term_app2 (module Type) env name T.String.at)

          | "str.is_digit" ->
            `Term (Base.term_app1 (module Type) env name T.String.is_digit)
          | "str._to_code" ->
            `Term (Base.term_app1 (module Type) env name T.String.to_code)
          | "str.from_code" ->
            `Term (Base.term_app1 (module Type) env name T.String.of_code)
          | "str.to_int" ->
            `Term (Base.term_app1 (module Type) env name T.String.to_int)
          | "str.from_int" ->
            `Term (Base.term_app1 (module Type) env name T.String.of_int)

          | "str.++" ->
            `Term (Base.term_app_left (module Type) env name T.String.concat)
          | "str.substr" ->
            `Term (Base.term_app3 (module Type) env name T.String.sub)

          | "str.indexof" ->
            `Term (Base.term_app3 (module Type) env name T.String.index_of)
          | "str.replace" ->
            `Term (Base.term_app3 (module Type) env name T.String.replace)
          | "str.replace_all" ->
            `Term (Base.term_app3 (module Type) env name T.String.replace_all)
          | "str.replace_re" ->
            `Term (Base.term_app3 (module Type) env name T.String.replace_re)
          | "str.replace_re_all" ->
            `Term (Base.term_app3 (module Type) env name T.String.replace_re_all)

          | "str.prefixof" ->
            `Term (Base.term_app2 (module Type) env name T.String.is_prefix)
          | "str.suffixof" ->
            `Term (Base.term_app2 (module Type) env name T.String.is_suffix)
          | "str.contains" ->
            `Term (Base.term_app2 (module Type) env name T.String.contains)

          | "str.<" ->
            `Term (Base.term_app2 (module Type) env name T.String.lt)
          | "str.<=" ->
            `Term (Base.term_app2 (module Type) env name T.String.leq)

          (* String/RegLan functions *)
          | "str.to_re" ->
            `Term (Base.term_app1 (module Type) env name T.String.RegLan.of_string)
          | "re.range" ->
            `Term (Base.term_app2 (module Type) env name T.String.RegLan.range)
          | "str.in_re" ->
            `Term (Base.term_app2 (module Type) env name T.String.in_re)

          (* RegLan functions *)
          | "re.none" ->
            `Term (Base.app0 (module Type) env name T.String.RegLan.empty)
          | "re.all" ->
            `Term (Base.app0 (module Type) env name T.String.RegLan.all)
          | "re.allchar" ->
            `Term (Base.app0 (module Type) env name T.String.RegLan.allchar)
          | "re.++" ->
            `Term (Base.term_app_left (module Type) env name T.String.RegLan.concat)
          | "re.union" ->
            `Term (Base.term_app_left (module Type) env name T.String.RegLan.union)
          | "re.inter" ->
            `Term (Base.term_app_left (module Type) env name T.String.RegLan.inter)
          | "re.*" ->
            `Term (Base.term_app1 (module Type) env name T.String.RegLan.star)
          | "re.comp" ->
            `Term (Base.term_app1 (module Type) env name T.String.RegLan.complement)
          | "re.diff" ->
            `Term (Base.term_app2 (module Type) env name T.String.RegLan.diff)
          | "re.+" ->
            `Term (Base.term_app1 (module Type) env name T.String.RegLan.cross)
          | "re.opt" ->
            `Term (Base.term_app1 (module Type) env name T.String.RegLan.option)

          (* Indexed identifiers *)
          | _ -> Base.parse_id id [
              "char", `Unary (fun s -> `Term (fun ast args ->
                  let s' = parse_uchar_hexa env ast s in
                  Base.app0 (module Type) env name (T.String.of_ustring s') ast args
                ));
              "re.^", `Unary (fun s ->
                  let n = int_of_string s in
                  `Term (Base.term_app1 (module Type) env name (T.String.RegLan.power n))
                );
              "re.loop", `Binary (fun s s' ->
                  let n1 = int_of_string s in
                  let n2 = int_of_string s' in
                  `Term (Base.term_app1 (module Type) env name (T.String.RegLan.loop n1 n2))
                );
            ] ~err:(Base.bad_term_index_arity (module Type) env)
              ~k:(fun _ -> `Not_found)
        end

      | _ -> `Not_found

  end

end
