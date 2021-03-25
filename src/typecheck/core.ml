
module Id = Dolmen.Std.Id
module Ast = Dolmen.Std.Term

(* Alt-ergo builtins *)
(* ************************************************************************ *)

module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Base with type t = Type.T.t) = struct

    let parse env s =
      match s with
      | Type.Builtin Ast.Bool ->
        `Ty (Base.app0 (module Type) env "bool" Ty.bool)
      | Type.Builtin Ast.Unit ->
        `Ty (Base.app0 (module Type) env "unit" Ty.unit)
      | Type.Builtin Ast.Void ->
        `Term (Base.app0 (module Type) env "void" T.void)
      | _ -> `Not_found

  end

end

(* TPTP builtins ($i, $o, etc..) *)
(* ************************************************************************ *)

module Tptp = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Base with type t = Type.T.t) = struct

    let parse _version env s =
      match s with
      (*
      | Type.Id ({ Id.name = "$_"; ns = Id.Term } as id) ->
        Some (Type.wildcard env ast id args)
      *)
      | Type.Id { Id.name = "$tType"; ns = Id.Term } ->
        `Ttype (Base.app0 (module Type) env "$tType" ())
      | Type.Id { Id.name = "$o"; ns = Id.Term } ->
        `Ty (Base.app0 (module Type) env "$o" Ty.prop)
      | Type.Id { Id.name = "$i"; ns = Id.Term } ->
        `Ty (Base.app0 (module Type) env "$i" Ty.base)
      | Type.Id { Id.name = "$true"; ns = Id.Term } ->
        `Term (Base.app0 (module Type) env "$true" T._true)
      | Type.Id { Id.name = "$false"; ns = Id.Term } ->
        `Term (Base.app0 (module Type) env "$false" T._false)
      | Type.Id id when Id.equal id Id.tptp_role ->
        `Tags (fun _ast _args -> [])
      | _ -> `Not_found

  end

end

(* Smtlib builtins (bool, =, etc...) *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Smtlib_Base with type 'a t = 'a Type.Tag.t
                                          and type term := Type.T.t)
      (Ty : Dolmen.Intf.Ty.Smtlib_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Base with type t = Type.T.t
                                         and type cstr := Type.T.Cstr.t) = struct

    let parse_symbol env = function
      | { Ast.term = Ast.Symbol s; _ }
      | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, []); _ } ->
        Id.full_name s
      | ast ->
        Type._error env (Ast ast) (Type.Expected ("symbol", None))

    let parse_sexpr_list env = function
      | { Ast.term = Ast.App (
          { Ast.term = Ast.Symbol { Id.name = "$data"; ns = Id.Attr }; _ },
          l); _} ->
        l
      | ast ->
        Type._error env (Ast ast)
          (Type.Expected ("a list of terms in a sexpr", None))

    let mk_or a b = Type.T._or [a; b]
    let mk_and a b = Type.T._and [a; b]

    let parse_f env ast cstr args =
      let loc = Ast.(ast.loc) in
      let t = Ast.apply ~loc cstr args in
      Type.parse_term env t

    let parse _version env s =
      match s with
      (* Bool sort and constants *)
      | Type.Id { Id.name = "Bool"; ns = Id.Sort } ->
        `Ty (Base.app0 (module Type) env "Bool" Ty.prop)
      | Type.Id { Id.name = "true"; ns = Id.Term } ->
        `Term (Base.app0 (module Type) env "true" Type.T._true)
      | Type.Id { Id.name = "false"; ns = Id.Term } ->
        `Term (Base.app0 (module Type) env "false" Type.T._false)

      (* Boolean operators *)
      | Type.Id { Id.name = "not"; ns = Id.Term } ->
        `Term (Base.term_app1 (module Type) env "not" Type.T.neg)
      | Type.Id { Id.name = "and"; ns = Id.Term } ->
        `Term (Base.term_app_left (module Type) env "and" mk_and)
      | Type.Id { Id.name = "or"; ns = Id.Term } ->
        `Term (Base.term_app_left (module Type) env "or" mk_or)
      | Type.Id { Id.name = "xor"; ns = Id.Term } ->
        `Term (Base.term_app_left (module Type) env "xor" Type.T.xor)
      | Type.Id { Id.name = "=>"; ns = Id.Term } ->
        `Term (Base.term_app_right (module Type) env "=>" Type.T.imply)

      (* If-then-else *)
      | Type.Id { Id.name = "ite"; ns = Id.Term } ->
        `Term (fun ast args -> parse_f env ast (Ast.ite_t ()) args)

      (* Equality *)
      | Type.Id { Id.name = "distinct"; ns = Id.Term } ->
        `Term (fun _ast args ->
            let args = List.map (Type.parse_term env) args in
            Type.T.distinct args)
      | Type.Id { Id.name = "="; ns = Id.Term } ->
        `Term (Base.term_app_chain (module Type) env "=" Type.T.eq)

      (* Named formulas *)
      | Type.Id { Id.name = ":named"; ns = Id.Attr } ->
        `Tags (Base.make_op1 (module Type) env ":named" (fun _ t ->
            let name = parse_symbol env t in
            [Type.Any (Tag.named, name)]
          ))

      (* Trigger annotations *)
      | Type.Id { Id.name = ":pattern"; ns = Id.Attr } ->
        `Tags (Base.make_op1 (module Type) env ":pattern" (fun _ t ->
            let l = parse_sexpr_list env t in
            let l = List.map (Type.parse_term env) l in
            [Type.Any (Tag.triggers, l)]
          ))

      (* N-ary s-expressions in attributes *)
      | Type.Id { Id.name = "$data"; ns = Id.Attr } ->
        `Term (fun ast args ->
            begin match args with
              | f :: r -> parse_f env ast f r
              | [] -> Type._error env (Ast ast)
                        (Type.Expected ("a non-empty s-expr", None))
            end)

      (* Rewrite rules *)
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        `Tags (fun _ast _args -> [Type.Any (Tag.rwrt, ())])

      (* ADT testers *)
      | Type.Id ({ Id.ns = Id.Term; _ } as id) ->
        Base.parse_id id [
          "is", `Unary (function s ->
              let id = Id.mk Id.Term s in
              begin match Type.find_bound env id with
                | `Cstr c ->
                  `Term (Base.term_app1 (module Type) env "is" (T.cstr_tester c))
                | _ -> `Not_found
              end);
        ] ~err:(fun _ _ _ -> `Not_found)
          ~k:(fun _ -> `Not_found)

      | _ -> `Not_found

  end

end

(* Zipperposition builtins *)
(* ************************************************************************ *)

module Zf = struct

  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Zf_Base with type 'a t = 'a Type.Tag.t) = struct

    let parse env s =
      match s with
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        `Tags (fun _ast _args -> [Type.Any (Tag.rwrt, ())])
      | Type.Id { Id.name = "infix"; ns = Id.Term } ->
        `Tags (fun ast args -> match args with
            | [ { Ast.term = Ast.Symbol { Id.name; _ }; _ } ] -> [
                Type.Any (Tag.name, Tag.exact name);
                Type.Any (Tag.pos, Tag.infix);
              ]
            | _ ->
              Type._error env (Ast ast)
                (Type.Expected ("a symbol", None))
          )
      | Type.Id { Id.name = "prefix"; ns = Id.Term } ->
        `Tags (fun ast args -> match args with
            | [ { Ast.term = Ast.Symbol { Id.name; _ }; _ } ] -> [
                Type.Any (Tag.name, Tag.exact name);
                Type.Any (Tag.pos, Tag.prefix);
              ]
            | _ ->
              Type._error env (Ast ast)
                (Type.Expected ("a symbol", None))
          )

      | _ -> `Not_found

  end

end

