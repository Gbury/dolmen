
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

      (* Types *)
      | Type.Builtin Ast.Bool ->
        `Ty (Base.app0 (module Type) env "bool" Ty.bool)
      | Type.Builtin Ast.Unit ->
        `Ty (Base.app0 (module Type) env "unit" Ty.unit)

      (* Constants *)
      | Type.Builtin Ast.Void ->
        `Term (Base.app0 (module Type) env "void" T.void)
      | Type.Builtin Ast.True ->
        `Term (Base.app0 (module Type) env "true" T._true)
      | Type.Builtin Ast.False ->
        `Term (Base.app0 (module Type) env "true" T._false)

      (* Terms connectors *)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app2 (module Type) env "=" T.eq)

      | _ -> `Not_found

  end

end

(* Alt-ergo builtins *)
(* ************************************************************************ *)

module Dimacs = struct

  module Tff
      (Type : Tff_intf.S)
      (T : Dolmen.Intf.Term.Dimacs with type t = Type.T.t) = struct

    let parse env s =
      match s with
      | Type.Builtin Ast.Not ->
        `Term (Base.term_app1 (module Type) env "not" T.neg)
      | _ -> `Not_found

  end

end


(* TPTP builtins ($i, $o, etc..) *)
(* ************************************************************************ *)

module Tptp = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Tff_Core with type t = Type.T.t) = struct

    let mk_or a b = T._or [a; b]
    let mk_and a b = T._and [a; b]

    let parse _version env s =
      match s with

      (* Predefined symbols *)
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

      (* Predefined connectives *)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app2 (module Type) env "=" T.eq)
      | Type.Builtin Ast.Distinct ->
        `Term (Base.term_app_list (module Type) env "!=" T.distinct)
      | Type.Id { Id.name = "$distinct"; ns = Id.Term; } ->
        `Term (Base.term_app_list (module Type) env "$distinct" T.distinct)

      | Type.Builtin Ast.Not ->
        `Term (Base.term_app1 (module Type) env "~" T.neg)
      | Type.Builtin Ast.Or ->
        `Term (Base.term_app2 (module Type) env "|" mk_or)
      | Type.Builtin Ast.And ->
        `Term (Base.term_app2 (module Type) env "&" mk_and)
      | Type.Builtin Ast.Xor ->
        `Term (Base.term_app2 (module Type) env "<~>" T.xor)
      | Type.Builtin Ast.Nor ->
        `Term (Base.term_app2 (module Type) env "~|" T.nor)
      | Type.Builtin Ast.Nand ->
        `Term (Base.term_app2 (module Type) env "~&" T.nand)
      | Type.Builtin Ast.Equiv ->
        `Term (Base.term_app2 (module Type) env "<=>" T.equiv)
      | Type.Builtin Ast.Imply ->
        `Term (Base.term_app2 (module Type) env "=>" T.imply)
      | Type.Builtin Ast.Implied ->
        `Term (Base.term_app2 (module Type) env "<=" T.implied)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        `Term (
          Base.make_op3 (module Type) env "$ite" (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Ignore the role and kind attributes *)
      | Type.Id id when Id.equal id Id.tptp_role ->
        `Tags (fun _ast _args -> [])
      | Type.Id id when Id.equal id Id.tptp_kind ->
        `Tags (fun _ast _args -> [])
      | _ -> `Not_found

  end

  module Thf
      (Type : Thf_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Thf_Core with type t = Type.T.t
                                           and type Const.t = Type.T.Const.t) = struct

    let parse _version env s =
      match s with

      (* Ttype and types *)
      | Type.Id { Id.name = "$tType"; ns = Id.Term } ->
        `Ttype (Base.app0 (module Type) env "$tType" ())
      | Type.Id { Id.name = "$o"; ns = Id.Term } ->
        `Ty (Base.app0 (module Type) env "$o" Ty.prop)
      | Type.Id { Id.name = "$i"; ns = Id.Term } ->
        `Ty (Base.app0 (module Type) env "$i" Ty.base)

      (* Predefined symbols *)
      | Type.Id { Id.name = "$true"; ns = Id.Term } ->
        `Term (Base.term_app_cst (module Type) env T.Const._true)
      | Type.Id { Id.name = "$false"; ns = Id.Term } ->
        `Term (Base.term_app_cst (module Type) env T.Const._false)

      (* Predefined connectives *)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.eq)))
      | Type.Builtin Ast.Distinct ->
        `Term (Base.term_app_list (module Type) env "!=" T.distinct)
      | Type.Id { Id.name = "$distinct"; ns = Id.Term; } ->
        `Term (Base.term_app_list (module Type) env "$distinct" T.distinct)

      | Type.Builtin Ast.Not ->
        `Term (Base.term_app_cst (module Type) env T.Const.neg)
      | Type.Builtin Ast.Or ->
        `Term (Base.term_app_cst (module Type) env T.Const.or_)
      | Type.Builtin Ast.And ->
        `Term (Base.term_app_cst (module Type) env T.Const.and_)
      | Type.Builtin Ast.Xor ->
        `Term (Base.term_app_cst (module Type) env T.Const.xor)
      | Type.Builtin Ast.Nor ->
        `Term (Base.term_app_cst (module Type) env T.Const.nor)
      | Type.Builtin Ast.Nand ->
        `Term (Base.term_app_cst (module Type) env T.Const.nand)
      | Type.Builtin Ast.Equiv ->
        `Term (Base.term_app_cst (module Type) env T.Const.equiv)
      | Type.Builtin Ast.Imply ->
        `Term (Base.term_app_cst (module Type) env T.Const.imply)
      | Type.Builtin Ast.Implied ->
        `Term (Base.term_app_cst (module Type) env T.Const.implied)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        `Term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.ite)))

      (* Pi & Sigma *)
      | Type.Builtin Ast.Pi ->
        `Term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.pi)))
      | Type.Builtin Ast.Sigma ->
        `Term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.sigma)))

      (* Ignore the role and kind attributes *)
      | Type.Id id when Id.equal id Id.tptp_role ->
        `Tags (fun _ast _args -> [])
      | Type.Id id when Id.equal id Id.tptp_kind ->
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

    let mk_or a b = T._or [a; b]
    let mk_and a b = T._and [a; b]

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
        `Term (Base.app0 (module Type) env "true" T._true)
      | Type.Id { Id.name = "false"; ns = Id.Term } ->
        `Term (Base.app0 (module Type) env "false" T._false)

      (* Boolean operators *)
      | Type.Id { Id.name = "not"; ns = Id.Term } ->
        `Term (Base.term_app1 (module Type) env "not" T.neg)
      | Type.Id { Id.name = "and"; ns = Id.Term } ->
        `Term (Base.term_app_left (module Type) env "and" mk_and)
      | Type.Id { Id.name = "or"; ns = Id.Term } ->
        `Term (Base.term_app_left (module Type) env "or" mk_or)
      | Type.Id { Id.name = "xor"; ns = Id.Term } ->
        `Term (Base.term_app_left (module Type) env "xor" T.xor)
      | Type.Id { Id.name = "=>"; ns = Id.Term } ->
        `Term (Base.term_app_right (module Type) env "=>" T.imply)

      (* If-then-else *)
      | Type.Id { Id.name = "ite"; ns = Id.Term } ->
        `Term (
          Base.make_op3 (module Type) env "$ite" (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Equality *)
      | Type.Id { Id.name = "distinct"; ns = Id.Term } ->
        `Term (fun _ast args ->
            let args = List.map (Type.parse_term env) args in
            T.distinct args)
      | Type.Id { Id.name = "="; ns = Id.Term } ->
        `Term (Base.term_app_chain (module Type) env "=" T.eq)

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
      (Tag : Dolmen.Intf.Tag.Zf_Base with type 'a t = 'a Type.Tag.t)
      (Ty : Dolmen.Intf.Ty.Zf_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Zf_Base with type t = Type.T.t) = struct

    let mk_or a b = T._or [a; b]
    let mk_and a b = T._and [a; b]

    let parse env s =
      match s with
      (* Types *)
      | Type.Builtin Ast.Prop ->
        `Ty (Base.app0 (module Type) env "bool" Ty.prop)

      (* Terms *)
      | Type.Builtin Ast.True ->
        `Term (Base.app0 (module Type) env "true" T._true)
      | Type.Builtin Ast.False ->
        `Term (Base.app0 (module Type) env "false" T._false)
      | Type.Builtin Ast.Not ->
        `Term (Base.term_app1 (module Type) env "~" T.neg)
      | Type.Builtin Ast.Or ->
        `Term (Base.term_app2 (module Type) env "||" mk_or)
      | Type.Builtin Ast.And ->
        `Term (Base.term_app2 (module Type) env "&&" mk_and)
      | Type.Builtin Ast.Imply ->
        `Term (Base.term_app2 (module Type) env "=>" T.imply)
      | Type.Builtin Ast.Equiv ->
        `Term (Base.term_app2 (module Type) env "<=>" T.equiv)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app2 (module Type) env "=" T.eq)
      | Type.Builtin Ast.Distinct ->
        `Term (Base.term_app2 (module Type) env "=" T.neq)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        `Term (
          Base.make_op3 (module Type) env "ite" (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Tags *)
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

