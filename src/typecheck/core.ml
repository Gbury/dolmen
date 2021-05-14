
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
        `Ty (Base.app0 (module Type) env s Ty.bool)
      | Type.Builtin Ast.Unit ->
        `Ty (Base.app0 (module Type) env s Ty.unit)

      (* Constants *)
      | Type.Builtin Ast.Void ->
        `Term (Base.app0 (module Type) env s T.void)
      | Type.Builtin Ast.True ->
        `Term (Base.app0 (module Type) env s T._true)
      | Type.Builtin Ast.False ->
        `Term (Base.app0 (module Type) env s T._false)

      (* Terms connectors *)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app2 (module Type) env s T.eq)

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
        `Term (Base.term_app1 (module Type) env s T.neg)
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
      | Type.Id { name = Simple "$tType"; ns = Term } ->
        `Ttype (Base.app0 (module Type) env s ())
      | Type.Id { name = Simple "$o"; ns = Term } ->
        `Ty (Base.app0 (module Type) env s Ty.prop)
      | Type.Id { name = Simple "$i"; ns = Term } ->
        `Ty (Base.app0 (module Type) env s Ty.base)
      | Type.Id { name = Simple "$true"; ns = Term } ->
        `Term (Base.app0 (module Type) env s T._true)
      | Type.Id { name = Simple "$false"; ns = Term } ->
        `Term (Base.app0 (module Type) env s T._false)

      (* Predefined connectives *)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app2 (module Type) env s T.eq)
      | Type.Builtin Ast.Distinct ->
        `Term (Base.term_app_list (module Type) env s T.distinct)
      | Type.Id { name = Simple "$distinct"; ns = Term; } ->
        `Term (Base.term_app_list (module Type) env s T.distinct)

      | Type.Builtin Ast.Not ->
        `Term (Base.term_app1 (module Type) env s T.neg)
      | Type.Builtin Ast.Or ->
        `Term (Base.term_app2 (module Type) env s mk_or)
      | Type.Builtin Ast.And ->
        `Term (Base.term_app2 (module Type) env s mk_and)
      | Type.Builtin Ast.Xor ->
        `Term (Base.term_app2 (module Type) env s T.xor)
      | Type.Builtin Ast.Nor ->
        `Term (Base.term_app2 (module Type) env s T.nor)
      | Type.Builtin Ast.Nand ->
        `Term (Base.term_app2 (module Type) env s T.nand)
      | Type.Builtin Ast.Equiv ->
        `Term (Base.term_app2 (module Type) env s T.equiv)
      | Type.Builtin Ast.Imply ->
        `Term (Base.term_app2 (module Type) env s T.imply)
      | Type.Builtin Ast.Implied ->
        `Term (Base.term_app2 (module Type) env s T.implied)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        `Term (
          Base.make_op3 (module Type) env s (fun _ (a, b, c) ->
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
      | Type.Id { name = Simple "$tType"; ns = Term } ->
        `Ttype (Base.app0 (module Type) env s ())
      | Type.Id { name = Simple "$o"; ns = Term } ->
        `Ty (Base.app0 (module Type) env s Ty.prop)
      | Type.Id { name = Simple "$i"; ns = Term } ->
        `Ty (Base.app0 (module Type) env s Ty.base)

      (* Predefined symbols *)
      | Type.Id { name = Simple "$true"; ns = Term } ->
        `Term (Base.term_app_cst (module Type) env T.Const._true)
      | Type.Id { name = Simple "$false"; ns = Term } ->
        `Term (Base.term_app_cst (module Type) env T.Const._false)

      (* Predefined connectives *)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.eq)))
      | Type.Builtin Ast.Distinct ->
        `Term (Base.term_app_list (module Type) env s T.distinct)
      | Type.Id { name = Simple "$distinct"; ns = Term; } ->
        `Term (Base.term_app_list (module Type) env s T.distinct)

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

    let parse_name env = function
      | ({ Ast.term = Ast.Symbol s; _ } as ast)
      | ({ Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, []); _ } as ast) ->
        begin match Dolmen.Std.Id.name s with
          | Simple s -> s
          | _ -> Type._error env (Ast ast) (Type.Expected ("simple name", None))
        end
      | ast ->
        Type._error env (Ast ast) (Type.Expected ("symbol", None))

    let parse_sexpr_list env = function
      | { Ast.term = Ast.App (
          { Ast.term = Ast.Symbol { name = Simple "$data"; ns = Attr }; _ },
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
      | Type.Id { name = Simple "Bool"; ns = Sort } ->
        `Ty (Base.app0 (module Type) env s Ty.prop)
      | Type.Id { name = Simple "true"; ns = Term } ->
        `Term (Base.app0 (module Type) env s T._true)
      | Type.Id { name = Simple "false"; ns = Term } ->
        `Term (Base.app0 (module Type) env s T._false)

      (* Boolean operators *)
      | Type.Id { name = Simple "not"; ns = Term } ->
        `Term (Base.term_app1 (module Type) env s T.neg)
      | Type.Id { name = Simple "and"; ns = Term } ->
        `Term (Base.term_app_left (module Type) env s mk_and)
      | Type.Id { name = Simple "or"; ns = Term } ->
        `Term (Base.term_app_left (module Type) env s mk_or)
      | Type.Id { name = Simple "xor"; ns = Term } ->
        `Term (Base.term_app_left (module Type) env s T.xor)
      | Type.Id { name = Simple "=>"; ns = Term } ->
        `Term (Base.term_app_right (module Type) env s T.imply)

      (* If-then-else *)
      | Type.Id { name = Simple "ite"; ns = Term } ->
        `Term (
          Base.make_op3 (module Type) env s (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Equality *)
      | Type.Id { name = Simple "distinct"; ns = Term } ->
        `Term (fun _ast args ->
            let args = List.map (Type.parse_term env) args in
            T.distinct args)
      | Type.Id { name = Simple "="; ns = Term } ->
        `Term (Base.term_app_chain (module Type) env s T.eq)

      (* Named formulas *)
      | Type.Id { name = Simple ":named"; ns = Attr } ->
        `Tags (Base.make_op1 (module Type) env s (fun _ t ->
            let name = parse_name env t in
            [Type.Set (Tag.named, name)]
          ))

      (* Trigger annotations *)
      | Type.Id { name = Simple ":pattern"; ns = Attr } ->
        `Tags (Base.make_op1 (module Type) env s (fun _ t ->
            let l = parse_sexpr_list env t in
            let l = List.map (Type.parse_term env) l in
            [Type.Add (Tag.triggers, l)]
          ))

      (* N-ary s-expressions in attributes *)
      | Type.Id { name = Simple "$data"; ns = Attr } ->
        `Term (fun ast args ->
            begin match args with
              | f :: r -> parse_f env ast f r
              | [] -> Type._error env (Ast ast)
                        (Type.Expected ("a non-empty s-expr", None))
            end)

      (* Rewrite rules *)
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        `Tags (fun _ast _args -> [Type.Set (Tag.rwrt, ())])

      (* ADT testers *)
      | Type.Id { Id.ns = Term; name = Indexed { basename; indexes; } } as symbol ->
        Base.parse_indexed basename indexes
          ~k:(fun _ -> `Not_found)
          ~err:(fun _ _ _ -> `Not_found) (function
              | "is" -> `Unary (function s ->
                  let id = Id.mk Term s in
                  begin match Type.find_bound env id with
                    | `Cstr c ->
                      `Term (Base.term_app1 (module Type) env symbol (T.cstr_tester c))
                    | _ -> `Not_found
                  end)
              | _ -> `Not_indexed
            )

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
        `Ty (Base.app0 (module Type) env s Ty.prop)

      (* Terms *)
      | Type.Builtin Ast.True ->
        `Term (Base.app0 (module Type) env s T._true)
      | Type.Builtin Ast.False ->
        `Term (Base.app0 (module Type) env s T._false)
      | Type.Builtin Ast.Not ->
        `Term (Base.term_app1 (module Type) env s T.neg)
      | Type.Builtin Ast.Or ->
        `Term (Base.term_app2 (module Type) env s mk_or)
      | Type.Builtin Ast.And ->
        `Term (Base.term_app2 (module Type) env s mk_and)
      | Type.Builtin Ast.Imply ->
        `Term (Base.term_app2 (module Type) env s T.imply)
      | Type.Builtin Ast.Equiv ->
        `Term (Base.term_app2 (module Type) env s T.equiv)
      | Type.Builtin Ast.Eq ->
        `Term (Base.term_app2 (module Type) env s T.eq)
      | Type.Builtin Ast.Distinct ->
        `Term (Base.term_app2 (module Type) env s T.neq)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        `Term (
          Base.make_op3 (module Type) env s (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Tags *)
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        `Tags (fun _ast _args -> [Type.Set (Tag.rwrt, ())])
      | Type.Id { name = Simple "infix"; ns = Term } ->
        `Tags (fun ast args -> match args with
            | [ { Ast.term = Ast.Symbol { name = Simple name; _ }; _ } ] -> [
                Type.Set (Tag.name, Tag.exact name);
                Type.Set (Tag.pos, Tag.infix);
              ]
            | _ ->
              Type._error env (Ast ast)
                (Type.Expected ("a symbol", None))
          )
      | Type.Id { name = Simple "prefix"; ns = Term } ->
        `Tags (fun ast args -> match args with
            | [ { Ast.term = Ast.Symbol { name = Simple name; _ }; _ } ] -> [
                Type.Set (Tag.name, Tag.exact name);
                Type.Set (Tag.pos, Tag.prefix);
              ]
            | _ ->
              Type._error env (Ast ast)
                (Type.Expected ("a symbol", None))
          )

      | _ -> `Not_found

  end

end

