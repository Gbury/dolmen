
module Id = Dolmen.Std.Id
module Ast = Dolmen.Std.Term

(* Alt-ergo builtins *)
(* ************************************************************************ *)

module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Ae_Base with type 'a t = 'a Type.Tag.t
                                      and type term := Type.T.t)
      (Ty : Dolmen.Intf.Ty.Ae_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Base with type t = Type.T.t
                                     and type term_var := Type.T.Var.t) = struct

    let mk_or a b = T._or [a; b]
    let mk_and a b = T._and [a; b]

    let parse_trigger env ast = function
      | { Ast.term = Ast.App (
          { Ast.term = Ast.Builtin And; _ }, l
        ); _} ->
        List.map (Type.parse_term env) l
      | _ ->
        Type._error env (Ast ast)
          (Type.Expected ("A multi-trigger (i.e. a list of term patterns)", None))

    let parse env s =
      match s with

      (* Types *)
      | Type.Builtin Ast.Bool ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.bool)
      | Type.Builtin Ast.Prop ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.bool)
      | Type.Builtin Ast.Unit ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.unit)

      (* Constants *)
      | Type.Builtin Ast.Void ->
        Type.builtin_term (Base.app0 (module Type) env s T.void)
      | Type.Builtin Ast.True ->
        Type.builtin_term (Base.app0 (module Type) env s T._true)
      | Type.Builtin Ast.False ->
        Type.builtin_term (Base.app0 (module Type) env s T._false)

      (* Boolean operators *)
      | Type.Builtin Ast.Not ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.neg)
      | Type.Builtin Ast.And ->
        Type.builtin_term (Base.term_app_left (module Type) env s mk_and)
      | Type.Builtin Ast.Or ->
        Type.builtin_term (Base.term_app_left (module Type) env s mk_or)
      | Type.Builtin Ast.Xor ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.xor)
      | Type.Builtin Ast.Imply ->
        Type.builtin_term (Base.term_app_right (module Type) env s T.imply)
      | Type.Builtin Ast.Equiv ->
        Type.builtin_term (Base.term_app_right (module Type) env s T.equiv)

      (* If-then-else *)
      | Type.Builtin Ast.Ite ->
        Type.builtin_term (
          Base.make_op3 (module Type) env s (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Equality *)
      | Type.Builtin Ast.Eq ->
        Type.builtin_term (
          fun ast args ->
            match args with
            | [Ast.{
                term = App ( { term = Builtin Eq; _ }, [_; lr_st] ); _
              } as l_st; r_st] ->
              Base.term_app_list (module Type) env s
                T._and ast [l_st; Ast.eq lr_st r_st]
            | _ ->
              Base.term_app2 (module Type) env s T.eq ast args
        )

      | Type.Builtin Ast.Distinct ->
        Type.builtin_term (Base.term_app_list (module Type) env s T.distinct)

      (* AC (Associative Commutative) symbol *)
      | Type.Id { name = Simple "ac"; ns = Attr; }->
        Type.builtin_tags (fun _ _ -> [Type.Set (Tag.ac, ())])

      (* Predicate definition *)
      | Type.Id { name = Simple "predicate"; ns = Attr; }->
        Type.builtin_tags (fun _ _ -> [Type.Set (Tag.predicate, ())])

      (* Named terms *)
      | Type.Id { name = Simple n; ns = Track; }->
        Type.builtin_tags (fun _ _ -> [Type.Set (Tag.named, n)])

      (* Triggers *)
      | Type.Id { name = Simple "triggers"; ns = Attr; } ->
        Type.builtin_tags (fun ast l ->
            let l = List.map (parse_trigger env ast) l in
            [Type.Set (Tag.triggers, l)]
          )

      (* Filters *)
      | Type.Id { name = Simple "filters"; ns = Attr; } ->
        Type.builtin_tags (fun _ l ->
            let l = List.map (Type.parse_prop env) l in
            [Type.Set (Tag.filters, l)]
          )

      (* Semantic triggers *)
      | Type.Builtin (Ast.In_interval (b1, b2)) ->
        Type.builtin_term (Base.term_app3_ast (module Type) env s
                 (fun _ast _a1 _a2 _a3 ->
                    T.in_interval _a1 (b1, b2) _a2 _a3))

      | Type.Builtin Ast.Maps_to ->
        Type.builtin_term (
          fun ast args ->
            begin match args with
              | [var; t] ->
                let t = Type.parse_term env t in
                begin match var with
                  | { Ast.term = Ast.Symbol sym; _ }
                  | { Ast.term =
                        Ast.App ({ Ast.term = Ast.Symbol sym; _ }, []); _
                    } ->
                    begin
                      match Type.find_bound env sym with
                      | `Term_var v -> T.maps_to v t
                      | _ -> Type._error env (Ast ast) (Type.Cannot_find (sym, ""))
                    end
                  | _ -> Type._error env (Ast ast) (Type.Expected ("Variable name", None))
                end
              | l -> Type._error env (Ast ast) (Type.Bad_op_arity (s, [2], List.length l))
            end
        )

      | _ -> `Not_found

  end

end

(* Dimacs builtins *)
(* ************************************************************************ *)

module Dimacs = struct

  module Tff
      (Type : Tff_intf.S)
      (T : Dolmen.Intf.Term.Dimacs with type t = Type.T.t) = struct

    let parse env s =
      match s with
      | Type.Builtin Ast.Not ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.neg)
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
        Type.builtin_ttype (Base.app0 (module Type) env s ())
      | Type.Id { name = Simple "$o"; ns = Term } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.prop)
      | Type.Id { name = Simple "$i"; ns = Term } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.base)
      | Type.Id { name = Simple "$true"; ns = Term } ->
        Type.builtin_term (Base.app0 (module Type) env s T._true)
      | Type.Id { name = Simple "$false"; ns = Term } ->
        Type.builtin_term (Base.app0 (module Type) env s T._false)

      (* Predefined connectives *)
      | Type.Builtin Ast.Eq ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.eq)
      | Type.Builtin Ast.Distinct ->
        Type.builtin_term (Base.term_app_list (module Type) env s T.distinct)
      | Type.Id { name = Simple "$distinct"; ns = Term; } ->
        Type.builtin_term (Base.term_app_list (module Type) env s T.distinct)

      | Type.Builtin Ast.Not ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.neg)
      | Type.Builtin Ast.Or ->
        Type.builtin_term (Base.term_app2 (module Type) env s mk_or)
      | Type.Builtin Ast.And ->
        Type.builtin_term (Base.term_app2 (module Type) env s mk_and)
      | Type.Builtin Ast.Xor ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.xor)
      | Type.Builtin Ast.Nor ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.nor)
      | Type.Builtin Ast.Nand ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.nand)
      | Type.Builtin Ast.Equiv ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.equiv)
      | Type.Builtin Ast.Imply ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.imply)
      | Type.Builtin Ast.Implied ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.implied)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        Type.builtin_term (
          Base.make_op3 (module Type) env s (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Ignore the role and kind attributes *)
      | Type.Id id when Id.equal id Id.tptp_role ->
        Type.builtin_tags (fun _ast _args -> [])
      | Type.Id id when Id.equal id Id.tptp_kind ->
        Type.builtin_tags (fun _ast _args -> [])
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
        Type.builtin_ttype (Base.app0 (module Type) env s ())
      | Type.Id { name = Simple "$o"; ns = Term } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.prop)
      | Type.Id { name = Simple "$i"; ns = Term } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.base)

      (* Predefined symbols *)
      | Type.Id { name = Simple "$true"; ns = Term } ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const._true)
      | Type.Id { name = Simple "$false"; ns = Term } ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const._false)

      (* Predefined connectives *)
      | Type.Builtin Ast.Eq ->
        Type.builtin_term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.eq)))
      | Type.Builtin Ast.Distinct ->
        Type.builtin_term (Base.term_app_list (module Type) env s T.distinct)
      | Type.Id { name = Simple "$distinct"; ns = Term; } ->
        Type.builtin_term (Base.term_app_list (module Type) env s T.distinct)

      | Type.Builtin Ast.Not ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.neg)
      | Type.Builtin Ast.Or ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.or_)
      | Type.Builtin Ast.And ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.and_)
      | Type.Builtin Ast.Xor ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.xor)
      | Type.Builtin Ast.Nor ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.nor)
      | Type.Builtin Ast.Nand ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.nand)
      | Type.Builtin Ast.Equiv ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.equiv)
      | Type.Builtin Ast.Imply ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.imply)
      | Type.Builtin Ast.Implied ->
        Type.builtin_term (Base.term_app_cst (module Type) env T.Const.implied)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        Type.builtin_term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.ite)))

      (* Pi & Sigma *)
      | Type.Builtin Ast.Pi ->
        Type.builtin_term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.pi)))
      | Type.Builtin Ast.Sigma ->
        Type.builtin_term (Base.term_app_ho_ast (module Type) env
                 (fun ast -> Type.monomorphize env ast (T.of_cst T.Const.sigma)))

      (* Ignore the role and kind attributes *)
      | Type.Id id when Id.equal id Id.tptp_role ->
        Type.builtin_tags (fun _ast _args -> [])
      | Type.Id id when Id.equal id Id.tptp_kind ->
        Type.builtin_tags (fun _ast _args -> [])
      | _ -> `Not_found

  end

end

(* Smtlib builtins (bool, =, etc...) *)
(* ************************************************************************ *)

module Smtlib2 = struct

  (* Conversion of sexpr to terms/types *)

  exception Empty_sexpr of Ast.t
  exception Bad_index_in_sexpr of Ast.t
  exception Unexpected_structure_in_sexpr of Ast.t
  exception Uninterpreted_reserved_word_in_sexpr of Id.t * Ast.t

  let uninterpreted_reserved_word = function
    | "!"
    | "let"
    | "exists"
    | "forall"
    | "match"
    | "par"
    | "assert"
    | "check-sat"
    | "check-sat-assuming"
    | "declare-const"
    | "declare-datatype"
    | "declare-datatypes"
    | "declare-fun"
    | "declare-sort"
    | "define-fun"
    | "define-fun-rec"
    | "define-funs-rec"
    | "define-sort"
    | "echo"
    | "exit"
    | "get-assertions"
    | "get-assignment"
    | "get-info"
    | "get-model"
    | "get-option"
    | "get-proof"
    | "get-unsat-assumptions"
    | "get-unsat-core"
    | "get-value"
    | "pop"
    | "push"
    | "reset"
    | "reset-assertions"
    | "set-info"
    | "set-logic"
    | "set-option" -> true
    | _ -> false

  let index_of_sexpr ast =
    match (ast : Ast.t) with
    | { term = Symbol {
        name = Simple index;
        ns = (Value (Integer|Hexadecimal|Binary));
      }; _ } ->
      index
    | _ -> raise (Bad_index_in_sexpr ast)

  let rec sexpr_as_term sexpr =
    match (sexpr : Ast.t) with

    (* "as" type annotation *)
    | { term = App ({ term = Builtin Sexpr; _ }, {
        term = App ({ term = Builtin Sexpr; _ }, [
            { term = Symbol { name = Simple "as"; _ }; _}; f; ty]);
        loc = loc_as; attr = attr_as
      } :: args); loc = loc_out; attr = attr_out} ->
      let ty = sexpr_as_sort ty in
      let f = sexpr_as_term f in
      let args = List.map sexpr_as_term args in
      let function_app =
        Ast.apply ~loc:loc_out f args
        |> Ast.add_attrs attr_out
      in
      Ast.colon ~loc:loc_as function_app ty
      |> Ast.add_attrs attr_as

    | { term = App ({ term = Builtin Sexpr; _ }, [
        { term = Symbol { name = Simple "as"; _ }; _}; f; ty])
      ; loc=loc_as; attr=attr_as } ->
      let f = sexpr_as_term f in
      let ty = sexpr_as_sort ty in
      Ast.colon ~loc:loc_as f ty
      |> Ast.add_attrs attr_as

    (* indexed identifiers *)
    | { term = App ({ term = Builtin Sexpr; _ }, {
        term = Symbol { ns; name = Simple "_"} ; _} ::
        { term = Symbol {name = Simple s; _}; _ } :: args); loc; attr } ->
      Ast.const ~loc (Id.indexed ns s (List.map index_of_sexpr args))
      |> Ast.add_attrs attr

    (* regular function application *)
    | { term = App ({ term = Builtin Sexpr; _ }, []); _ }  ->
      raise (Empty_sexpr sexpr)
    | { term = App ({ term = Builtin Sexpr; _ }, f :: args); loc; attr } ->
      let f = sexpr_as_term f in
      let args = List.map sexpr_as_term args in
      Ast.apply ~loc f args
      |> Ast.add_attrs attr

    (* check that we handle all Sexpr cases *)
    | { term = App({ term = Builtin Sexpr; _ }, _); _ } -> .

    (* direct symbols *)
    | { term = Symbol ({ name = Simple s; _ } as id); _ } ->
      if uninterpreted_reserved_word s
      then raise (Uninterpreted_reserved_word_in_sexpr (id, sexpr))
      else sexpr

    (* fallback *)
    | _ -> raise (Unexpected_structure_in_sexpr sexpr)

  and sexpr_as_sort sexpr =
    match (sexpr : Ast.t) with

    (* Symbol must have their namespace changed to correctly be interpreted
       as sorts/types *)
    | { term = Symbol ({ ns = Term; name = (Simple s as name); } as id) ; loc; attr} ->
      if uninterpreted_reserved_word s
      then raise (Uninterpreted_reserved_word_in_sexpr (id, sexpr))
      else
        Ast.const ~loc (Id.create Sort name)
        |> Ast.add_attrs attr

    (* indexed identifiers *)
    | { term = App ({ term = Builtin Sexpr; _ }, {
        term = Symbol { ns = Term; name = Simple "_"} ; _} ::
        { term = Symbol {name = Simple s; _}; _ } :: args); loc; attr } ->
      Ast.const ~loc (Id.indexed Sort s (List.map index_of_sexpr args))
      |> Ast.add_attrs attr

    (* type constructor application *)
    | { term = App ({ term = Builtin Sexpr; _ }, []); _ }  ->
      raise (Empty_sexpr sexpr)
    | { term = App ({ term = Builtin Sexpr; _ }, f :: args); loc; attr } ->
      let f = sexpr_as_sort f in
      let args = List.map sexpr_as_sort args in
      Ast.apply ~loc f args
      |> Ast.add_attrs attr

    (* check that we handle all Sexpr cases *)
    | { term = App({ term = Builtin Sexpr; _ }, _); _ } -> .

    (* fallback *)
    | _ -> raise (Unexpected_structure_in_sexpr sexpr)


  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Smtlib_Base with type 'a t = 'a Type.Tag.t
                                          and type term := Type.T.t)
      (Ty : Dolmen.Intf.Ty.Smtlib_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Base with type t = Type.T.t
                                         and type cstr := Type.T.Cstr.t) = struct

    type _ Type.err +=
      | Incorrect_sexpression : Dolmen.Intf.Msg.t -> Dolmen.Std.Term.t Type.err

    let inferred_model_constants = Dolmen.Std.Tag.create ()

    let add_model_constant state c =
      let l =
        match Type.get_global_custom_state state
                inferred_model_constants with
        | None -> []
        | Some l -> l
      in
      Type.set_global_custom_state state inferred_model_constants (c :: l)

    let parse_name env = function
      | ({ Ast.term = Ast.Symbol s; _ } as ast)
      | ({ Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, []); _ } as ast) ->
        begin match Dolmen.Std.Id.name s with
          | Simple s -> s
          | _ -> Type._error env (Ast ast) (Type.Expected ("simple name", None))
        end
      | ast ->
        Type._error env (Ast ast) (Type.Expected ("symbol", None))

    let extract_sexpr_list_from_sexpr env = function
      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Sexpr; _ }, l); _} -> l
      | ast ->
        Type._error env (Ast ast)
          (Type.Expected ("a list of terms in a sexpr", None))

    let parse_sexpr env sexpr =
      match sexpr_as_term sexpr with
      | term -> Type.parse_term env term
      | exception Empty_sexpr ast ->
        let msg = Format.dprintf "empty s-expression" in
        Type._error env (Ast ast) (Incorrect_sexpression msg)
      | exception Bad_index_in_sexpr ast ->
        let msg = Format.dprintf "indexes of indexed identifiers must be integers" in
        Type._error env (Ast ast) (Incorrect_sexpression msg)
      | exception Uninterpreted_reserved_word_in_sexpr (id, ast) ->
        let msg =
          Format.dprintf
            "the reserved word '%a' is currently not interpreted in s-expressions,@ \
             please report upstream if you think it should be interpreted" Id.print id
        in
        Type._error env (Ast ast) (Incorrect_sexpression msg)
      | exception Unexpected_structure_in_sexpr ast ->
        let msg = Format.dprintf
            "unexpected structure in a s-expression,@ \
             please report upstream"
        in
        Type._error env (Ast ast) (Incorrect_sexpression msg)

    let mk_or a b = T._or [a; b]
    let mk_and a b = T._and [a; b]

    let parse (version : Dolmen.Smtlib2.version) env s =
      match s with
      (* Bool sort and constants *)
      | Type.Id { name = Simple "Bool"; ns = Sort } ->
        Type.builtin_ty (Base.app0 (module Type) env s Ty.prop)
      | Type.Id { name = Simple "true"; ns = Term } ->
        Type.builtin_term (Base.app0 (module Type) env s T._true)
      | Type.Id { name = Simple "false"; ns = Term } ->
        Type.builtin_term (Base.app0 (module Type) env s T._false)

      (* Boolean operators *)
      | Type.Id { name = Simple "not"; ns = Term } ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.neg)
      | Type.Id { name = Simple "and"; ns = Term } ->
        Type.builtin_term (Base.term_app_left (module Type) env s mk_and)
      | Type.Id { name = Simple "or"; ns = Term } ->
        Type.builtin_term (Base.term_app_left (module Type) env s mk_or)
      | Type.Id { name = Simple "xor"; ns = Term } ->
        Type.builtin_term (Base.term_app_left (module Type) env s T.xor)
      | Type.Id { name = Simple "=>"; ns = Term } ->
        Type.builtin_term (Base.term_app_right (module Type) env s T.imply)

      (* If-then-else *)
      | Type.Id { name = Simple "ite"; ns = Term } ->
        Type.builtin_term (
          Base.make_op3 (module Type) env s (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Equality *)
      | Type.Id { name = Simple "distinct"; ns = Term } ->
        Type.builtin_term (fun _ast args ->
            let args = List.map (Type.parse_term env) args in
            T.distinct args)
      | Type.Id { name = Simple "="; ns = Term } ->
        Type.builtin_term (Base.term_app_chain (module Type) env s T.eq)

      (* Named formulas *)
      | Type.Id { name = Simple ":named"; ns = Attr } ->
        Type.builtin_tags (Base.make_op1 (module Type) env s (fun _ t ->
            let name = parse_name env t in
            [Type.Set (Tag.named, name)]
          ))

      (* Trigger annotations *)
      | Type.Id { name = Simple ":pattern"; ns = Attr } ->
        Type.builtin_tags (Base.make_op1 (module Type) env s (fun _ t ->
            let l = extract_sexpr_list_from_sexpr env t in
            let l = List.map (parse_sexpr env) l in
            [Type.Add (Tag.triggers, l)]
          ))

      (* Rewrite rules *)
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        Type.builtin_tags (fun _ast _args -> [Type.Set (Tag.rwrt, ())])

      (* ADT testers *)
      | Type.Id { Id.ns = Term; name = Indexed { basename; indexes; } } as symbol ->
        Base.parse_indexed basename indexes
          ~k:(fun _ -> `Not_found)
          ~err:(fun _ _ _ -> `Not_found) (function
              | "is" -> `Unary (function s ->
                  let id = Id.mk Term s in
                  begin match Type.find_bound env id with
                    | `Cstr c ->
                      Type.builtin_term (Base.term_app1 (module Type) env symbol (Type.T.cstr_tester c))
                    | _ -> `Not_found
                  end)
              | _ -> `Not_indexed
            )

      (* Abstract *)
      | Type.Id { Id.ns = Term; name = Simple name; } ->
        if String.length name <= 1 then `Not_found
        else begin
          match name.[0] with
          | '.' ->
            begin match version with
              | `Script _ ->
                `Reserved (
                  "solver-generated function symbols other than abstract values",
                  `Solver)
              (* this is effectively a namespace reserved for solver-generated symbols
                 in their output. In such case, we expect these symbols to be explicitly
                 bound/defined somewher, so we need not do anything specific. *)
              | `Response _ -> `Not_found
            end
          | '@' ->
            begin match version with
              | `Script _ -> `Reserved ("abstract values in models", `Solver)
              | `Response _ ->
                (* the var infer does not matter *)
                let var_infer = Type.var_infer env in
                let sym_infer = Type.sym_infer env in
                (* Avoid capturing `env` in the hook. *)
                let state = Type.state env in
                let sym_hook cst =
                  sym_infer.sym_hook cst;
                  match cst with
                  | `Ty_cst _ -> ()
                  | `Term_cst c -> add_model_constant state c
                in
                let sym_infer : Type.sym_infer = {
                  sym_hook;
                  infer_type_csts = false;
                  infer_term_csts = Wildcard Any_in_scope;
                } in
                `Infer ("abstract values (i.e. constants)", var_infer, sym_infer)
            end
          | _ -> `Not_found
        end

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
        Type.builtin_ty (Base.app0 (module Type) env s Ty.prop)

      (* Terms *)
      | Type.Builtin Ast.True ->
        Type.builtin_term (Base.app0 (module Type) env s T._true)
      | Type.Builtin Ast.False ->
        Type.builtin_term (Base.app0 (module Type) env s T._false)
      | Type.Builtin Ast.Not ->
        Type.builtin_term (Base.term_app1 (module Type) env s T.neg)
      | Type.Builtin Ast.Or ->
        Type.builtin_term (Base.term_app2 (module Type) env s mk_or)
      | Type.Builtin Ast.And ->
        Type.builtin_term (Base.term_app2 (module Type) env s mk_and)
      | Type.Builtin Ast.Imply ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.imply)
      | Type.Builtin Ast.Equiv ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.equiv)
      | Type.Builtin Ast.Eq ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.eq)
      | Type.Builtin Ast.Distinct ->
        Type.builtin_term (Base.term_app2 (module Type) env s T.neq)

      (* Ite *)
      | Type.Builtin Ast.Ite ->
        Type.builtin_term (
          Base.make_op3 (module Type) env s (fun _ (a, b, c) ->
              let cond = Type.parse_prop env a in
              let then_ = Type.parse_term env b in
              let else_ = Type.parse_term env c in
              T.ite cond then_ else_
            )
        )

      (* Tags *)
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        Type.builtin_tags (fun _ast _args -> [Type.Set (Tag.rwrt, ())])
      | Type.Id { name = Simple "infix"; ns = Term } ->
        Type.builtin_tags (fun ast args -> match args with
            | [ { Ast.term = Ast.Symbol { name = Simple name; _ }; _ } ] -> [
                Type.Set (Tag.name, Tag.exact name);
                Type.Set (Tag.pos, Tag.infix);
              ]
            | _ ->
              Type._error env (Ast ast)
                (Type.Expected ("a symbol", None))
          )
      | Type.Id { name = Simple "prefix"; ns = Term } ->
        Type.builtin_tags (fun ast args -> match args with
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

