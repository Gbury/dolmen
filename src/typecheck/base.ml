
open Dolmen
module Ast = Dolmen.Term

(* Merging builtin parser functions *)
(* ************************************************************************ *)

let noop _ _ _ _ = None

let rec merge l env ast s args =
  match l with
  | [] -> None
  | f :: r ->
    begin match f env ast s args with
      | (Some _) as ret -> ret
      | None -> merge r env ast s args
    end

(* Smtlib logic merging *)

type smtlib_theory = [
  | `Core
  | `Arrays
  | `Bitvectors
  | `Ints
  | `Reals
  | `Reals_Ints
]

type smtlib_restriction = [
  | `No_free_symbol
  | `Quantifier_free
  | `Difference_logic
  | `Linear_arithmetic
]

type smtlib_logic = {
  theories      : smtlib_theory list;
  restrictions  : smtlib_restriction list;
}

(*
QF for the restriction to quantifier free formulas
A or AX for the theory ArraysEx
BV for the theory FixedSizeBitVectors
FP (forthcoming) for the theory FloatingPoint
IA for the theory Ints (Integer Arithmetic)
RA for the theory Reals (Real Arithmetic)
IRA for the theory Reals_Ints (mixed Integer Real Arithmetic)
IDL for Integer Difference Logic
RDL for Rational Difference Logic
L before IA, RA, or IRA for the linear fragment of those arithmetics
N before IA, RA, or IRA for the non-linear fragment of those arithmetics
UF for the extension allowing free sort and function symbols
*)

let smtlib_logic s =
  let add_restrictions c r = { c with restrictions = r::c.restrictions } in
  let add_theories c r = { c with theories = r::c.theories } in
  let rec aux c = function
    | [] -> Some c
    | 'Q'::'F'::'_'::l -> aux (add_restrictions c `Quantifier_free) l
    | 'U'::'F'::l -> aux c l
    | 'D'::'T'::l -> aux c l
    | 'I'::'D'::'L'::l ->  aux (add_theories (add_restrictions c `Difference_logic) `Ints) l
    | 'R'::'D'::'L'::l ->  aux (add_theories (add_restrictions c `Difference_logic) `Reals) l
    | 'L'::'I'::'A'::l -> aux (add_theories (add_restrictions c `Linear_arithmetic) `Ints) l
    | 'L'::'R'::'A'::l -> aux (add_theories (add_restrictions c `Linear_arithmetic) `Reals) l
    | 'L'::'I'::'R'::'A'::l -> aux (add_theories (add_restrictions c `Linear_arithmetic) `Reals_Ints) l
    | 'N'::'I'::'A'::l -> aux (add_theories c `Ints) l
    | 'N'::'R'::'A'::l -> aux (add_theories c `Reals) l
    | 'N'::'I'::'R'::'A'::l -> aux (add_theories c `Reals_Ints) l
    | 'A'::'X'::l | 'A'::l  -> aux (add_theories c `Arrays) l
    | 'B'::'V'::l -> aux (add_theories c `Bitvectors) l
    | _ -> None
  in
  aux { theories = [ `Core ]; restrictions = [] } (List.of_seq (String.to_seq s))


(* Building builtins parser functions *)
(* ************************************************************************ *)

type ('env, 'args, 'ret) helper =
  (module Tff_intf.S with type env = 'env) ->
  'env -> Dolmen.Term.t -> string -> Dolmen.Term.t list ->
  ('args -> 'ret) -> 'ret option

let make_op0
    (type env) (module Type: Tff_intf.S with type env = env)
    env ast op args ret =
  match args with
  | [] -> Some (ret ())
  | _ ->
    let err = Type.Bad_op_arity (op, 0, List.length args) in
    raise (Type.Typing_error (err, env, ast))

let make_op1
    (type env) (module Type: Tff_intf.S with type env = env)
    env ast op args ret =
  match args with
  | [t1] -> Some (ret t1)
  | _ ->
    let err = Type.Bad_op_arity (op, 1, List.length args) in
    raise (Type.Typing_error (err, env, ast))

let make_op2
    (type env) (module Type: Tff_intf.S with type env = env)
    env ast op args ret =
  match args with
  | [t1; t2] -> Some (ret (t1, t2))
  | _ ->
    let err = Type.Bad_op_arity (op, 2, List.length args) in
    raise (Type.Typing_error (err, env, ast))

let make_op3
    (type env) (module Type: Tff_intf.S with type env = env)
    env ast op args ret =
  match args with
  | [t1; t2; t3] -> Some (ret (t1, t2, t3))
  | _ ->
    let err = Type.Bad_op_arity (op, 3, List.length args) in
    raise (Type.Typing_error (err, env, ast))

let make_opn n
    (type env) (module Type: Tff_intf.S with type env = env)
    env ast op args ret =
  let l = List.length args in
  if l = n then
    Some (ret args)
  else begin
    let err = Type.Bad_op_arity (op, 1, List.length args) in
    raise (Type.Typing_error (err, env, ast))
  end

let make_assoc
    (type env) (module Type: Tff_intf.S with type env = env)
    env ast op args ret =
  match args with
  | [] | [_] ->
    let err = Type.Bad_op_arity (op, 2, List.length args) in
    raise (Type.Typing_error (err, env, ast))
  | _ -> Some (ret args)

let fold_left_assoc mk = function
  | h :: r -> List.fold_left mk h r
  | _ -> raise (Invalid_argument "Base.fold_left_assoc")

let rec fold_right_assoc mk = function
  | [x] -> x
  | h :: r -> mk h (fold_right_assoc mk r)
  | _ -> raise (Invalid_argument "Base.fold_right_assoc")

let make_chain = make_assoc

let map_chain
    (type t) (module Type: Tff_intf.S with type T.t = t) mk args =
  let rec aux mk = function
    | [] -> assert false
    | [_] -> []
    | x :: ((y :: _) as r) -> mk x y :: aux mk r
  in
  match aux mk args with
  | [] -> assert false
  | [x] -> x
  | l -> Type.T._and l

(* Alt-ergo builtins *)
(* ************************************************************************ *)

module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Base with type t = Type.T.t) = struct

    let parse env ast s args =
      match s with
      | Type.Builtin Term.Bool ->
        make_op0 (module Type) env ast "bool" args
          (fun () -> Type.Ty Ty.bool)
      | Type.Builtin Term.Unit ->
        make_op0 (module Type) env ast "unit" args
          (fun () -> Type.Ty Ty.unit)
      | Type.Builtin Term.Void ->
        make_op0 (module Type) env ast "void" args
          (fun () -> Type.Term T.void)
      | _ -> None

  end

end

(* TPTP builtins ($i, $o, etc..) *)
(* ************************************************************************ *)

module Tptp = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Base with type t = Type.T.t) = struct

    let parse _version env ast s args =
      match s with
      (*
      | Type.Id ({ Id.name = "$_"; ns = Id.Term } as id) ->
        Some (Type.wildcard env ast id args)
      *)
      | Type.Id { Id.name = "$tType"; ns = Id.Term } ->
        make_op0 (module Type) env ast "$tType" args
          (fun () -> Type.Ttype)
      | Type.Id { Id.name = "$o"; ns = Id.Term } ->
        make_op0 (module Type) env ast "$o" args
          (fun () -> (Type.Ty Ty.prop))
      | Type.Id { Id.name = "$i"; ns = Id.Term } ->
        make_op0 (module Type) env ast "$i" args
          (fun () -> (Type.Ty Ty.base))
      | Type.Id { Id.name = "$true"; ns = Id.Term } ->
        make_op0 (module Type) env ast "$true" args
          (fun () -> Type.Term T._true)
      | Type.Id { Id.name = "$false"; ns = Id.Term } ->
        make_op0 (module Type) env ast "$false" args
          (fun () -> Type.Term T._false)
      | Type.Id id when Id.equal id Id.tptp_role ->
        Some (Type.Tags [])
      | _ -> None

  end

end

(* Smtlib builtins (bool, =, etc...) *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Smtlib_Base with type 'a t = 'a Type.Tag.t)
      (Ty : Dolmen.Intf.Ty.Smtlib_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Base with type t = Type.T.t) = struct

    let app_left env ast args name mk =
      make_assoc (module Type) env ast name args
        (fun l -> Type.Term (fold_left_assoc mk (List.map (Type.parse_term env) l)))

    let app_right env ast args name mk =
      make_assoc (module Type) env ast name args
        (fun l -> Type.Term (fold_right_assoc mk (List.map (Type.parse_term env) l)))

    let parse_symbol env = function
      | { Ast.term = Ast.Symbol s; _ }
      | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, []); _ } ->
        Id.full_name s
      | ast ->
        raise (Type.Typing_error (Type.Expected ("symbol", None), env, ast))

    let parse_f env ast cstr args =
      let loc = Term.(ast.loc) in
      let t = Term.apply ?loc cstr args in
      Type.Term (Type.parse_term env t)

    let parse _version env ast s args =
      match s with
      (* Bool sort and constants *)
      | Type.Id { Id.name = "Bool"; ns = Id.Sort } ->
        make_op0 (module Type) env ast "Bool" args
          (fun () -> (Type.Ty Ty.prop))
      | Type.Id { Id.name = "true"; ns = Id.Term } ->
        make_op0 (module Type) env ast "true" args
          (fun () -> (Type.Term Type.T._true))
      | Type.Id { Id.name = "false"; ns = Id.Term } ->
        make_op0 (module Type) env ast "false" args
          (fun () -> (Type.Term Type.T._false))

      (* Boolean operators *)
      | Type.Id { Id.name = "not"; ns = Id.Term } ->
        make_op1 (module Type) env ast "not" args
          (fun t -> Type.Term (Type.T.neg (Type.parse_term env t)))
      | Type.Id { Id.name = "and"; ns = Id.Term } ->
        Some (parse_f env ast (Term.and_t ()) args)
      | Type.Id { Id.name = "or"; ns = Id.Term } ->
        Some (parse_f env ast (Term.or_t ()) args)
      | Type.Id { Id.name = "xor"; ns = Id.Term } ->
        app_left env ast args "xor" Type.T.xor
      | Type.Id { Id.name = "=>"; ns = Id.Term } ->
        app_right env ast args "=>" Type.T.imply

      (* If-then-else *)
      | Type.Id { Id.name = "ite"; ns = Id.Term } ->
        make_op3 (module Type) env ast "ite" args
        (fun (c, a, b) ->
          let loc = ast.Term.loc in
          let ast = Term.ite ?loc c a b in
          Type.Term (Type.parse_term env ast))

      (* Equality *)
      | Type.Id { Id.name = "distinct"; ns = Id.Term } ->
        Some (parse_f env ast (Term.neq_t ()) args)
      | Type.Id { Id.name = "="; ns = Id.Term } ->
        let l = List.map (Type.parse_term env) args in
        Some (Type.Term (T.eqs l))

      (* Named formulas *)
      | Type.Id { Id.name = ":named"; ns = Id.Attr } ->
        make_op1 (module Type) env ast ":named" args
          (fun t ->
             let name = parse_symbol env t in
             Type.Tags [Type.Any (Tag.named, name)]
          )

      (* Rewrite rules *)
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        Some (Type.Tags [Type.Any (Tag.rwrt, ())])

      | _ -> None

  end

end

(* Zipperposition builtins *)
(* ************************************************************************ *)

module Zf = struct

  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Zf_Base with type 'a t = 'a Type.Tag.t) = struct

    let parse _env _ast s args =
      match s with
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        Some (Type.Tags [Type.Any (Tag.rwrt, ())])
      | Type.Id { Id.name = "infix"; ns = Id.Term } ->
        begin match args with
          | [ { Term.term = Term.Symbol { Id.name; _ }; _ } ] ->
            Some (Type.Tags [
                Type.Any (Tag.name, Tag.exact name);
                Type.Any (Tag.pos, Tag.infix);
              ])
          | _ -> assert false
        end
      | Type.Id { Id.name = "prefix"; ns = Id.Term } ->
        begin match args with
          | [ { Term.term = Term.Symbol { Id.name; _ }; _ } ] ->
            Some (Type.Tags [
                Type.Any (Tag.name, Tag.exact name);
                Type.Any (Tag.pos, Tag.prefix);
              ])
          | _ -> assert false
        end
      | _ -> None

  end

end

