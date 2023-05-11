
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module E = Dolmen.Std.Expr
exception Unsupported_coercion of E.Ty.t * E.Ty.t

(* Arithmetic conversions *)
(* ************************************************************************* *)

let cst = E.Term.Const.coerce

let int_to_rat =
  let v =
    Fun.fun_1 ~cst (fun v ->
        let z = Value.extract_exn ~ops:Int.ops v in
        Rat.mk (Q.of_bigint z))
  in
  [E.Ty.int; E.Ty.rat], (fun _ -> v)

let int_to_real =
  let v =
    Fun.fun_1 ~cst (fun v ->
        let z = Value.extract_exn ~ops:Int.ops v in
        Real.mk (Real.A.of_bigint z))
  in
  [E.Ty.int; E.Ty.real], (fun _ -> v)

let fallback =
  let a = E.Ty.Var.mk "a" in
  let b = E.Ty.Var.mk "b" in
  [E.Ty.of_var a; E.Ty.of_var b], (fun subst ->
      let a_ty = E.Subst.Var.get a subst in
      let b_ty = E.Subst.Var.get b subst in
      raise (Unsupported_coercion (a_ty, b_ty))
    )

(* Builtins *)
(* ************************************************************************* *)

let builtins ~eval:_ _env (cst : E.Term.Const.t) =
  match cst.builtin with
  | Dolmen.Std.Builtin.Coercion ->
    Some (Fun.mk_clos @@ Fun.ad_hoc ~cst ~ty_arity:2 ~arity:1 [
        int_to_rat;
        int_to_real;
        fallback;
      ])
  | _ -> None

