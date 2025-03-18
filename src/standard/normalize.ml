
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(*
type denv = {
  ty_subst : Expr.Ty.subst;
}

type dacc = {
  unit : unit;
}

type uenv = {
  unit : unit;
}

type uacc = {
  unit : unit;
}

let rec normalize_ty k denv dacc ty =
  match Expr.Ty.descr ty with
  | Expr.TyVar v -> normalize_ty_var k denv dacc v
  | Expr.TyApp (f, args) -> normalize_ty_app k denv dacc f args
  | Expr.Arrow (params, ret) -> normalize_ty_arrow k denv dacc params ret
  | Expr.Pi (vars, body) -> normalize_ty_pi k denv dacc vars body

and normalize_ty_var k denv dacc v =
  assert false

and normalize_ty_app k denv dacc f args =
  assert false

and normalize_ty_arrow k denv dacc params ret =
  assert false

and normalize_ty_pi k denv dacc vars body =
  assert false
*)
