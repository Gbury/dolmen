
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Arithmetic conversions *)
(* ************************************************************************* *)

let int_to_rat v =
  let z = Value.extract_exn ~ops:Int.ops v in
  Rat.mk (Q.of_bigint z)

let int_to_real v =
  let z = Value.extract_exn ~ops:Int.ops v in
  Real.mk (Q.of_bigint z)

(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
exception Unsupported_coercion of E.Ty.t * E.Ty.t

let coerce src dst =
  if E.Ty.equal src dst then (fun value -> value)
  else match E.Ty.view src, E.Ty.view dst with
    | `Int, `Rat -> int_to_rat
    | `Int, `Real -> int_to_real
    | _, _ -> raise (Unsupported_coercion (src, dst))

let builtins _env (cst : E.Term.Const.t) =
  match cst.builtin with
  | Dolmen.Std.Builtin.Coercion ->
    Some (Fun.ad_hoc ~cst (fun src ->
        Fun.ad_hoc ~cst ~ty_args:[src] (fun dst ->
            Fun.fun_1 ~cst ~ty_args:[src; dst] (coerce src dst)
          )))
  | _ -> None

