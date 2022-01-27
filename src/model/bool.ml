
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

let ops = Value.ops
    ~compare:(fun b b' -> Stdlib.compare b b')
    ~print:(fun fmt b -> Format.fprintf fmt "%b" b)

(* Builtins *)
(* ************************************************************************* *)

module B = Dolmen.Std.Builtin

let true_v = Value.mk ~ops true
let false_v = Value.mk ~ops false
let mk b = if b then true_v else false_v

let fun1 f ~cst =
  let func = function
    | [b] -> mk (f (Value.extract_exn ~ops b))
    | _ -> assert false
  in
  Fun.builtin ~arity:1 ~cst func

let fun2 f ~cst =
  let func = function
    | [a; b] ->
      mk (f (Value.extract_exn ~ops a) (Value.extract_exn ~ops b))
    | _ -> assert false
  in
  Fun.builtin ~arity:2 ~cst func

let builtins (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.True -> Some true_v
  | B.False -> Some false_v
  | B.Neg -> Some (fun1 ~cst not)
  | B.And -> Some (fun2 ~cst (&&))
  | B.Or -> Some (fun2 ~cst (||))
  | B.Nand -> Some (fun2 ~cst (fun b1 b2 -> not (b1 && b2)))
  | B.Nor -> Some (fun2 ~cst (fun b1 b2 -> not (b1 || b2)))
  | B.Xor -> Some (fun2 ~cst (fun b1 b2 -> b1 <> b2))
  | B.Imply -> Some (fun2 ~cst (fun b1 b2 -> not b1 || b2))
  | B.Implied -> Some (fun2 ~cst (fun b1 b2 -> not b2 || b1))
  | B.Equiv -> Some (fun2 ~cst (fun b1 b2 -> b1 = b2))
  | _ -> None

