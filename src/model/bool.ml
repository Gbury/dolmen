
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

let ops = Value.ops
    ~compare:(fun b b' -> Stdlib.compare b b')
    ~print:(fun fmt b -> Format.fprintf fmt "%b" b)
    ()

(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let true_v = Value.mk ~ops true
let false_v = Value.mk ~ops false
let mk b = if b then true_v else false_v

let fun1 f ~cst =
  Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> mk @@ f (Value.extract_exn ~ops x))

let fun2 f ~cst =
  Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y ->
      mk @@ f (Value.extract_exn ~ops x) (Value.extract_exn ~ops y)
    )

let fun_n f ~cst =
  let func l =
    let l' = List.map (Value.extract_exn ~ops) l in
    mk (f l')
  in
  Fun.fun_n ~cst func

let builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.True -> Some true_v
  | B.False -> Some false_v
  | B.Neg -> Some (fun1 ~cst not)
  | B.And -> Some (fun_n ~cst @@ fun l -> List.fold_left (&&) true l)
  | B.Or -> Some (fun_n ~cst @@ fun l -> List.fold_left (||) false l)
  | B.Nand -> Some (fun2 ~cst (fun b1 b2 -> not (b1 && b2)))
  | B.Nor -> Some (fun2 ~cst (fun b1 b2 -> not (b1 || b2)))
  | B.Xor -> Some (fun2 ~cst (fun b1 b2 -> b1 <> b2))
  | B.Imply -> Some (fun2 ~cst (fun b1 b2 -> not b1 || b2))
  | B.Implied -> Some (fun2 ~cst (fun b1 b2 -> not b2 || b1))
  | B.Equiv -> Some (fun2 ~cst (fun b1 b2 -> b1 = b2))
  | _ -> None

