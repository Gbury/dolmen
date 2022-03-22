(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

let ops =
  Value.ops
    ~compare:(fun b b' -> Stdlib.compare b b')
    ~print:(fun fmt b -> Format.fprintf fmt "%a" Z.pp_print b)
    ()

(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let mk i = Value.mk ~ops i
let fun1 f ~cst = Fun.fun_1 ~cst (fun x -> mk @@ f (Value.extract_exn ~ops x))

let fun2 f ~cst =
  Fun.fun_2 ~cst (fun x y ->
      f (Value.extract_exn ~ops x) (Value.extract_exn ~ops y))

let cmp ~cst p = Some (fun2 ~cst (fun x y -> Bool.mk @@ p x y))
let op2 ~cst f = Some (fun2 ~cst (fun x y -> mk @@ f x y))
let op1 ~cst f = Some (fun1 ~cst (fun x -> f x))
let floor x = Z.fdiv x.Q.num x.Q.den
let ceil x = Z.cdiv x.Q.num x.Q.den
let truncate d = if Q.sign d > 0 then floor d else ceil d

let div_e a b =
  let s = Z.sign b in
  let d = Q.div (Q.of_bigint a) (Q.of_bigint b) in
  if s > 0 then floor d else ceil d

let div_t a b = truncate (Q.div (Q.of_bigint a) (Q.of_bigint b))
let div_f a b = floor (Q.div (Q.of_bigint a) (Q.of_bigint b))
let mod_e a b = Z.sub a (Z.mul (div_e a b) b)
let mod_t a b = Z.sub a (Z.mul (div_t a b) b)
let mod_f a b = Z.sub a (Z.mul (div_f a b) b)

let builtins (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Integer i -> Some (mk (Z.of_string i))
  | B.Lt `Int -> cmp ~cst Z.lt
  | B.Gt `Int -> cmp ~cst Z.gt
  | B.Geq `Int -> cmp ~cst Z.geq
  | B.Leq `Int -> cmp ~cst Z.leq
  | B.Minus `Int -> Some (fun1 ~cst (fun x -> Z.neg x))
  | B.Add `Int -> op2 ~cst Z.add
  | B.Sub `Int -> op2 ~cst Z.sub
  | B.Mul `Int -> op2 ~cst Z.mul
  | B.Pow `Int ->
      op2 ~cst (fun x y ->
          if Z.equal Z.zero x then Z.zero
          else if Z.equal Z.one x then Z.one
          else if Z.equal Z.minus_one x then
            if Z.is_odd y then Z.minus_one else Z.one
          else Z.pow x (Z.to_int y))
  | B.Div_e `Int -> op2 ~cst div_e
  | B.Div_t `Int -> op2 ~cst div_t
  | B.Div_f `Int -> op2 ~cst div_f
  | B.Modulo_e `Int -> op2 ~cst mod_e
  | B.Modulo_t `Int -> op2 ~cst mod_t
  | B.Modulo_f `Int -> op2 ~cst mod_f
  | B.Divisible -> Some (fun2 ~cst (fun x y -> Bool.mk @@ Z.divisible x y))
  | B.Abs -> op1 ~cst Z.abs
  | B.Is_int `Int | B.Is_rat `Int -> Some (Bool.mk true)
  | B.Floor `Int | B.Ceiling `Int | B.Truncate `Int | B.Round `Int ->
      op1 ~cst (fun x -> x)
  | _ -> None
