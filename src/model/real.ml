(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

let ops = Value.ops
    ~compare:(fun b b' -> Stdlib.compare b b')
    ~print:(fun fmt b -> Format.fprintf fmt "%a" Q.pp_print b)
    ()

(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let mk i = Value.mk ~ops i

let fun1 f ~cst =
  Fun.fun_1 ~cst (fun x -> f (Value.extract_exn ~ops x))

let fun2 f ~cst =
  Fun.fun_2 ~cst (fun x y ->
      f (Value.extract_exn ~ops x) (Value.extract_exn ~ops y)
    )

let cmp ~cst p =
  Some (fun2 ~cst (fun x y -> Bool.mk @@ p x y))

let op2 ~cst f =
  Some (fun2 ~cst (fun x y -> mk @@ f x y))
let op1 ~cst f =
  Some (fun1 ~cst (fun x -> mk @@ f x))

let half = { Q.num = Z.one; Q.den = Z.of_int 2 }

let floor x = Q.of_bigint @@ Int.floor x
let ceil x = Q.of_bigint @@ Int.ceil x
let truncate x = Q.of_bigint @@ Int.truncate x
let round x = if Q.lt Q.zero x then ceil (Q.sub x half) else floor (Q.add x half)

let div_e a b =
  let s = Q.sign b in
  let d = Q.div a b in
  if s > 0 then floor d else ceil d

let div_t a b = truncate (Q.div a b)
let div_f a b = floor (Q.div a b)
let mod_e a b = Q.sub a (div_e a b)
let mod_t a b = Q.sub a (div_t a b)
let mod_f a b = Q.sub a (div_f a b)

let builtins (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Decimal i
  | B.Rational i -> Some (mk (Q.of_string i))
  | B.Lt (`Rat | `Real) -> cmp ~cst Q.lt
  | B.Gt (`Rat | `Real) -> cmp ~cst Q.gt
  | B.Geq (`Rat | `Real) -> cmp ~cst Q.geq
  | B.Leq (`Rat | `Real) -> cmp ~cst Q.leq
  | B.Minus (`Rat | `Real) -> op1 ~cst (fun x -> Q.neg x)
  | B.Add (`Rat | `Real) -> op2 ~cst Q.add
  | B.Sub (`Rat | `Real) -> op2 ~cst Q.sub
  | B.Mul (`Rat | `Real) -> op2 ~cst Q.mul
  | B.Div (`Rat | `Real) -> op2 ~cst Q.div
  | B.Div_e (`Rat | `Real) -> op2 ~cst div_e
  | B.Div_t (`Rat | `Real) -> op2 ~cst div_t
  | B.Div_f (`Rat | `Real) -> op2 ~cst div_f
  | B.Modulo_e (`Rat | `Real) -> op2 ~cst mod_e
  | B.Modulo_t (`Rat | `Real) -> op2 ~cst mod_t
  | B.Modulo_f (`Rat | `Real) -> op2 ~cst mod_f
  | B.Is_int (`Rat | `Real) -> Some (fun1 ~cst (fun x -> Bool.mk (Z.equal Z.one x.Q.den)))
  | B.Is_rat (`Rat | `Real) -> Some (Bool.mk true)
  | B.Floor (`Rat | `Real) -> op1 ~cst floor
  | B.Ceiling (`Rat | `Real) -> op1 ~cst ceil
  | B.Truncate  (`Rat | `Real) -> op1 ~cst truncate
  | B.Round (`Rat | `Real) -> op1 ~cst round
  | _ -> None
