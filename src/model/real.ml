
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

(* Real are currently represented as rationals.
   This is correct for quantifier free logic, since without non-rational
   contants, we do not have a way to create non-rational values.
   However, we may need to upgrade this representation to a more complete
   one when we add quantifiers and/or new constants/operators on real
   values. *)
type t = Q.t

let compare = Q.compare
let print fmt r =
  Format.fprintf fmt "%a" Q.pp_print r

let ops = Value.ops ~compare ~print ()

(* Value helpers *)
(* ************************************************************************* *)

let ceil x = Q.of_bigint @@ Int.ceil x
let floor x = Q.of_bigint @@ Int.floor x
let truncate x = Q.of_bigint @@ Int.truncate x

let half = { Q.num = Z.one; Q.den = Z.of_int 2 }

let round x =
  if Q.lt Q.zero x then ceil (Q.sub x half) else floor (Q.add x half)

let div_e a b =
  let s = Q.sign b in
  let d = Q.div a b in
  if s > 0 then floor d else ceil d

let div_t a b = truncate (Q.div a b)
let div_f a b = floor (Q.div a b)
let mod_e a b = Q.sub a (Q.mul (div_e a b) b)
let mod_t a b = Q.sub a (Q.mul (div_t a b) b)
let mod_f a b = Q.sub a (Q.mul (div_f a b) b)


(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let mk i = Value.mk ~ops i
let get v = Value.extract_exn ~ops v

let fun1 f ~cst =
  Fun.mk_clos @@ Fun.fun_1 ~cst (fun x ->
      f (Value.extract_exn ~ops x))

let fun2 f ~cst =
  Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y ->
      f (Value.extract_exn ~ops x) (Value.extract_exn ~ops y))

let op1 ~cst f = Some (fun1 ~cst (fun x -> mk @@ f x))
let op2 ~cst f = Some (fun2 ~cst (fun x y -> mk @@ f x y))
let cmp ~cst p = Some (fun2 ~cst (fun x y -> Bool.mk @@ p x y))

let op2_zero ~eval ~env ~cst f =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y ->
      let v_x = Value.extract_exn ~ops x in
      let v_y = Value.extract_exn ~ops y in
      if Q.equal Q.zero v_y then
        Fun.corner_case ~eval env cst [] [x; y]
      else mk @@ f v_x v_y
    ))

let builtins ~eval env (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Decimal i -> Some (mk (Q.of_string i))
  | B.Lt `Real -> cmp ~cst Q.lt
  | B.Gt `Real -> cmp ~cst Q.gt
  | B.Geq `Real -> cmp ~cst Q.geq
  | B.Leq `Real -> cmp ~cst Q.leq
  | B.Minus `Real -> op1 ~cst (fun x -> Q.neg x)
  | B.Add `Real -> op2 ~cst Q.add
  | B.Sub `Real -> op2 ~cst Q.sub
  | B.Mul `Real -> op2 ~cst Q.mul
  | B.Div `Real -> op2_zero Q.div ~cst ~env ~eval
  | B.Div_e `Real -> op2 ~cst div_e
  | B.Div_t `Real -> op2 ~cst div_t
  | B.Div_f `Real -> op2 ~cst div_f
  | B.Modulo_e `Real -> op2 ~cst mod_e
  | B.Modulo_t `Real -> op2 ~cst mod_t
  | B.Modulo_f `Real -> op2 ~cst mod_f
  | B.Is_rat `Real -> Some (Bool.mk true)
  | B.Floor `Real -> op1 ~cst floor
  | B.Ceiling `Real -> op1 ~cst ceil
  | B.Truncate `Real -> op1 ~cst truncate
  | B.Round `Real -> op1 ~cst round
  | B.Is_int `Real ->
    Some (fun1 ~cst (fun x -> Bool.mk (Z.equal Z.one x.Q.den)))
  | _ -> None

