
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

type t = Z.t

let compare = Z.compare
let print fmt z =
  Format.fprintf fmt "%a" Z.pp_print z

let ops : t Value.ops = Value.ops ~compare ~print ()

(* Helper functions on unbounded integers *)
(* ************************************************************************* *)

let raw_ceil x = Z.cdiv x.Q.num x.Q.den
let raw_floor x = Z.fdiv x.Q.num x.Q.den
let raw_truncate x = Z.div x.Q.num x.Q.den (* it is truncated toward zero *)

let div_e a b =
  let s = Z.sign b in
  let d = Q.div (Q.of_bigint a) (Q.of_bigint b) in
  if s > 0 then raw_floor d else raw_ceil d

let mod_e a b = Z.sub a (Z.mul (div_e a b) b)

let div_f a b = raw_floor (Q.div (Q.of_bigint a) (Q.of_bigint b))
let mod_f a b = Z.sub a (Z.mul (div_f a b) b)

let div_t a b = raw_truncate (Q.div (Q.of_bigint a) (Q.of_bigint b))
let mod_t a b = Z.sub a (Z.mul (div_t a b) b)

let pow x y =
  if Z.equal Z.zero x then Z.zero
  else if Z.equal Z.one x then Z.one
  else if Z.equal Z.minus_one x then
    if Z.is_odd y then Z.minus_one else Z.one
  else
    (* TODO: add check that y is in range of ints *)
    Z.pow x (Z.to_int y)


(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let mk i = Value.mk ~ops i

let fun1 f ~cst =
  Fun.mk_clos @@ Fun.fun_1 ~cst (fun x ->
      mk @@ f (Value.extract_exn ~ops x))

let fun2 f ~cst =
  Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y ->
      f (Value.extract_exn ~ops x) (Value.extract_exn ~ops y))

let op1 ~cst f = Some (fun1 ~cst (fun x -> f x))
let op2 ~cst f = Some (fun2 ~cst (fun x y -> mk @@ f x y))
let cmp ~cst p = Some (fun2 ~cst (fun x y -> Bool.mk @@ p x y))

let op2_zero ~eval ~env ~cst f =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y ->
      let v_x = Value.extract_exn ~ops x in
      let v_y = Value.extract_exn ~ops y in
      if Z.equal Z.zero v_y then
        Fun.corner_case ~eval env cst [] [x; y]
      else
        mk @@ f v_x v_y
    ))

let integer i = Some (mk (Z.of_string i))
let lt ~cst = cmp ~cst Z.lt
let gt ~cst = cmp ~cst Z.gt
let geq ~cst = cmp ~cst Z.geq
let leq ~cst = cmp ~cst Z.leq
let minus ~cst = Some (fun1 ~cst (fun x -> Z.neg x))
let add ~cst = op2 ~cst Z.add
let sub ~cst = op2 ~cst Z.sub
let mul ~cst = op2 ~cst Z.mul
let pow ~cst = op2 ~cst pow
let div_e ~cst ~eval ~env = op2_zero ~eval ~env ~cst div_e
let div_t ~cst = op2 ~cst div_t
let div_f ~cst = op2 ~cst div_f
let mod_e ~cst ~eval ~env = op2_zero ~eval ~env ~cst mod_e
let mod_t ~cst = op2 ~cst mod_t
let mod_f ~cst = op2 ~cst mod_f
let divisible ~cst = Some (fun2 ~cst (fun x y -> Bool.mk @@ Z.divisible x y))
let abs ~cst = op1 ~cst Z.abs
let is_int ~cst = Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun _ -> Bool.mk true))
let is_rat ~cst = Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun _ -> Bool.mk true))
let floor ~cst = op1 ~cst (fun x -> x)
let ceiling ~cst = op1 ~cst (fun x -> x)
let truncate ~cst = op1 ~cst (fun x -> x)
let round ~cst = op1 ~cst (fun x -> x)

