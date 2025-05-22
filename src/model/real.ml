
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

let raw_ceil x = Q.of_bigint @@ Int.raw_ceil x
let raw_floor x = Q.of_bigint @@ Int.raw_floor x
let raw_truncate x = Q.of_bigint @@ Int.raw_truncate x

let half = { Q.num = Z.one; Q.den = Z.of_int 2 }

let round x =
  if Q.lt Q.zero x then raw_ceil (Q.sub x half) else raw_floor (Q.add x half)

let div_e a b =
  let s = Q.sign b in
  let d = Q.div a b in
  if s > 0 then raw_floor d else raw_ceil d

let div_t a b = raw_truncate (Q.div a b)
let div_f a b = raw_floor (Q.div a b)
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

let decimal i = Some (mk (Q.of_string i))
let lt ~cst = cmp ~cst Q.lt
let gt ~cst = cmp ~cst Q.gt
let geq ~cst = cmp ~cst Q.geq
let leq ~cst = cmp ~cst Q.leq
let minus ~cst = op1 ~cst (fun x -> Q.neg x)
let add ~cst = op2 ~cst Q.add
let sub ~cst = op2 ~cst Q.sub
let mul ~cst = op2 ~cst Q.mul
let div ~cst ~eval ~env = op2_zero Q.div ~cst ~env ~eval
let div_e ~cst = op2 ~cst div_e
let div_t ~cst = op2 ~cst div_t
let div_f ~cst = op2 ~cst div_f
let mod_e ~cst = op2 ~cst mod_e
let mod_t ~cst = op2 ~cst mod_t
let mod_f ~cst = op2 ~cst mod_f
(* TODO: fix this when we use algebraic numbers *)
let is_rat ~cst = Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun _ -> Bool.mk true))
let floor ~cst = op1 ~cst raw_floor
let floor_to_int ~cst = Some (fun1 ~cst (fun x -> Int.mk (Int.raw_floor x)))
let ceiling ~cst = op1 ~cst raw_ceil
let truncate ~cst = op1 ~cst raw_truncate
let round ~cst = op1 ~cst round
let is_int ~cst = Some (fun1 ~cst (fun x -> Bool.mk (Z.equal Z.one x.Q.den)))
let pow ~cst:_ = assert false (* not implemented yet *)

