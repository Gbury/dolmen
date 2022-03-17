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
  Fun.fun_1 ~cst (fun x -> mk @@ f (Value.extract_exn ~ops x))

let fun2 f ~cst =
  Fun.fun_2 ~cst (fun x y ->
      f (Value.extract_exn ~ops x) (Value.extract_exn ~ops y)
    )

let cmp ~cst p =
  Some (fun2 ~cst (fun x y -> Bool.mk @@ p x y))

let op2 ~cst f =
  Some (fun2 ~cst (fun x y -> mk @@ f x y))
let op1 ~cst f =
  Some (fun1 ~cst (fun x -> f x))

let floor x = Q.of_bigint @@ Int.floor x
let ceil x = Q.of_bigint @@ Int.ceil x
let truncate x = Q.of_bigint @@ Int.truncate x

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
  let arg_is_real () =
    match E.Ty.poly_sig cst.id_ty with
    | _, ty::_, _ -> E.Ty.equal E.Ty.real ty
    | _ -> false
  in
  match cst.builtin with
  | B.Decimal i
  | B.Rational i -> Some (mk (Q.of_string i))
  | B.Lt when arg_is_real () -> cmp ~cst Q.lt
  | B.Gt when arg_is_real () -> cmp ~cst Q.gt
  | B.Geq when arg_is_real () -> cmp ~cst Q.geq
  | B.Leq when arg_is_real () -> cmp ~cst Q.leq
  | B.Minus when arg_is_real () -> Some (fun1 ~cst (fun x -> Q.neg x))
  | B.Add when arg_is_real () -> op2 ~cst Q.add
  | B.Sub when arg_is_real () -> op2 ~cst Q.sub
  | B.Mul when arg_is_real () -> op2 ~cst Q.mul
  | B.Div when arg_is_real () -> op2 ~cst Q.div
  | B.Div_e when arg_is_real () -> op2 ~cst div_e
  | B.Div_t when arg_is_real () -> op2 ~cst div_t
  | B.Div_f when arg_is_real () -> op2 ~cst div_f
  | B.Modulo_e when arg_is_real () -> op2 ~cst mod_e
  | B.Modulo_t when arg_is_real () -> op2 ~cst mod_t
  | B.Modulo_f when arg_is_real () -> op2 ~cst mod_f
  | B.Abs when arg_is_real () -> op1 ~cst Q.abs
  | (B.Is_int| B.Is_rat) when arg_is_real () -> Some (Bool.mk true)
  | B.Floor | B.Ceiling | B.Truncate | B.Round when arg_is_real () -> op1 ~cst (fun x -> x)
  | _ -> None
