
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Builtin dispatch *)
(* ************************************************************************* *)

let builtins ~eval env (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | Dolmen.Std.Builtin.Arith blt ->
    begin match blt with
      (* Integers *)
      | Int -> assert false (* Types are not evaluated *)
      | Integer i -> Int.integer i
      | Lt `Int -> Int.lt ~cst
      | Gt `Int -> Int.gt ~cst
      | Geq `Int -> Int.geq ~cst
      | Leq `Int -> Int.leq ~cst
      | Minus `Int -> Int.minus ~cst
      | Add `Int -> Int.add ~cst
      | Sub `Int -> Int.sub ~cst
      | Mul `Int -> Int.mul ~cst
      | Pow `Int -> Int.pow ~cst
      | Div_e `Int -> Int.div_e ~cst ~eval ~env
      | Div_t `Int -> Int.div_t ~cst
      | Div_f `Int -> Int.div_f ~cst
      | Modulo_e `Int -> Int.mod_e ~cst ~eval ~env
      | Modulo_t `Int -> Int.mod_t ~cst
      | Modulo_f `Int -> Int.mod_f ~cst
      | Divisible -> Int.divisible ~cst
      | Abs -> Int.abs ~cst
      | Is_int `Int -> Int.is_int ~cst
      | Is_rat `Int -> Int.is_rat ~cst
      | Floor `Int -> Int.floor ~cst
      | Ceiling `Int -> Int.ceiling ~cst
      | Truncate `Int -> Int.truncate ~cst
      | Round `Int -> Int.round ~cst
      (* Rationals *)
      | Rat -> assert false (* Types are not evaluated *)
      | Rational i -> Rat.rational i
      | Lt `Rat -> Rat.lt ~cst
      | Gt `Rat -> Rat.gt ~cst
      | Geq `Rat -> Rat.geq ~cst
      | Leq `Rat -> Rat.leq ~cst
      | Minus `Rat -> Rat.minus ~cst
      | Add `Rat -> Rat.add ~cst
      | Sub `Rat -> Rat.sub ~cst
      | Mul `Rat -> Rat.mul ~cst
      | Div `Rat -> Rat.div ~cst
      | Div_e `Rat -> Rat.div_e ~cst
      | Div_t `Rat -> Rat.div_t ~cst
      | Div_f `Rat -> Rat.div_f ~cst
      | Modulo_e `Rat -> Rat.mod_e ~cst
      | Modulo_t `Rat -> Rat.mod_t ~cst
      | Modulo_f `Rat -> Rat.mod_f ~cst
      | Floor `Rat -> Rat.floor ~cst
      | Floor_to_int `Rat -> Rat.floor_to_int ~cst
      | Ceiling `Rat -> Rat.ceiling ~cst
      | Truncate `Rat -> Rat.truncate ~cst
      | Round `Rat -> Rat.round ~cst
      | Is_int `Rat -> Rat.is_int ~cst
      | Is_rat `Rat -> Rat.is_rat ~cst
      (* Reals *)
      | Real -> assert false (* Types are not evaluated *)
      | Decimal i -> Real.decimal i
      | Lt `Real -> Real.lt ~cst
      | Gt `Real -> Real.gt ~cst
      | Geq `Real -> Real.geq ~cst
      | Leq `Real -> Real.leq ~cst
      | Minus `Real -> Real.minus ~cst
      | Add `Real -> Real.add ~cst
      | Sub `Real -> Real.sub ~cst
      | Mul `Real -> Real.mul ~cst
      | Div `Real -> Real.div ~cst ~env ~eval
      | Div_e `Real -> Real.div_e ~cst
      | Div_t `Real -> Real.div_t ~cst
      | Div_f `Real -> Real.div_f ~cst
      | Modulo_e `Real -> Real.mod_e ~cst
      | Modulo_t `Real -> Real.mod_t ~cst
      | Modulo_f `Real -> Real.mod_f ~cst
      | Is_rat `Real -> Real.is_rat ~cst
      | Floor `Real -> Real.floor ~cst
      | Floor_to_int `Real -> Real.floor_to_int ~cst
      | Ceiling `Real -> Real.ceiling ~cst
      | Truncate `Real -> Real.truncate ~cst
      | Round `Real -> Real.round ~cst
      | Is_int `Real -> Real.is_int ~cst
      | Pow `Real -> Real.pow ~cst
    end
  | _ -> None

