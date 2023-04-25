(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

open Farith

let ops_rm =
  Value.ops
    ~compare:Mode.compare
    ~print:Mode.pp
    ()

let ops =
  Value.ops
    ~compare:(fun b b' -> F.compare b b')
    ~print:(fun fmt b -> Format.fprintf fmt "%a" F.pp b)
    ()


(* Configuration for corner cases *)
(* ************************************************************************* *)

exception Min_Max_with_different_zeros
exception FP_to_BV_undefined

(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let mk f = Value.mk ~ops f

let mode v = (Value.extract_exn ~ops:ops_rm v)
let fp v = (Value.extract_exn ~ops v)

let test ~cst p = Some (Fun.fun_1 ~cst (fun x -> Bool.mk @@ p (fp x)))
let cmp ~cst p = Some (Fun.fun_2 ~cst (fun x y -> Bool.mk @@ p (fp x) (fp y)))
let op2_mode ~cst f =
  Some (Fun.fun_3 ~cst (fun m x y -> mk @@ f (mode m) (fp x) (fp y)))
let op1_mode ~cst f =
  Some (Fun.fun_2 ~cst (fun m x -> mk @@ f (mode m) (fp x)))
let op1 ~cst f = Some (Fun.fun_1 ~cst(fun x -> mk @@ f (fp x)))

let min_max ~cmp ~cst =
      Some (Fun.fun_2 ~cst (fun x y ->
        let x = fp x in
        let y = fp y in
        match F.classify x, F.classify y with
        | NaN, _ -> mk y
        | _, NaN -> mk x
        | PZero, NZero | NZero, PZero -> raise Min_Max_with_different_zeros
        | _ -> if cmp x y then mk x else mk y
      ))

let round_q ~neg ~mw ~ew mode r =
  if Q.equal Q.zero r then F.zero ~mw ~ew neg else F.of_q ~mw ~ew mode r

let nearest_no_tie x =
  (* the tie break should be handled before *)
  assert (not (Z.equal x.Q.den (Z.of_int 2)));
  Int.ceil (Q.sub x (Q.make Z.one (Z.of_int 2)))


let toIntegral mode q =
  match mode with
  | Mode.NE ->
    if Z.equal (Z.of_int 2) q.Q.den then
      (* denominator is 2 so in the middle *)
      let r = Int.floor q in
      (if Z.is_even r then r else Z.succ r)
    else
      (nearest_no_tie q)
  | Mode.NA ->
    if Z.equal (Z.of_int 2) q.Q.den then
      (* denominator is 2 so in the middle *)
      let r = if Z.sign q.Q.num < 0 then Int.floor q else Int.ceil q in
      r
    else
      (nearest_no_tie q)
  | Mode.ZR -> (Int.truncate q)
  | Mode.DN -> (Int.floor q)
  | Mode.UP -> (Int.ceil q)



let builtins _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.RoundNearestTiesToEven -> Some (Value.mk ~ops:ops_rm Mode.NE)
  | B.RoundNearestTiesToAway -> Some (Value.mk ~ops:ops_rm Mode.NA)
  | B.RoundTowardPositive -> Some (Value.mk ~ops:ops_rm Mode.UP)
  | B.RoundTowardNegative -> Some (Value.mk ~ops:ops_rm Mode.DN)
  | B.RoundTowardZero -> Some (Value.mk ~ops:ops_rm Mode.ZR)
  | B.Real_to_fp (ew, prec) ->
    Some (Fun.fun_2 ~cst (fun m r ->
        mk (F.of_q ~ew ~mw:(prec - 1) (mode m) (Real.get r))))
  | B.Fp_to_fp (_ew1, _prec1, ew2, prec2) ->
    Some (Fun.fun_2 ~cst
            (fun m f1 -> mk @@ F.round ~ew:ew2 ~mw:(prec2 - 1) (mode m) (fp f1)))
  | B.Sbv_to_fp (n, ew, prec) ->
    Some (Fun.fun_2 ~cst (fun m bv ->
       mk @@ F.of_q ~ew ~mw:(prec - 1) (mode m) (Q.of_bigint (Bitv.sbitv n bv))))
  | B.Ubv_to_fp (n, ew, prec) ->
    Some (Fun.fun_2 ~cst (fun m bv ->
       mk @@ F.of_q ~ew ~mw:(prec - 1) (mode m) (Q.of_bigint (Bitv.ubitv n bv))))
  | B.Fp (ew, prec) ->
    Some (Fun.fun_3 ~cst (fun bvs bve bvm ->
      mk @@
      F.of_bits ~ew ~mw:(prec - 1)
           (Z.logor
              (Z.logor
                 (Z.shift_left
                    (Bitv.ubitv 1 bvs)
                    (ew + prec - 1))
                 (Z.shift_left
                    (Bitv.ubitv ew bve)
                    (prec - 1)))
              (Bitv.ubitv (prec - 1) bvm))))
  | B.Ieee_format_to_fp (ew, prec) ->
    Some (Fun.fun_1 ~cst (fun bv ->
        mk @@
        F.of_bits ~ew ~mw:(prec - 1) (Bitv.ubitv (ew + prec) bv)))
  | B.To_real (_ew, _prec) ->
    Some (Fun.fun_1 ~cst (fun f -> Real.mk @@ (F.to_q (fp f))))
  | B.Plus_infinity (ew, prec) ->
    Some (mk @@ F.inf ~ew ~mw:(prec - 1) false)
  | B.Minus_infinity (ew, prec) ->
    Some (mk @@ F.inf ~ew ~mw:(prec - 1) true)
  | B.NaN (ew, prec) ->
    Some (mk @@ F.nan ~ew ~mw:(prec - 1))
  | B.Plus_zero (ew, prec) ->
    Some (mk @@ F.zero ~ew ~mw:(prec - 1) false)
  | B.Minus_zero (ew, prec) ->
    Some (mk @@ F.zero ~ew ~mw:(prec - 1) true)
  | B.Fp_add (_ew, _prec) ->
    op2_mode ~cst F.add
  | B.Fp_sub (_ew, _prec) ->
    op2_mode ~cst F.sub
  | B.Fp_mul (_ew, _prec) ->
    op2_mode ~cst F.mul
  | B.Fp_abs (_ew, _prec) ->
    op1 ~cst F.abs
  | B.Fp_neg (_ew, _prec) ->
    op1 ~cst F.neg
  | B.Fp_sqrt (_ew, _prec) ->
    op1_mode ~cst F.sqrt
  | B.Fp_div (_ew, _prec) ->
    op2_mode ~cst F.div
  | B.Fp_fma (_ew, _prec) ->
    Some (Fun.fun_4 ~cst (fun m x y z -> mk @@ F.fma (mode m) (fp x) (fp y) (fp z)))
  | B.Fp_eq (_ew, _prec) ->
    cmp ~cst F.eq
  | B.Fp_leq (_ew, _prec) ->
    cmp ~cst F.le
  | B.Fp_lt (_ew, _prec) ->
    cmp ~cst F.lt
  | B.Fp_geq (_ew, _prec) ->
    cmp ~cst F.ge
  | B.Fp_gt (_ew, _prec) ->
    cmp ~cst F.gt
  | B.Fp_isInfinite (_ew, _prec) ->
    test ~cst F.is_infinite
  | B.Fp_isZero (_ew, _prec) ->
      test ~cst F.is_zero
  | B.Fp_isNaN (_ew, _prec) ->
      test ~cst F.is_nan
  | B.Fp_isNegative (_ew, _prec) ->
    test ~cst F.is_negative
  | B.Fp_isPositive (_ew, _prec) ->
    test ~cst F.is_positive
  | B.Fp_isNormal (_ew, _prec) ->
    test ~cst F.is_normal
  | B.Fp_isSubnormal (_ew, _prec) ->
    test ~cst F.is_subnormal
  | B.Fp_rem (ew,prec) ->
      Some (Fun.fun_2 ~cst (fun f g ->
        let f = fp f in
        let g = fp g in
        let mode = Farith.Mode.NE in
        let mw = prec - 1 in
        match F.classify f, F.classify g with
        | (NaN | PInf | NInf), _ -> mk (F.nan ~ew ~mw)
        | _, (NaN | PZero | NZero) -> mk (F.nan ~ew ~mw)
        | _, (PInf | NInf) -> mk f
        | (PZero | NZero | PNormal | NNormal | PSubn | NSubn) ,
          (PNormal | NNormal | PSubn | NSubn) ->
          let qf = F.to_q f and qg = F.to_q g in
          let y = toIntegral mode (Q.div qf qg) in
          let x = Q.sub qf (Q.mul qg (Q.of_bigint y)) in
          mk (round_q ~neg:(F.is_negative f) mode ~ew ~mw x)
      ))
  | B.Fp_roundToIntegral (ew,prec) ->
          Some (Fun.fun_2 ~cst (fun m f ->
        let f = fp f in
        let mode = mode m in
        let mw = prec - 1 in
        match F.classify f with
        | (NaN | PInf | NInf | PZero | NZero) -> mk f
        | (PNormal | NNormal | PSubn | NSubn) ->
          let q = F.to_q f in
          let n = toIntegral mode q in
          mk (round_q ~neg:(F.is_negative f) ~mw ~ew mode (Q.of_bigint n))
      ))
  | B.Fp_min (_ew,_prec) -> min_max ~cmp:F.lt ~cst
  | B.Fp_max (_ew,_prec) -> min_max ~cmp:F.gt ~cst
  | B.To_ubv (_ew,_prec,size) ->
    Some (Fun.fun_2 ~cst (fun m f ->
        let f = fp f in
        let mode = mode m in
        match F.classify f with
        | (NaN | PInf | NInf) -> raise FP_to_BV_undefined
        | (PNormal | NNormal | PSubn | NSubn | PZero | NZero) ->
          let q = F.to_q f in
          let n = toIntegral mode q in
          if Z.sign n >= 0 && Z.numbits n <= size then
            Bitv.mk size n
          else raise FP_to_BV_undefined
      ))
  | B.To_sbv (_ew,_prec,size) ->
    Some (Fun.fun_2 ~cst (fun m f ->
        let f = fp f in
        let mode = mode m in
        match F.classify f with
        | (NaN | PInf | NInf) -> raise FP_to_BV_undefined
        | (PNormal | NNormal | PSubn | NSubn | PZero | NZero) ->
          let q = F.to_q f in
          let n = toIntegral mode q in
          let n' = Z.extract n 0 size in
          if Z.equal n (Z.signed_extract n' 0 size) then
            Bitv.mk size n'
          else raise FP_to_BV_undefined
      ))
  | _ -> None
