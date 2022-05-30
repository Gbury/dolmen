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

let builtins (cst : Dolmen.Std.Expr.Term.Const.t) =
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
  | _ -> None
