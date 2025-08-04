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

exception Unhandled_exponand_and_mantissa of { ew : int; mw : int; }

let mk f = Value.mk ~ops f
let fp v = (Value.extract_exn ~ops v)
let mode v = (Value.extract_exn ~ops:ops_rm v)

let check ~ew ~mw =
  (* Note: this is a bit of a hack, since the `GenericFloat` module is not
     exposed in the main interface of `Farith` *)
  if not (Farith__GenericFloat.check_param (Z.of_int mw) (Z.of_int ew))
  then raise (Unhandled_exponand_and_mantissa { ew; mw = mw + 1; })

let test ~cst p =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> Bool.mk @@ p (fp x)))
let cmp ~cst p =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y -> Bool.mk @@ p (fp x) (fp y)))
let op2_mode ~cst f =
  Some (Fun.mk_clos @@ Fun.fun_3 ~cst (fun m x y -> mk @@ f (mode m) (fp x) (fp y)))
let op1_mode ~cst f =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun m x -> mk @@ f (mode m) (fp x)))
let op1 ~cst f =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst(fun x -> mk @@ f (fp x)))

let f_of_q ~ew ~mw mode q =
  check ~ew ~mw;
  F.of_q ~ew ~mw mode q

let f_round ~mw ~ew mode f =
  check ~ew ~mw;
  F.round ~ew ~mw mode f

let f_of_bits ~mw ~ew bits =
  check ~ew ~mw;
  F.of_bits ~ew ~mw bits

let f_inf ~mw ~ew plus =
  check ~ew ~mw;
  F.inf ~mw ~ew plus

let f_zero ~mw ~ew plus =
  check ~mw ~ew;
  F.zero ~mw ~ew plus

let f_nan ~mw ~ew =
  check ~ew ~mw;
  F.nan ~ew ~mw

let round_q ~neg ~mw ~ew mode r =
  check ~mw ~ew;
  if Q.equal Q.zero r then F.zero ~mw ~ew neg else F.of_q ~mw ~ew mode r

let min_max ~eval env ~cmp ~cst =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun x' y' ->
      let x = fp x' in
      let y = fp y' in
      match F.classify x, F.classify y with
      | NaN, _ -> mk y
      | _, NaN -> mk x
      | PZero, NZero | NZero, PZero ->
        Fun.corner_case ~eval env cst [] [x'; y']
          ~post_check:(fun res ->
              match F.classify (fp res) with
              | PZero | NZero -> ()
              | _ -> raise (Model.Incorrect_extension (cst, [x'; y'], res)))
      | _ -> if cmp x y then mk x else mk y
    ))


let nearest_no_tie x =
  (* the tie break should be handled before *)
  assert (not (Z.equal x.Q.den (Z.of_int 2)));
  Int.raw_ceil (Q.sub x (Q.make Z.one (Z.of_int 2)))


let toIntegral mode q =
  match mode with
  | Mode.NE ->
    if Z.equal (Z.of_int 2) q.Q.den then
      (* denominator is 2 so in the middle *)
      let r = Int.raw_floor q in
      (if Z.is_even r then r else Z.succ r)
    else
      (nearest_no_tie q)
  | Mode.NA ->
    if Z.equal (Z.of_int 2) q.Q.den then
      (* denominator is 2 so in the middle *)
      let r = if Z.sign q.Q.num < 0 then Int.raw_floor q else Int.raw_ceil q in
      r
    else
      (nearest_no_tie q)
  | Mode.ZR -> (Int.raw_truncate q)
  | Mode.DN -> (Int.raw_floor q)
  | Mode.UP -> (Int.raw_ceil q)


let builtins ~eval env (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | Dolmen.Std.Builtin.Float blt ->
    begin match blt with
      | T _ | RoundingMode -> assert false (* Types are not evaluated *)
      | RoundNearestTiesToEven -> Some (Value.mk ~ops:ops_rm Mode.NE)
      | RoundNearestTiesToAway -> Some (Value.mk ~ops:ops_rm Mode.NA)
      | RoundTowardPositive -> Some (Value.mk ~ops:ops_rm Mode.UP)
      | RoundTowardNegative -> Some (Value.mk ~ops:ops_rm Mode.DN)
      | RoundTowardZero -> Some (Value.mk ~ops:ops_rm Mode.ZR)
      | Of_real { e = ew; s = prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun m r ->
            check ~ew ~mw:(prec - 1);
            mk (f_of_q ~ew ~mw:(prec - 1) (mode m) (Real.get r))))
      | To_fp { e1 = _ew1; s1 = _prec1; e2 = ew2; s2 = prec2; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst
                (fun m f1 -> mk @@ f_round ~ew:ew2 ~mw:(prec2 - 1) (mode m) (fp f1)))
      | Of_sbv { m = n; e = ew; s = prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun m bv ->
            mk @@ f_of_q ~ew ~mw:(prec - 1) (mode m) (Q.of_bigint (Bitv.sbitv n bv))))
      | Of_ubv { m = n; e = ew; s = prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun m bv ->
            mk @@ f_of_q ~ew ~mw:(prec - 1) (mode m) (Q.of_bigint (Bitv.ubitv n bv))))
      | Fp { e = ew; s = prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_3 ~cst (fun bvs bve bvm ->
            mk @@
            f_of_bits ~ew ~mw:(prec - 1)
              (Z.logor
                 (Z.logor
                    (Z.shift_left
                       (Bitv.ubitv 1 bvs)
                       (ew + prec - 1))
                    (Z.shift_left
                       (Bitv.ubitv ew bve)
                       (prec - 1)))
                 (Bitv.ubitv (prec - 1) bvm))))
      | Ieee_format_to_fp { e = ew; s = prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun bv ->
            mk @@
            f_of_bits ~ew ~mw:(prec - 1) (Bitv.ubitv (ew + prec) bv)))
      | To_real { e = _ew; s = _prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun f -> Real.mk @@ (F.to_q (fp f))))
      | Plus_infinity { e = ew; s = prec; } ->
        Some (mk @@ f_inf ~ew ~mw:(prec - 1) false)
      | Minus_infinity { e = ew; s = prec; } ->
        Some (mk @@ f_inf ~ew ~mw:(prec - 1) true)
      | NaN { e = ew; s = prec; } ->
        Some (mk @@ f_nan ~ew ~mw:(prec - 1))
      | Plus_zero { e = ew; s = prec; } ->
        Some (mk @@ f_zero ~ew ~mw:(prec - 1) false)
      | Minus_zero { e = ew; s = prec; } ->
        Some (mk @@ f_zero ~ew ~mw:(prec - 1) true)
      | Add { e = _ew; s = _prec; } ->
        op2_mode ~cst F.add
      | Sub { e = _ew; s = _prec; } ->
        op2_mode ~cst F.sub
      | Mul { e = _ew; s = _prec; } ->
        op2_mode ~cst F.mul
      | Abs { e = _ew; s = _prec; } ->
        op1 ~cst F.abs
      | Neg { e = _ew; s = _prec; } ->
        op1 ~cst F.neg
      | Sqrt { e = _ew; s = _prec; } ->
        op1_mode ~cst F.sqrt
      | Div { e = _ew; s = _prec; } ->
        op2_mode ~cst F.div
      | Fma { e = _ew; s = _prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_4 ~cst (fun m x y z ->
            mk @@ F.fma (mode m) (fp x) (fp y) (fp z)))
      | Eq { e = _ew; s = _prec; } ->
        cmp ~cst F.eq
      | Leq { e = _ew; s = _prec; } ->
        cmp ~cst F.le
      | Lt { e = _ew; s = _prec; } ->
        cmp ~cst F.lt
      | Geq { e = _ew; s = _prec; } ->
        cmp ~cst F.ge
      | Gt { e = _ew; s = _prec; } ->
        cmp ~cst F.gt
      | IsInfinite { e = _ew; s = _prec; } ->
        test ~cst F.is_infinite
      | IsZero { e = _ew; s = _prec; } ->
        test ~cst F.is_zero
      | IsNaN { e = _ew; s = _prec; } ->
        test ~cst F.is_nan
      | IsNegative { e = _ew; s = _prec; } ->
        test ~cst F.is_negative
      | IsPositive { e = _ew; s = _prec; } ->
        test ~cst F.is_positive
      | IsNormal { e = _ew; s = _prec; } ->
        test ~cst F.is_normal
      | IsSubnormal { e = _ew; s = _prec; } ->
        test ~cst F.is_subnormal
      | Rem {e = ew; s = prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun f g ->
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
      | RoundToIntegral {e = ew; s = prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun m f ->
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
      | Min { e = _ew; s = _prec; } -> min_max ~eval env ~cmp:F.lt ~cst
      | Max { e = _ew; s = _prec; } -> min_max ~eval env ~cmp:F.gt ~cst
      | To_ubv { m = size; e = _ew; s = _prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun m f ->
            let f' = fp f in
            let mode = mode m in
            match F.classify f' with
            | (NaN | PInf | NInf) ->
              Fun.corner_case ~eval env cst [] [m; f]
            | (PNormal | NNormal | PSubn | NSubn | PZero | NZero) ->
              let q = F.to_q f' in
              let n = toIntegral mode q in
              if Z.sign n >= 0 && Z.numbits n <= size then
                Bitv.mk size n
              else
                Fun.corner_case ~eval env cst [] [m; f]
          ))
      | To_sbv { m = size; e = _ew; s = _prec; } ->
        Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun m f ->
            let f' = fp f in
            let mode = mode m in
            match F.classify f' with
            | (NaN | PInf | NInf) ->
              Fun.corner_case ~eval env cst [] [m; f]
            | (PNormal | NNormal | PSubn | NSubn | PZero | NZero) ->
              let q = F.to_q f' in
              let n = toIntegral mode q in
              let n' = Z.extract n 0 size in
              if Z.equal n (Z.signed_extract n' 0 size) then
                Bitv.mk size n'
              else
                Fun.corner_case ~eval env cst [] [m; f]
          ))
    end
  | _ -> None
