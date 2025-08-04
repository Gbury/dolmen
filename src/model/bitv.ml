
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

(** Bitvectors are represented by an unsigned unbounded integer. *)
type t = Z.t

let compare = Z.compare

let print_bitpattern fmt t =
  let rec aux fmt t n =
    if n < 0 then ()
    else begin
      Format.fprintf fmt "%d" (if Z.testbit t n then 1 else 0);
      aux fmt t (n - 1)
    end
  in
  aux fmt t (Z.numbits t - 1)

let print fmt t =
  Format.fprintf fmt "%a / #%a"
    Z.pp_print t print_bitpattern t

let ops = Value.ops ~print ~compare ()


(* Value helpers *)
(* ************************************************************************* *)

let is_unsigned_integer size z =
  Z.sign z >= 0 && Z.numbits z <= size

let is_signed_integer size z =
  Z.equal z (Z.signed_extract z 0 size)

let ubitv n t =
  let t = Value.extract_exn ~ops t in
  (* the typing of expressions should guarantee that this never happens *)
  if not (is_unsigned_integer n t) then
    (invalid_arg
      (Format.asprintf "%a is not an unsigned integer of size %i"
         Z.pp_print t n));
  t

let sbitv n t = Z.signed_extract (ubitv n t) 0 n

let from_bitv n t =
  (* TODO: proper error *)
  if not (is_unsigned_integer n t) then (
    (* Format.eprintf "@[[BV] %s(%a) is not of size %i@]@."
      (Z.format (Printf.sprintf "%%0+#%ib" n) t)
      Z.pp_print t n; *)
    assert false (* Internal error *)
  );
  t

let extract n t = Z.extract t 0 n

let concat n m a b =
  Z.logor (Z.shift_left (ubitv n a) m) (ubitv m b)

let repeat n k a =
  let rec loop n k acc z =
    if k = 0 then acc
    else
      let acc = Z.logor (Z.shift_left acc n) z in
      loop n (k - 1) acc z
  in
  loop n k Z.zero a

let rotate_left n i a =
  let k = i mod n in
  extract n (Z.logor (Z.shift_left a k) (Z.extract a (n - k) k))

let rotate_right n i a =
  let k = i mod n in
  extract n (Z.logor (Z.shift_left a (n - k)) (Z.shift_right a k))



(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let mk n i = Value.mk ~ops (from_bitv n i)

let cmp ~cst p =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y -> Bool.mk @@ p x y))
let op2 ~cst ~size f =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y -> mk size @@ f x y))
let op1 ~cst ~size f =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> mk size @@ f x))

let bitv ~signed n t = if signed then sbitv n t else ubitv n t

let bv2int ~cst ~signed ~size =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> Int.mk (bitv ~signed size x)))

let int2bv ~cst ~size =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x ->
      mk size (Z.extract (Value.extract_exn ~ops:Int.ops x) 0 size)))

let neg_overflow ~cst ~size =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x ->
      let x = sbitv size x in
      let z = Z.neg x in
      let b = is_signed_integer size z in
      Bool.mk (not b)
    ))

let binary_overflow ~cst ~signed ~size op =
  Some (Fun.mk_clos @@ Fun.fun_2 ~cst (fun x y ->
      let x = bitv ~signed size x in
      let y = bitv ~signed size y in
      let z = op x y in
      let b =
        if signed
        then is_signed_integer size z
        else is_unsigned_integer size z
      in
      Bool.mk (not b)
    ))

let builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  let extract n t = Z.extract t 0 n in
  match cst.builtin with
  | Dolmen.Std.Builtin.Bitv blt ->
    begin match blt with
      | T _ -> assert false (* Types are not evaluated *)
      | Binary_lit s ->
        Some (mk (String.length s) (Z.of_string_base 2 s))
      | Of_int { n } -> int2bv ~cst ~size:n
      | To_int { n; signed } -> bv2int ~cst ~signed ~size:n
      | Concat { n; m } ->
        op2 ~cst ~size:(n + m) (concat n m)
      | Extract { n; i; j } ->
        op1 ~cst ~size:(i - j + 1) (fun a -> Z.extract (ubitv n a) j (i - j + 1))
      | Repeat { n; k } ->
        op1 ~cst ~size:(n * k) (fun a -> repeat n k (ubitv n a))
      | Zero_extend { n; k } ->
        op1 ~cst ~size:(n + k) (ubitv n)
      | Sign_extend { n; k } ->
        op1 ~cst ~size:(n + k) (fun a -> extract (n + k) (sbitv n a))
      | Rotate_left { n; i } ->
        op1 ~cst ~size:n (fun a -> rotate_left n i (ubitv n a))
      | Rotate_right { n; i } ->
        op1 ~cst ~size:n (fun a -> rotate_right n i (ubitv n a))
      | Not {n} ->
        op1 ~cst ~size:n (fun a -> extract n (Z.lognot (ubitv n a)))
      | And {n} ->
        op2 ~cst ~size:n (fun a b -> Z.logand (ubitv n a) (ubitv n b))
      | Or {n} ->
        op2 ~cst ~size:n (fun a b ->
            from_bitv n (Z.logor (ubitv n a) (ubitv n b)))
      | Nand {n} ->
        op2 ~cst ~size:n (fun a b ->
            extract n (Z.lognot (Z.logand (ubitv n a) (ubitv n b))))
      | Nor {n} ->
        op2 ~cst ~size:n (fun a b ->
            extract n (Z.lognot (Z.logor (ubitv n a) (ubitv n b))))
      | Xor {n} ->
        op2 ~cst ~size:n (fun a b -> extract n (Z.logxor (ubitv n a) (ubitv n b)))
      | Xnor {n} ->
        op2 ~cst ~size:n (fun a b ->
            extract n (Z.logxor (ubitv n a) (Z.lognot (ubitv n b))))
      | Comp {n} ->
        op2 ~cst ~size:n (fun a b ->
            if Z.equal (ubitv n a) (ubitv n b)
            then extract 1 Z.minus_one
            else from_bitv 1 Z.zero)
      | Neg {n} ->
        op1 ~cst ~size:n (fun a ->
            extract n (Z.sub (Z.shift_left Z.one n) (ubitv n a)))
      | Add {n} ->
        op2 ~cst ~size:n (fun a b -> extract n (Z.add (ubitv n a) (ubitv n b)))
      | Sub {n} ->
        op2 ~cst ~size:n (fun a b -> extract n (Z.sub (ubitv n a) (ubitv n b)))
      | Mul {n} ->
        op2 ~cst ~size:n (fun a b -> extract n (Z.mul (ubitv n a) (ubitv n b)))
      | Udiv {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = ubitv n b in
            if Z.equal b Z.zero then extract n Z.minus_one
            else extract n (Z.div (ubitv n a) b))
      | Urem {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = ubitv n b in
            if Z.equal b Z.zero then from_bitv n (ubitv n a)
            else extract n (Z.rem (ubitv n a) b))
      | Sdiv {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = sbitv n b in
            let a = sbitv n a in
            if Z.equal b Z.zero then
              if Z.sign a >= 0 then extract n Z.minus_one else extract n Z.one
            else extract n (Z.div a b))
      | Srem {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = sbitv n b in
            if Z.equal b Z.zero then from_bitv n (ubitv n a)
            else extract n (Z.rem (sbitv n a) b))
      | Smod {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = sbitv n b in
            if Z.equal b Z.zero then from_bitv n (ubitv n a)
            else begin
              let a = sbitv n a in
              extract n (Z.sub a (Z.mul (Z.fdiv a b) b))
            end)
      | Shl {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = ubitv n b in
            if Z.leq (Z.of_int n) b then from_bitv n Z.zero
            else extract n (Z.shift_left (ubitv n a) (Z.to_int b)))
      | Lshr {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = ubitv n b in
            if Z.leq (Z.of_int n) b then from_bitv n Z.zero
            else extract n (Z.shift_right (ubitv n a) (Z.to_int b)))
      | Ashr {n} ->
        op2 ~cst ~size:n (fun a b ->
            let b = ubitv n b in
            let b = if Z.leq (Z.of_int n) b then n else Z.to_int b in
            extract n (Z.shift_right (sbitv n a) b))
      | Ult {n} -> cmp ~cst (fun a b -> Z.lt (ubitv n a) (ubitv n b))
      | Ule {n} -> cmp ~cst (fun a b -> Z.leq (ubitv n a) (ubitv n b))
      | Ugt {n} -> cmp ~cst (fun a b -> Z.gt (ubitv n a) (ubitv n b))
      | Uge {n} -> cmp ~cst (fun a b -> Z.geq (ubitv n a) (ubitv n b))
      | Slt {n} -> cmp ~cst (fun a b -> Z.lt (sbitv n a) (sbitv n b))
      | Sle {n} -> cmp ~cst (fun a b -> Z.leq (sbitv n a) (sbitv n b))
      | Sgt {n} -> cmp ~cst (fun a b -> Z.gt (sbitv n a) (sbitv n b))
      | Sge {n} -> cmp ~cst (fun a b -> Z.geq (sbitv n a) (sbitv n b))
      | Overflow_neg { n; } -> neg_overflow ~cst ~size:n
      | Overflow_add { n; signed; } -> binary_overflow ~cst ~size:n ~signed Z.add
      | Overflow_sub { n; signed; } -> binary_overflow ~cst ~size:n ~signed Z.sub
      | Overflow_mul { n; signed; } -> binary_overflow ~cst ~size:n ~signed Z.mul
      | Overflow_div { n; } -> binary_overflow ~cst ~size:n ~signed:true Z.div
    end
  | _ -> None

