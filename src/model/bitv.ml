(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

let ops =
  Value.ops
    ~compare:(fun b b' -> Stdlib.compare b b')
    ~print:(fun fmt b -> Format.fprintf fmt "%a" Z.pp_print b)
    ()

(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let is_unsigned_integer size z = Z.sign z >= 0 && Z.numbits z <= size

let from_bitv n t =
  if not (is_unsigned_integer n t) then
    Format.eprintf "@[[BV] %s(%a) is not of size %i@]@."
      (Z.format (Printf.sprintf "%%0+#%ib" n) t)
      Z.pp_print t n;
  assert (is_unsigned_integer n t);
  t

let mk n i = Value.mk ~ops (from_bitv n i)
let fun1 f ~cst = Fun.fun_1 ~cst (fun x -> f (Value.extract_exn ~ops x))

let fun2 f ~cst =
  Fun.fun_2 ~cst (fun x y ->
      f (Value.extract_exn ~ops x) (Value.extract_exn ~ops y))

let cmp ~cst p = Some (fun2 ~cst (fun x y -> Bool.mk @@ p x y))
let op2 ~cst ~size f = Some (fun2 ~cst (fun x y -> mk size @@ f x y))
let op1 ~cst ~size f = Some (fun1 ~cst (fun x -> mk size @@ f x))

let ubitv n t =
  if not (is_unsigned_integer n t) then invalid_arg "not unsigned integer";
  t

let sbitv n t = Z.signed_extract (ubitv n t) 0 n
let extract n t = Z.extract t 0 n

let builtins (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Bitvec s -> Some (mk (String.length s) (Z.of_string_base 2 s))
  | B.Bitv_concat { n; m } ->
      op2 ~cst ~size:(n + m) (fun a b ->
          Z.logor (Z.shift_left (ubitv n a) m) (ubitv m b))
  | B.Bitv_extract { n; i; j } ->
      op1 ~cst ~size:(i - j + 1) (fun a -> Z.extract (ubitv n a) j (i - j + 1))
  | B.Bitv_repeat { n; k } ->
      let rec loop n k acc z =
        if k = 0 then acc
        else
          let acc = Z.logor (Z.shift_left acc n) z in
          loop n (k - 1) acc z
      in
      op1 ~cst ~size:(n * k) (fun a ->
          let a = ubitv n a in
          loop n k Z.zero a)
  | B.Bitv_zero_extend { n; k } -> op1 ~cst ~size:(n + k) (ubitv n)
  | B.Bitv_sign_extend { n; k } ->
      op1 ~cst ~size:(n + k) (fun a -> extract (n + k) (sbitv n a))
  | B.Bitv_rotate_left { n; i } ->
      op1 ~cst ~size:n (fun a ->
          let a = ubitv n a in
          let k = i mod n in
          extract n (Z.logor (Z.shift_left a k) (Z.extract a (n - k) k)))
  | B.Bitv_rotate_right { n; i } ->
      op1 ~cst ~size:n (fun a ->
          let a = ubitv n a in
          let k = i mod n in
          extract n (Z.logor (Z.shift_left a (n - k)) (Z.shift_right a k)))
  | B.Bitv_not n -> op1 ~cst ~size:n (fun a -> extract n (Z.lognot (ubitv n a)))
  | B.Bitv_and n ->
      op2 ~cst ~size:n (fun a b -> Z.logand (ubitv n a) (ubitv n b))
  | B.Bitv_or n ->
      op2 ~cst ~size:n (fun a b ->
          from_bitv n (Z.logor (ubitv n a) (ubitv n b)))
  | B.Bitv_nand n ->
      op2 ~cst ~size:n (fun a b ->
          extract n (Z.lognot (Z.logand (ubitv n a) (ubitv n b))))
  | B.Bitv_nor n ->
      op2 ~cst ~size:n (fun a b ->
          extract n (Z.lognot (Z.logor (ubitv n a) (ubitv n b))))
  | B.Bitv_xor n ->
      op2 ~cst ~size:n (fun a b -> extract n (Z.logxor (ubitv n a) (ubitv n b)))
  | B.Bitv_xnor n ->
      op2 ~cst ~size:n (fun a b ->
          extract n (Z.logxor (ubitv n a) (Z.lognot (ubitv n b))))
  | B.Bitv_comp n ->
      op2 ~cst ~size:n (fun a b ->
          if Z.equal (ubitv n a) (ubitv n b) then extract 1 Z.minus_one
          else from_bitv 1 Z.zero)
  | B.Bitv_neg n ->
      op1 ~cst ~size:n (fun a -> Z.sub (Z.shift_left Z.one n) (ubitv n a))
  | B.Bitv_add n ->
      op2 ~cst ~size:n (fun a b -> extract n (Z.add (ubitv n a) (ubitv n b)))
  | B.Bitv_mul n ->
      op2 ~cst ~size:n (fun a b -> extract n (Z.mul (ubitv n a) (ubitv n b)))
  | B.Bitv_udiv n ->
      op2 ~cst ~size:n (fun a b ->
          let b = ubitv n b in
          if Z.equal b Z.zero then extract n Z.minus_one
          else extract n (Z.div (ubitv n a) b))
  | B.Bitv_urem n ->
      op2 ~cst ~size:n (fun a b ->
          let b = ubitv n b in
          if Z.equal b Z.zero then from_bitv n (ubitv n a)
          else extract n (Z.rem (ubitv n a) b))
  | B.Bitv_sdiv n ->
      op2 ~cst ~size:n (fun a b ->
          let b = sbitv n b in
          if Z.equal b Z.zero then extract n Z.one
          else extract n (Z.div (sbitv n a) b))
  | B.Bitv_srem n ->
      op2 ~cst ~size:n (fun a b ->
          let b = sbitv n b in
          if Z.equal b Z.zero then from_bitv n (ubitv n a)
          else extract n (Z.rem (sbitv n a) b))
  | B.Bitv_smod n ->
      op2 ~cst ~size:n (fun a b ->
          let b = sbitv n b in
          let a = sbitv n a in
          if Z.equal b Z.zero then assert false (* TODO *)
          else extract n (Z.sub a (Z.mul (Z.fdiv a b) b)))
  | B.Bitv_shl n ->
      op2 ~cst ~size:n (fun a b ->
          let b = ubitv n b in
          if Z.leq (Z.of_int n) b then from_bitv n Z.zero
          else extract n (Z.shift_left (ubitv n a) (Z.to_int b)))
  | B.Bitv_lshr n ->
      op2 ~cst ~size:n (fun a b ->
          let b = ubitv n b in
          if Z.leq (Z.of_int n) b then from_bitv n Z.zero
          else extract n (Z.shift_right (ubitv n a) (Z.to_int b)))
  | B.Bitv_ashr n ->
      op2 ~cst ~size:n (fun a b ->
          let b = ubitv n b in
          let b = if Z.leq (Z.of_int n) b then n else Z.to_int b in
          extract n (Z.shift_right (sbitv n a) b))
  | B.Bitv_ult n -> cmp ~cst (fun a b -> Z.lt (ubitv n a) (ubitv n b))
  | B.Bitv_ule n -> cmp ~cst (fun a b -> Z.leq (ubitv n a) (ubitv n b))
  | B.Bitv_ugt n -> cmp ~cst (fun a b -> Z.gt (ubitv n a) (ubitv n b))
  | B.Bitv_uge n -> cmp ~cst (fun a b -> Z.geq (ubitv n a) (ubitv n b))
  | B.Bitv_slt n -> cmp ~cst (fun a b -> Z.lt (sbitv n a) (sbitv n b))
  | B.Bitv_sle n -> cmp ~cst (fun a b -> Z.leq (sbitv n a) (sbitv n b))
  | B.Bitv_sgt n -> cmp ~cst (fun a b -> Z.gt (sbitv n a) (sbitv n b))
  | B.Bitv_sge n -> cmp ~cst (fun a b -> Z.geq (sbitv n a) (sbitv n b))
  | _ -> None
