
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

let ubitv n t =
  let t = Value.extract_exn ~ops t in
  (* the typing of expressions should guarantee that this never happens *)
  if not (is_unsigned_integer n t) then
    (invalid_arg
      (Format.asprintf "%a is not an unsigned integer of size %i"
         Z.pp_print t n));
  t

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

let sbitv n t = Z.signed_extract (ubitv n t) 0 n
let extract n t = Z.extract t 0 n

let builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Bitvec s ->
    Some (mk (String.length s) (Z.of_string_base 2 s))
  | B.Bitv_concat { n; m } ->
    op2 ~cst ~size:(n + m) (concat n m)
  | B.Bitv_extract { n; i; j } ->
    op1 ~cst ~size:(i - j + 1) (fun a -> Z.extract (ubitv n a) j (i - j + 1))
  | B.Bitv_repeat { n; k } ->
    op1 ~cst ~size:(n * k) (fun a -> repeat n k (ubitv n a))
  | B.Bitv_zero_extend { n; k } ->
    op1 ~cst ~size:(n + k) (ubitv n)
  | B.Bitv_sign_extend { n; k } ->
    op1 ~cst ~size:(n + k) (fun a -> extract (n + k) (sbitv n a))
  | B.Bitv_rotate_left { n; i } ->
    op1 ~cst ~size:n (fun a -> rotate_left n i (ubitv n a))
  | B.Bitv_rotate_right { n; i } ->
    op1 ~cst ~size:n (fun a -> rotate_right n i (ubitv n a))
  | B.Bitv_not n ->
    op1 ~cst ~size:n (fun a -> extract n (Z.lognot (ubitv n a)))
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
        if Z.equal (ubitv n a) (ubitv n b)
        then extract 1 Z.minus_one
        else from_bitv 1 Z.zero)
  | B.Bitv_neg n ->
    op1 ~cst ~size:n (fun a ->
        extract n (Z.sub (Z.shift_left Z.one n) (ubitv n a)))
  | B.Bitv_add n ->
    op2 ~cst ~size:n (fun a b -> extract n (Z.add (ubitv n a) (ubitv n b)))
  | B.Bitv_sub n ->
    op2 ~cst ~size:n (fun a b -> extract n (Z.sub (ubitv n a) (ubitv n b)))
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
        let a = sbitv n a in
        if Z.equal b Z.zero then
          if Z.sign a >= 0 then extract n Z.minus_one else extract n Z.one
        else extract n (Z.div a b))
  | B.Bitv_srem n ->
    op2 ~cst ~size:n (fun a b ->
        let b = sbitv n b in
        if Z.equal b Z.zero then from_bitv n (ubitv n a)
        else extract n (Z.rem (sbitv n a) b))
  | B.Bitv_smod n ->
    op2 ~cst ~size:n (fun a b ->
        let b = sbitv n b in
        if Z.equal b Z.zero then from_bitv n (ubitv n a)
        else begin
          let a = sbitv n a in
          extract n (Z.sub a (Z.mul (Z.fdiv a b) b))
        end)
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

let bv2nat ~cst ~size =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> Int.mk (ubitv size x)))

let int2bv ~cst ~size =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x ->
    mk size (Z.extract (Value.extract_exn ~ops:Int.ops x) 0 size)))

let bvconv_builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Bitv_of_int { n } -> int2bv ~cst ~size:n
  | B.Bitv_to_nat { n } -> bv2nat ~cst ~size:n
  | _ -> None
