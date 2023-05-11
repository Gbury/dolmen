
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definition *)
(* ************************************************************************* *)

(* Real are currently represented as algebraic numbers *)

(* Extending Q module from Zarith *)
module Q = struct
    let pp fmt q =
        match Q.classify q with
        | Q.ZERO -> Format.pp_print_char fmt '0'
        | Q.INF -> Format.pp_print_string fmt "+∞"
        | Q.MINF -> Format.pp_print_string fmt "-∞"
        | Q.UNDEF -> Format.pp_print_string fmt "!undef!"
        | Q.NZERO -> Q.pp_print fmt q


  let two = Q.of_int 2
  let is_integer q = Z.equal Z.one q.Q.den

  let is_unsigned_integer size q =
    is_integer q && (Z.sign q.Q.num >= 0) && (Z.numbits q.Q.num <= size)

  let floor x = Q.of_bigint (Z.fdiv x.Q.num x.Q.den)
  let ceil x = Q.of_bigint (Z.cdiv x.Q.num x.Q.den)
  let truncate d = if (Q.sign d > 0) then floor d else ceil d

  let div_e a b =
    let s = Q.sign b in
    let d = Q.div a b in
    if (s > 0) then floor d else ceil d

  let div_t a b = truncate (Q.div a b)
  let div_f a b = floor (Q.div a b)
  let mod_e a b = Q.sub a (Q.mul (div_e a b) b)
  let mod_t a b = Q.sub a (Q.mul (div_t a b) b)
  let mod_f a b = Q.sub a (Q.mul (div_f a b) b)
  let is_zero c = (Q.sign c) = 0
  let pow q n =
    if n < 0 then (
      assert ((Q.sign q <> 0));
      Q.make (Z.pow q.den (-n)) (Z.pow q.num (-n)))
    else Q.make (Z.pow q.num n) (Z.pow q.den n)

    include Q (* Module from Zarith *)
end

module A = struct
  let ctx = Calcium.CTX.mk ()

    module A = Calcium.CA

    type t = Q of Q.t | A of A.t [@@deriving hash]

    let to_a = function Q q -> A.of_q ~ctx q | A a -> a

    let compare a b =
      match (a, b) with
      | Q a, Q b -> Q.compare a b
      | A a, Q b -> A.compare_q ~ctx a b
      | Q a, A b -> -A.compare_q ~ctx b a
      | A a, A b -> A.compare ~ctx a b

    let equal a b =
      match (a, b) with
      | Q a, Q b -> Q.equal a b
      | A a, Q b -> (not (Z.equal b.den Z.one)) && A.compare_q ~ctx a b = 0
      | Q a, A b -> (not (Z.equal a.den Z.one)) && A.compare_q ~ctx b a = 0
      | A a, A b -> A.compare ~ctx a b = 0

    let pp fmt = function Q q -> Q.pp fmt q | A a -> Calcium.CA.pp ~ctx fmt a

  let zero = Q Q.zero
  let one = Q Q.one
  let half = Q (Q.make Z.one (Z.of_int 2))
  let minus_one = Q Q.minus_one
  let two = Q Q.two
  let sign = function Q q -> Q.sign q | A a -> A.sign ~ctx a
  let ge a b = compare a b >= 0
  let gt a b = compare a b > 0
  let le a b = compare a b <= 0
  let lt a b = compare a b < 0
  let min a b = if lt a b then a else b
  let max a b = if gt a b then a else b
  let of_string s = Q (Q.of_string s)
  let to_string = function Q q -> Q.to_string q | A a -> A.to_string ~ctx a
  let is_integer = function Q q -> Q.is_integer q | A _ -> false
  (* by normalization *)

  let is_real = function Q q -> Q.is_real q | A _ -> true
  let to_z = function Q q -> q.Q.num | A _ -> assert false
  let to_int = function Q q -> Q.to_int q | A _ -> assert false

  let to_q = function Q q -> Some q | A _ -> None
  let inf = Q Q.inf
  let minus_inf = Q Q.minus_inf

  let normalize (a : A.t) =
    match A.to_q ~ctx a with None -> A a | Some q -> Q q

  let ( !! ) = normalize

  let is_unsigned_integer size = function
    | Q q -> Q.is_unsigned_integer size q
    | A _ -> false

  let of_q q = Q q
  let of_z z = Q (Q.of_bigint z)
  let of_int z = Q (Q.of_int z)
  let of_bigint = of_z
  let floor = function Q q -> Q (Q.floor q) | A a -> of_z (A.floor ~ctx a)
  let ceil = function Q q -> Q (Q.ceil q) | A a -> of_z (A.ceil ~ctx a) 

  let truncate = function
    | Q q -> Q (Q.truncate q)
    | A a -> of_z (A.truncate ~ctx a)

  let neg = function Q q -> Q (Q.neg q) | A a -> normalize (A.neg ~ctx a)
  let inv = function Q q -> Q (Q.inv q) | A a -> normalize (A.inv ~ctx a)
  let abs = function Q q -> Q (Q.abs q) | A a -> normalize (A.abs ~ctx a)

  let combine2 fq fa cv a b =
    match (a, b) with
    | Q a, Q b -> Q (fq a b)
    | _ ->
        cv (fa ~ctx (to_a a) (to_a b))

  (* todo special case for one, zero, ... *)
  let div = combine2 Q.div A.div normalize
  let add = combine2 Q.add A.add normalize
  let sub = combine2 Q.sub A.sub normalize
  let mul = combine2 Q.mul A.mul normalize
  let ( + ) = add
  let ( - ) = sub
  let ( ~- ) = neg
  let ( ~+ ) x = x
  let ( * ) = mul
  let ( / ) = div
  let div_e = combine2 Q.div_e A.div_e of_z
  let div_t = combine2 Q.div_t A.div_t of_z
  let div_f = combine2 Q.div_f A.div_f of_z
  let mod_e = combine2 Q.mod_e A.mod_e normalize
  let mod_t = combine2 Q.mod_t A.mod_t normalize
  let mod_f = combine2 Q.mod_f A.mod_f normalize
  let is_zero c = match c with Q q -> Q.is_zero q | A _ -> false
  (* by normalization *)
  let round x =
    if lt zero x then ceil (sub x half) else floor (add x half)

  let none_zero c = if is_zero c then None else Some c
  let is_not_zero c = not ((sign c) = 0)

  let pow q n =
    match q with Q q -> Q (Q.pow q n) | A a -> !!(A.pow_int ~ctx a n)

  let positive_root q n =
    if n = 0 then one else !!(A.pow ~ctx (to_a q) (Q.make Z.one (Z.of_int n)))

  let positive_pow q n =
    !!(A.pow ~ctx (to_a q) n)

  let from_order poly order =
    let poly = Flint.FMPZ_poly.create poly in
    let roots = Calcium.QQBAR.from_roots ~unsorted:true poly in
    if Stdlib.Array.length roots <= order then invalid_arg "Not enough roots";
    if Stdlib.Array.exists (fun x -> not (Calcium.QQBAR.is_real x)) roots
    then invalid_arg "Roots must all be reals";
    let roots = Stdlib.Array.map (Calcium.CA.from_qqbar ~ctx) roots in
    let cmp r1 r2 = Calcium.CA.compare ~ctx r1 r2 in
    Stdlib.Array.sort cmp roots;
    !!( (Stdlib.Array.get roots order))

  let from_enclosure poly min max =
    let poly = Flint.FMPZ_poly.create poly in
    let roots = Calcium.QQBAR.from_roots ~unsorted:true poly in
    let roots = Stdlib.Array.map (Calcium.CA.from_qqbar ~ctx) roots in
    let inside r = Calcium.CA.compare_q ~ctx r min >= 0 &&
        Calcium.CA.compare_q ~ctx r max <= 0
    in
    let roots = List.filter inside (Stdlib.Array.to_list roots) in
    match roots with
    | [] -> invalid_arg "No roots found in the enclosure"
    | [r] -> !!r
    | _ -> invalid_arg "The roots are not uniq in the enclosure"
  end



type t = A.t

let compare = A.compare
let print fmt r =
  Format.fprintf fmt "%a" A.pp r

let ops = Value.ops ~compare ~print ()

(* Value helpers *)
(* ************************************************************************* *)
(*
let ceil x = Q.of_bigint @@ Int.ceil x
let floor x = Q.of_bigint @@ Int.floor x
let truncate x = Q.of_bigint @@ Int.truncate x

let half = { Q.num = Z.one; Q.den = Z.of_int 2 }

let round x =
  if Q.lt Q.zero x then ceil (Q.sub x half) else floor (Q.add x half)

let div_e a b =
  let s = Q.sign b in
  let d = Q.div a b in
  if s > 0 then floor d else ceil d

let div_t a b = truncate (Q.div a b)
let div_f a b = floor (Q.div a b)
let mod_e a b = Q.sub a (Q.mul (div_e a b) b)
let mod_t a b = Q.sub a (Q.mul (div_t a b) b)
let mod_f a b = Q.sub a (Q.mul (div_f a b) b)
*)

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
      if A.equal A.zero v_y then
        Fun.corner_case ~eval env cst [] [x; y]
      else mk @@ f v_x v_y
    ))

let builtins ~eval env (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Decimal i -> Some (mk (A.of_string i))
  | B.Algebraic (Ordered_root {coeffs; order}) ->
    let coeffs = Stdlib.Array.of_list (List.map Z.of_string coeffs) in
    let order = int_of_string order in
    Some (mk (A.from_order coeffs order))
  | B.Algebraic (Enclosed_root {coeffs; min; max}) ->
      let coeffs = Stdlib.Array.of_list (List.map Z.of_string coeffs) in
      let q_of_pair (num,den) = Q.div (Q.of_string num) (Q.of_string den) in
      let min = q_of_pair min in
      let max = q_of_pair max in
      Some (mk (A.from_enclosure coeffs min max))
    | B.Lt `Real -> cmp ~cst A.lt
  | B.Gt `Real -> cmp ~cst A.gt
  | B.Geq `Real -> cmp ~cst A.ge
  | B.Leq `Real -> cmp ~cst A.le
  | B.Minus `Real -> op1 ~cst (fun x -> A.neg x)
  | B.Add `Real -> op2 ~cst A.add
  | B.Sub `Real -> op2 ~cst A.sub
  | B.Mul `Real -> op2 ~cst A.mul
  | B.Div `Real -> op2_zero A.div ~cst ~env ~eval
  | B.Div_e `Real -> op2 ~cst A.div_e
  | B.Div_t `Real -> op2 ~cst A.div_t
  | B.Div_f `Real -> op2 ~cst A.div_f
  | B.Modulo_e `Real -> op2 ~cst A.mod_e
  | B.Modulo_t `Real -> op2 ~cst A.mod_t
  | B.Modulo_f `Real -> op2 ~cst A.mod_f
  | B.Is_rat `Real -> Some (Bool.mk true)
  | B.Floor `Real -> op1 ~cst A.floor
  | B.Ceiling `Real -> op1 ~cst A.ceil
  | B.Truncate `Real -> op1 ~cst A.truncate
  | B.Round `Real -> op1 ~cst A.round
  | B.Is_int `Real ->
    Some (fun1 ~cst (fun x -> Bool.mk (A.is_integer x)))
  | _ -> None

