
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* String values are [Ustr.t], regex (RegLan) values are [Reglan.t]. *)

module S = Smt_string.Ustr
module R = Smt_string.Reglan
module C = Smt_string.Codepoint

(* String operations *)
(* ************************************************************************* *)

module Str = struct

  let empty = S.empty

  (* [str.<] and [str.<=] numeric code-point lexicographic order. *)
  let lt a b = S.compare a b < 0
  let leq a b = S.compare a b <= 0

  (* [str.at s i] returns the singleton string containing the element at [i],
     or the empty string if [i] is out of range. *)
  let at s i =
    match S.get_opt s i with Some c -> S.of_codepoint c | None -> empty

  (* [str.substr s i n] longest substring from [i] of length at most [n],
     returns the empty string unless [0 <= i < length s] and [n > 0]. *)
  let substr s i n =
    let len = S.length s in
    if i < 0 || i >= len || n <= 0 then empty
    else S.sub s i (if n < len - i then n else len - i)

  (* [str.contains s sub] does [sub] occur in [s]? ([true] if [sub] is
     empty). *)
  let contains s sub =
    match S.find_opt s sub 0 with None -> false | _ -> true

  (* [str.prefixof p s] is [p] a prefix of [s]? *)
  let prefixof p s =
    S.length p <= S.length s && S.equal p (S.sub s 0 (S.length p))

  (* [str.suffixof q s] is [q] a suffix of [s]? *)
  let suffixof q s =
    let lq = S.length q in
    let ls = S.length s in
    lq <= ls && S.equal q (S.sub s (ls - lq) lq)

  (* [str.indexof s sub i] smallest position [>= i] of [sub] in [s], or [-1]
     (also when [i < 0] or [i > length s]). Empty [sub] matches at [i]. *)
  let indexof s sub i =
    match S.find_opt s sub i with Some j -> j | None -> -1

  (* [str.replace s sub by] replace the first occurrence of [sub] with [by] in
     [s], returns [s] if [sub] is not found, and if [sub] is empty returns
     [by ++ s]. *)
  let replace s sub by =
    match S.find_opt s sub 0 with
    | None -> s
    | Some j ->
      let k = j + S.length sub in
      S.concat_list [ S.sub s 0 j; by; S.sub s k (S.length s - k) ]

  (* [str.replace_all s sub by] replace every non-overlapping occurrence, from
     left to right, of [sub] with [by] in [s], returns [s] if [sub] is
     empty. *)
  let replace_all s sub by =
    let lsub = S.length sub and ls = S.length s in
    if lsub = 0 then s
    else begin
      let rec aux i acc =
        match S.find_opt s sub i with
        | None -> List.rev (S.sub s i (ls - i) :: acc)
        | Some j -> aux (j + lsub) (by :: S.sub s i (j - i) :: acc)
      in
      S.concat_list (aux 0 [])
    end

  let is_digit_cp c =
    let c = C.to_int c in
    c >= 0x30 && c <= 0x39

  (* [str.is_digit s] is [s] a single ASCII digit? *)
  let is_digit s = S.length s = 1 && is_digit_cp (S.get s 0)

  (* [str.to_code s] returns the code point of a singleton string [s], [-1]
     otherwise. *)
  let to_code s = if S.length s = 1 then C.to_int (S.get s 0) else -1

  (* [str.from_code n] singleton for a valid [n], else empty. *)
  let from_code n =
    match C.of_int n with Some c -> S.of_codepoint c | None -> empty

  (* [str.to_int s] returns the integer value of a non-empty all-digit string
     [s], [-1] otherwise. *)
  let to_int s =
    let len = S.length s in
    let rec aux acc i =
      if i = len then acc
      else begin
        let c = S.get s i in
        if is_digit_cp c then
          aux (Z.add (Z.mul acc (Z.of_int 10)) (Z.of_int (C.to_int c - 0x30)))
            (i + 1)
        else Z.minus_one
      end
    in
    if len = 0 then Z.minus_one else aux Z.zero 0

  (* [str.from_int n] returns the string corresponding to the decimal
     representation of the integer [n >= 0], otherwise returns the empty
     string. *)
  let from_int n =
    if Z.sign n < 0 then empty
    else begin
      let d = Z.to_string n in
      S.of_code_points_exn
        (Stdlib.Array.init (String.length d) (fun i -> Char.code d.[i]))
    end

  (* The SMT-LIB quoted form of [s]: printable ASCII is kept as is (a double
     quote inside [s] is written twice), and every other character is written
     as a [\u] hex escape. A backslash is also escaped, since [\u] always starts
     an escape. *)
  let to_literal s =
    let buf = Buffer.create (S.length s + 2) in
    Buffer.add_char buf '"';
    Stdlib.Array.iter (fun cp ->
        if cp = 0x22 then Buffer.add_string buf "\"\""
        else if cp = 0x5C then Buffer.add_string buf "\\u{5c}"
        else if cp >= 0x20 && cp <= 0x7E then Buffer.add_char buf (Char.chr cp)
        else Buffer.add_string buf (Printf.sprintf "\\u{%x}" cp)
      ) (S.to_code_points s);
    Buffer.add_char buf '"';
    Buffer.contents buf

  (* Prints using [to_literal]. *)
  let print fmt s = Format.pp_print_string fmt (to_literal s)

end

(* Regex operations *)
(* ************************************************************************* *)

module Re = struct

  (* [re.all] all strings. *)
  let all = R.star R.any_char

  (* [re.diff] strings in [a] but not [b]. *)
  let diff a b = R.inter a (R.comp b)

  (* [re.+], i.e. [r ++ r*]. *)
  let plus r = R.concat r (R.star r)

  (* [re.opt], i.e. [r | ""]. *)
  let opt r = R.union r R.epsilon

  (* [(_ re.^ n) r] [n] repetitions ([{""}] if [n <= 0]). *)
  let rec power n r =
    if n <= 0 then R.epsilon else R.concat r (power (n - 1) r)

  (* [(_ re.loop n1 n2) r] [n1] to [n2] repetitions, empty if [n1 > n2]. *)
  let loop n1 n2 r =
    if n1 > n2 || n2 < 0 then R.empty
    else begin
      let n1 = if n1 < 0 then 0 else n1 in
      R.concat (power n1 r) (power (n2 - n1) (opt r))
    end

  (* [str.replace_re s r by] replace the first, shortest match of [r] in [s]
     with [by], prepends [by] if [r] accepts [""], and [s] is returned
     unchanged if nothing matches. *)
  let replace_re s r by =
    match R.find_opt r s 0 with
    | None -> s
    | Some (i, n) ->
      let k = i + n in
      S.concat_list [ S.sub s 0 i; by; S.sub s k (S.length s - k) ]

  (* [str.replace_re_all s r by] replace every non-overlapping, shortest,
     non-empty match, left to right (empty matches skipped). *)
  let replace_re_all s r by =
    let ls = S.length s in
    let rec aux i acc =
      match R.find_opt ~nonempty:true r s i with
      | None -> List.rev (S.sub s i (ls - i) :: acc)
      | Some (j, n) -> aux (j + n) (by :: S.sub s i (j - i) :: acc)
    in
    S.concat_list (aux 0 [])

  let char_lit c = Str.to_literal (S.of_codepoint c)

  (* Prints in SMT-LIB syntax. *)
  let rec print fmt (r : R.t) =
    match r with
    | Empty -> Format.pp_print_string fmt "re.none"
    | Eps -> Format.fprintf fmt "(str.to_re \"\")"
    | Chars (lo, hi) when C.equal lo hi ->
      Format.fprintf fmt "(str.to_re %s)" (char_lit lo)
    | Chars _ when R.equal r R.any_char ->
      Format.pp_print_string fmt "re.allchar"
    | Chars (lo, hi) ->
      Format.fprintf fmt "(re.range %s %s)" (char_lit lo) (char_lit hi)
    | Lit s -> Format.fprintf fmt "(str.to_re %s)" (Str.to_literal s)
    | Concat (a, b) -> Format.fprintf fmt "(re.++ %a %a)" print a print b
    | Union (a, b) -> Format.fprintf fmt "(re.union %a %a)" print a print b
    | Inter (a, b) -> Format.fprintf fmt "(re.inter %a %a)" print a print b
    | Star a -> Format.fprintf fmt "(re.* %a)" print a
    | Comp a -> Format.fprintf fmt "(re.comp %a)" print a

end

(* Value definitions *)
(* ************************************************************************* *)

let str_ops = Value.ops ~compare:S.compare ~print:Str.print ()

(* [R.compare] is syntactic equality, not language equivalence. *)
let regl_ops = Value.ops ~compare:R.compare ~print:Re.print ()


(* Value helpers *)
(* ************************************************************************* *)

let ustr t = Value.extract_exn ~ops:str_ops t
let reglan t = Value.extract_exn ~ops:regl_ops t

let mk_str s = Value.mk ~ops:str_ops s
let mk_regl r = Value.mk ~ops:regl_ops r

let ustr_of_raw (raw : string) : S.t =
  let rev_cps =
    Uutf.String.fold_utf_8 (fun acc _pos dec ->
        match dec with
        | `Uchar u -> Uchar.to_int u :: acc
        | `Malformed _ ->
          invalid_arg "Dolmen_model.Strings: literal is not valid UTF-8"
      ) [] raw
  in
  match S.of_code_points (Stdlib.Array.of_list (List.rev rev_cps)) with
  | Some s -> s
  | None ->
    invalid_arg "Dolmen_model.Strings: literal outside the SMT-LIB alphabet"

let int_of_value t =
  let z = Value.extract_exn ~ops:Int.ops t in
  if Z.fits_int z then Z.to_int z
  else if Z.sign z < 0 then min_int else max_int
(* Strings longer than [max_int] are not representable in the smt-string
   library (which uses arrays for strings), and solvers are not likely to
   produce models with such big strings.
   So integers that do not fit in ocaml [int]s are replaced with [min_int] or
   [max_int], which is fine since the theory totalizes¹ access operations
   outside the bounds of the strings.

   [1]: see https://smt-lib.org/theories-UnicodeStrings.shtml and
   tests/model/string/totalization.smt2 *)

let z_of_value t = Value.extract_exn ~ops:Int.ops t

let codepoint_of_singleton t =
  let s = ustr t in
  if S.length s = 1 then Some (S.get s 0) else None


(* Builtins *)
(* ************************************************************************* *)

module B = Dolmen.Std.Builtin

let op1 ~cst f = Some (Fun.mk_clos @@ Fun.fun_1 ~cst f)
let op2 ~cst f = Some (Fun.mk_clos @@ Fun.fun_2 ~cst f)
let op3 ~cst f = Some (Fun.mk_clos @@ Fun.fun_3 ~cst f)

let builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Str s ->
    begin match s with
      | B.Str.T -> assert false (* not a value *)
      | B.Str.Raw str -> Some (mk_str (ustr_of_raw str))
      | B.Str.Length ->
        op1 ~cst (fun s -> Int.mk (Z.of_int (S.length (ustr s))))
      | B.Str.At ->
        op2 ~cst (fun s i -> mk_str (Str.at (ustr s) (int_of_value i)))
      | B.Str.To_code ->
        op1 ~cst (fun s -> Int.mk (Z.of_int (Str.to_code (ustr s))))
      | B.Str.Of_code ->
        op1 ~cst (fun n -> mk_str (Str.from_code (int_of_value n)))
      | B.Str.Is_digit ->
        op1 ~cst (fun s -> Bool.mk (Str.is_digit (ustr s)))
      | B.Str.To_int ->
        op1 ~cst (fun s -> Int.mk (Str.to_int (ustr s)))
      | B.Str.Of_int ->
        op1 ~cst (fun n -> mk_str (Str.from_int (z_of_value n)))
      | B.Str.Concat ->
        op2 ~cst (fun a b -> mk_str (S.concat (ustr a) (ustr b)))
      | B.Str.Sub ->
        op3 ~cst (fun s i n ->
            mk_str (Str.substr (ustr s) (int_of_value i) (int_of_value n)))
      | B.Str.Index_of ->
        op3 ~cst (fun s sub i ->
            Int.mk (Z.of_int (Str.indexof (ustr s) (ustr sub) (int_of_value i))))
      | B.Str.Replace ->
        op3 ~cst (fun s sub by ->
            mk_str (Str.replace (ustr s) (ustr sub) (ustr by)))
      | B.Str.Replace_all ->
        op3 ~cst (fun s sub by ->
            mk_str (Str.replace_all (ustr s) (ustr sub) (ustr by)))
      | B.Str.Replace_re ->
        op3 ~cst (fun s r by ->
            mk_str (Re.replace_re (ustr s) (reglan r) (ustr by)))
      | B.Str.Replace_re_all ->
        op3 ~cst (fun s r by ->
            mk_str (Re.replace_re_all (ustr s) (reglan r) (ustr by)))
      | B.Str.Is_prefix ->
        op2 ~cst (fun p s -> Bool.mk (Str.prefixof (ustr p) (ustr s)))
      | B.Str.Is_suffix ->
        op2 ~cst (fun q s -> Bool.mk (Str.suffixof (ustr q) (ustr s)))
      | B.Str.Contains ->
        op2 ~cst (fun s sub -> Bool.mk (Str.contains (ustr s) (ustr sub)))
      | B.Str.Lexicographic_strict ->
        op2 ~cst (fun a b -> Bool.mk (Str.lt (ustr a) (ustr b)))
      | B.Str.Lexicographic_large ->
        op2 ~cst (fun a b -> Bool.mk (Str.leq (ustr a) (ustr b)))
      | B.Str.In_re ->
        op2 ~cst (fun s r -> Bool.mk (R.matches (reglan r) (ustr s)))
    end
  | B.Regexp r ->
    begin match r with
      | B.Str.RegLan.T -> assert false (* not a value *)
      | B.Str.RegLan.Empty -> Some (mk_regl R.empty)
      | B.Str.RegLan.All -> Some (mk_regl Re.all)
      | B.Str.RegLan.Allchar -> Some (mk_regl R.any_char)
      | B.Str.RegLan.Of_string ->
        op1 ~cst (fun s -> mk_regl (R.of_ustr (ustr s)))
      | B.Str.RegLan.Range ->
        op2 ~cst (fun lo hi ->
            match codepoint_of_singleton lo, codepoint_of_singleton hi with
            | Some lo, Some hi -> mk_regl (R.range lo hi)
            | _ -> mk_regl R.empty (* non-singleton bound: empty language *))
      | B.Str.RegLan.Concat ->
        op2 ~cst (fun a b -> mk_regl (R.concat (reglan a) (reglan b)))
      | B.Str.RegLan.Union ->
        op2 ~cst (fun a b -> mk_regl (R.union (reglan a) (reglan b)))
      | B.Str.RegLan.Inter ->
        op2 ~cst (fun a b -> mk_regl (R.inter (reglan a) (reglan b)))
      | B.Str.RegLan.Diff ->
        op2 ~cst (fun a b -> mk_regl (Re.diff (reglan a) (reglan b)))
      | B.Str.RegLan.Star ->
        op1 ~cst (fun a -> mk_regl (R.star (reglan a)))
      | B.Str.RegLan.Cross ->
        op1 ~cst (fun a -> mk_regl (Re.plus (reglan a)))
      | B.Str.RegLan.Complement ->
        op1 ~cst (fun a -> mk_regl (R.comp (reglan a)))
      | B.Str.RegLan.Option ->
        op1 ~cst (fun a -> mk_regl (Re.opt (reglan a)))
      | B.Str.RegLan.Power n ->
        op1 ~cst (fun a -> mk_regl (Re.power n (reglan a)))
      | B.Str.RegLan.Loop (n1, n2) ->
        op1 ~cst (fun a -> mk_regl (Re.loop n1 n2 (reglan a)))
    end
  | _ -> None
