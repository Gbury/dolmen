
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Value definitions *)
(* ************************************************************************* *)

(* String values are [Ustr.t], regex (RegLan) values are [Reglan.t]. *)

module S = Smt_string.Ustr
module R = Smt_string.Reglan

let str_ops = Value.ops ~compare:S.compare ~print:S.pp ()

(* [R.compare] is syntactic equality, not language equivalence. *)
let regl_ops = Value.ops ~compare:R.compare ~print:R.pp ()


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
  if S.length s = 1 then S.get s 0 else None


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
        op2 ~cst (fun s i -> mk_str (S.at (ustr s) (int_of_value i)))
      | B.Str.To_code ->
        op1 ~cst (fun s -> Int.mk (Z.of_int (S.to_code (ustr s))))
      | B.Str.Of_code ->
        op1 ~cst (fun n -> mk_str (S.from_code (int_of_value n)))
      | B.Str.Is_digit ->
        op1 ~cst (fun s -> Bool.mk (S.is_digit (ustr s)))
      | B.Str.To_int ->
        op1 ~cst (fun s -> Int.mk (S.to_int (ustr s)))
      | B.Str.Of_int ->
        op1 ~cst (fun n -> mk_str (S.from_int (z_of_value n)))
      | B.Str.Concat ->
        op2 ~cst (fun a b -> mk_str (S.concat (ustr a) (ustr b)))
      | B.Str.Sub ->
        op3 ~cst (fun s i n ->
            mk_str (S.substr (ustr s) (int_of_value i) (int_of_value n)))
      | B.Str.Index_of ->
        op3 ~cst (fun s sub i ->
            Int.mk (Z.of_int (S.indexof (ustr s) (ustr sub) (int_of_value i))))
      | B.Str.Replace ->
        op3 ~cst (fun s sub by ->
            mk_str (S.replace (ustr s) (ustr sub) (ustr by)))
      | B.Str.Replace_all ->
        op3 ~cst (fun s sub by ->
            mk_str (S.replace_all (ustr s) (ustr sub) (ustr by)))
      | B.Str.Replace_re ->
        op3 ~cst (fun s r by ->
            mk_str (R.replace_re (ustr s) (reglan r) (ustr by)))
      | B.Str.Replace_re_all ->
        op3 ~cst (fun s r by ->
            mk_str (R.replace_re_all (ustr s) (reglan r) (ustr by)))
      | B.Str.Is_prefix ->
        op2 ~cst (fun p s -> Bool.mk (S.prefixof (ustr p) (ustr s)))
      | B.Str.Is_suffix ->
        op2 ~cst (fun q s -> Bool.mk (S.suffixof (ustr q) (ustr s)))
      | B.Str.Contains ->
        op2 ~cst (fun s sub -> Bool.mk (S.contains (ustr s) (ustr sub)))
      | B.Str.Lexicographic_strict ->
        op2 ~cst (fun a b -> Bool.mk (S.lt (ustr a) (ustr b)))
      | B.Str.Lexicographic_large ->
        op2 ~cst (fun a b -> Bool.mk (S.leq (ustr a) (ustr b)))
      | B.Str.In_re ->
        op2 ~cst (fun s r -> Bool.mk (R.in_re (ustr s) (reglan r)))
    end
  | B.Regexp r ->
    begin match r with
      | B.Str.RegLan.T -> assert false (* not a value *)
      | B.Str.RegLan.Empty -> Some (mk_regl R.none)
      | B.Str.RegLan.All -> Some (mk_regl R.all)
      | B.Str.RegLan.Allchar -> Some (mk_regl R.allchar)
      | B.Str.RegLan.Of_string ->
        op1 ~cst (fun s -> mk_regl (R.to_re (ustr s)))
      | B.Str.RegLan.Range ->
        op2 ~cst (fun lo hi ->
            match codepoint_of_singleton lo, codepoint_of_singleton hi with
            | Some lo, Some hi -> mk_regl (R.range lo hi)
            | _ -> mk_regl R.none (* non-singleton bound: empty language *))
      | B.Str.RegLan.Concat ->
        op2 ~cst (fun a b -> mk_regl (R.concat (reglan a) (reglan b)))
      | B.Str.RegLan.Union ->
        op2 ~cst (fun a b -> mk_regl (R.union (reglan a) (reglan b)))
      | B.Str.RegLan.Inter ->
        op2 ~cst (fun a b -> mk_regl (R.inter (reglan a) (reglan b)))
      | B.Str.RegLan.Diff ->
        op2 ~cst (fun a b -> mk_regl (R.diff (reglan a) (reglan b)))
      | B.Str.RegLan.Star ->
        op1 ~cst (fun a -> mk_regl (R.star (reglan a)))
      | B.Str.RegLan.Cross ->
        op1 ~cst (fun a -> mk_regl (R.plus (reglan a)))
      | B.Str.RegLan.Complement ->
        op1 ~cst (fun a -> mk_regl (R.comp (reglan a)))
      | B.Str.RegLan.Option ->
        op1 ~cst (fun a -> mk_regl (R.opt (reglan a)))
      | B.Str.RegLan.Power n ->
        op1 ~cst (fun a -> mk_regl (R.power n (reglan a)))
      | B.Str.RegLan.Loop (n1, n2) ->
        op1 ~cst (fun a -> mk_regl (R.loop n1 n2 (reglan a)))
    end
  | _ -> None
