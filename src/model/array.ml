
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Useful shorthand for chaining comparisons *)
let (<?>) = Dolmen.Std.Misc.(<?>)


(* Type definition *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

type base =
  | Const of Value.t
  | Abstract of E.Term.Const.t

type t = {
  base : base;
  map : Value.t Value.Map.t;
}
(* invariant : none of the keys in the map binds to the [Const] value of
   [base] if it is not abstract. *)

(* abstract arrays, as defined in the smtlib standard
   NOTE: to make cvc5 output models using abstract values, use the
         `--abstract-values` option
*)
let abstract cst = {
  base = Abstract cst;
  map = Value.Map.empty;
}

(* Printing functions *)
let print_base fmt = function
  | Const base ->
    Format.fprintf fmt "_ -> %a;@ " Value.print base
  | Abstract cst ->
    Format.fprintf fmt "%a;@ " E.Term.Const.print cst

let print_map fmt map =
  Value.Map.iter (fun key value ->
      Format.fprintf fmt "%a -> %a;@ "
        Value.print key Value.print value
    ) map

let print fmt { map; base; } =
  Format.fprintf fmt "@[<hv 1>{ %a%a@] }"
    print_map map print_base base

(* Comparison *)
let compare_base base base' =
  match base, base' with
  | Abstract cst, Abstract cst' -> E.Term.Const.compare cst cst'
  | Const _, Abstract _ -> -1
  | Abstract _, Const _ -> 1
  | Const v, Const v' -> Value.compare v v'

let compare_map map map' =
  Value.Map.compare Value.compare map map'

let compare t t' =
  compare_base t.base t'.base
  <?> (compare_map, t.map, t'.map)

(* value ops *)
let ops = Value.ops ~abstract ~print ~compare ()


(* Manipulation functions *)
(* ************************************************************************* *)

let const base =
  Value.mk ~ops { map = Value.Map.empty; base = Const base; }

let select array key =
  let { map; base; } = Value.extract_exn ~ops array in
  match Value.Map.find_opt key map with
  | Some res -> res
  | None ->
    begin match base with
      | Const res -> res
      | Abstract _ -> raise Eval.Partial_model
    end

let store array key value =
  let { map; base; } = Value.extract_exn ~ops array in
  let map' =
    match base with
    | Abstract _ -> Value.Map.add key value map
    | Const base ->
      if Value.compare value base = 0 then
        Value.Map.remove key map
      else
        Value.Map.add key value map
  in
  if map == map' then array
  else Value.mk ~ops { base; map = map'; }


(* Builtin values *)
(* ************************************************************************* *)

let builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Const -> Some (Fun.mk_clos @@ Fun.fun_1 ~cst const)
  | B.Select -> Some (Fun.mk_clos @@ Fun.fun_2 ~cst select)
  | B.Store -> Some (Fun.mk_clos @@ Fun.fun_3 ~cst store)
  | _ -> None


