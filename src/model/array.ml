
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

type t = {
  base : Value.t option;
  map : Value.t Value.Map.t;
}
(* invariant : none of the keys in the map binds to the [base] value *)


let print_base fmt = function
  | None -> ()
  | Some base ->
    Format.fprintf fmt "_ -> %a;@ " Value.print base

let print_map fmt map =
  Value.Map.iter (fun key value ->
      Format.fprintf fmt "%a -> %a;@ "
        Value.print key Value.print value
    ) map

let print fmt { map; base; } =
  Format.fprintf fmt "@[<hv 1>{ %a%a@] }"
    print_map map print_base base

let compare_base base base' =
  match base, base' with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some v, Some v' -> Value.compare v v'

let compare_map map map' =
  Value.Map.compare Value.compare map map'

let compare t t' =
  match compare_base t.base t'.base with
  | 0 -> compare_map t.map t'.map
  | x -> x

let ops = Value.ops ~print ~compare

(* Manipulation functions *)
(* ************************************************************************* *)

let const base =
  Value.mk ~ops { map = Value.Map.empty; base = Some base; }

let select array key =
  let { map; base; } = Value.extract_exn ~ops array in
  match Value.Map.find_opt key map with
  | Some res -> res
  | None ->
    begin match base with
      | Some res -> res
      | None -> raise Eval.Partial_model
    end

let store array key value =
  let { map; base; } = Value.extract_exn ~ops array in
  let map' =
    match base with
    | None -> Value.Map.add key value map
    | Some base ->
      if Value.compare value base = 0 then
        Value.Map.remove key map
      else
        Value.Map.add key value map
  in
  if map == map' then array
  else Value.mk ~ops { base; map = map'; }


(* Builtin values *)
(* ************************************************************************* *)

let builtins (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Const -> Some (Fun.fun_1 ~cst const)
  | B.Select -> Some (Fun.fun_2 ~cst select)
  | B.Store -> Some (Fun.fun_3 ~cst store)
  | _ -> None


