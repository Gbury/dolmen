
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Builtins *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin

let rec all_equals = function
  | [] | [_] -> Bool.mk true
  | x :: ((y :: _) as r) ->
    if Value.compare x y = 0 then all_equals r else Bool.mk false

let rec distinct = function
  | [] | [_] -> Bool.mk true
  | x :: r ->
    if List.for_all (fun y -> Value.compare x y <> 0) r
    then distinct r else Bool.mk false

let builtins ~eval:_ _ (cst : E.Term.Const.t) =
  match cst.builtin with
  | B.Equal -> Some (Fun.fun_n ~cst all_equals)
  | B.Distinct -> Some (Fun.fun_n ~cst distinct)
  | B.Ite ->
    Some (Fun.mk_clos @@ Fun.fun_lazy ~cst (fun env eval args ->
        match args with
        | [cond; then_; else_] ->
          if Value.extract_exn ~ops:Bool.ops (eval env cond)
          then eval env then_
          else eval env else_
        | _ -> assert false
      ))
  | _ -> None

