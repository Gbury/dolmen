
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr

exception Comparison_of_functional_values

type value_function =
  | Builtin of {
      arity : int;
      cst : E.Term.Const.t;
      eval_f : Value.t list -> Value.t;
    }
  | Lambda of {
      ty_params : E.Ty.Var.t list;
      term_params : E.Term.Var.t list;
      body : E.Term.t;
    }

and t = {
  func : value_function;
  args : Value.t list; (* partial applications arguments *)
}


let print fmt { func; args; } =
  let return fmt_str out () = Format.fprintf out "%(%)" fmt_str in
  let f =
    match func with
    | Builtin { cst; _ } ->
      E.Term.of_cst cst
    | Lambda { ty_params; term_params; body; } ->
      E.Term.lam (ty_params, term_params) body
  in
  match args with
  | [] ->
    Format.fprintf fmt "%a" E.Term.print f
  | _ ->
    Format.fprintf fmt "@[<hov 2>%a(%a)@]"
      E.Term.print f
      (Format.pp_print_list ~pp_sep:(return ",@ ") Value.print) args


let compare _ _ =
  raise Comparison_of_functional_values

(* Value definition *)
let ops = Value.ops ~compare ~print


(* Creation *)
(* ************************************************************************* *)

let mk ~env ~eval ty_params term_params body =
  match term_params with
  | [] -> eval env body
  | _ ->
    let func = Lambda { ty_params; term_params; body; } in
    Value.mk ~ops { args = []; func; }

let builtin ~arity ~cst eval_f =
  let func = Builtin { arity; cst; eval_f; } in
  Value.mk ~ops { args = []; func; }

let fun_1 ~cst f =
  builtin ~arity:1 ~cst (function
      | [x] -> f x
      | _ -> assert false
    )

let fun_2 ~cst f =
  builtin ~arity:2 ~cst (function
      | [x; y] -> f x y
      | _ -> assert false
    )

let fun_3 ~cst f =
  builtin ~arity:3 ~cst (function
      | [x; y; z] -> f x y z
      | _ -> assert false
    )

let fun_n ~cst eval_f =
  let _, arity = E.Term.Const.arity cst in
  builtin ~arity ~cst eval_f


(* Application&Reduction *)
(* ************************************************************************* *)

let arity = function
  | Builtin { arity; _ } -> arity
  | Lambda { term_params; _ } -> List.length term_params

let reduce ~eval env func args =
  match func with
  | Builtin { arity; cst = _; eval_f; } ->
    assert (List.length args = arity);
    eval_f args
  | Lambda { ty_params = _; term_params; body; } ->
    assert (List.length term_params = List.length args);
    let env =
      List.fold_left2 (fun env var value ->
          Env.Var.add var value env
        ) env term_params args
    in
    eval env body

let take_drop n l =
  let rec aux acc n = function
    | r when n <= 0 -> List.rev acc, r
    | [] -> raise (Invalid_argument "take_drop")
    | x :: r -> aux (x :: acc) (n - 1) r
  in
  aux [] n l

let[@specialise] rec apply ~eval env v = function
  | [] -> v
  | new_args ->
    begin match Value.extract ~ops v with
      | None ->
        assert false (* internal error: well-typed terms should not allow this *)
      | Some { func; args = partial_args; } ->
        let f_arity = arity func in
        let n = List.length partial_args in
        let m = List.length new_args in
        if n + m < f_arity then begin
          (* partial application *)
          let args = List.rev_append new_args partial_args in
          Value.mk ~ops { func; args; }
        end else begin
          let all_args = List.rev_append partial_args new_args in
          let full_args, over_args = take_drop f_arity all_args in
          let v' = reduce ~eval env func full_args in
          apply ~eval env v' over_args
        end
    end

