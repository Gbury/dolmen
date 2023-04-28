
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr

exception Comparison_of_functional_values
exception Function_value_expected of Value.t
exception Bad_arity of E.Term.Const.t * int * Value.t list

type value_function =
  | Lambda of {
      ty_params : E.Ty.Var.t list;
      term_params : E.Term.Var.t list;
      body : E.Term.t;
    }
  | Lazy of {
      arity : int;
      cst : E.Term.Const.t;
      eval_lazy : Env.t -> (Env.t -> E.term -> Value.t) -> E.term list -> Value.t;
    }
  | Parametric of {
      arity : int;
      cst : E.Term.Const.t;
      ty_args : E.Ty.t list;
      eval_p : Value.t list -> Value.t;
    } (* parametric polymorphic functions (including non-polymorphic ones) *)

and t =
  | Partial of {
      func : value_function;
      args : E.term list; (* partial applications arguments *)
    }
  | Ad_hoc of {
      cst : E.Term.Const.t;
      ty_args : E.Ty.t list;
      eval_a : E.Ty.t -> Value.t;
    } (* ad-hoc parametric polymorphic functions, curried for simplicity *)

let return fmt_str out () = Format.fprintf out "%(%)" fmt_str

let print fmt = function
  | Ad_hoc { cst; ty_args; eval_a = _; } ->
    Format.fprintf fmt "< @[<hov 2>%a(%a)@] >"
      E.Term.Const.print cst
      (Format.pp_print_list ~pp_sep:(return ",@ ") E.Ty.print) ty_args
  | Partial { func; args; } ->
    let f =
      match func with
      | Parametric { cst; ty_args = []; _ } ->
        E.Term.of_cst cst
      | Parametric { cst; ty_args = ((_ :: _) as ty_args); _ } ->
        E.Term.apply (E.Term.of_cst cst) ty_args []
      | Lambda { ty_params; term_params; body; } ->
        E.Term.lam (ty_params, term_params) body
      | Lazy { cst; _ } ->
        E.Term.of_cst cst
    in
    match args with
    | [] ->
      Format.fprintf fmt "< %a >" E.Term.print f
    | _ ->
      Format.fprintf fmt "< @[<hov 2>%a(%a)@] >"
        E.Term.print f
        (Format.pp_print_list ~pp_sep:(return ",@ ") E.Term.print) args

let compare _ _ =
  raise Comparison_of_functional_values

(* Value definition *)
let ops = Value.ops ~compare ~print ()


(* Creation *)
(* ************************************************************************* *)

let ad_hoc ~cst ?(ty_args=[]) eval_a =
  Value.mk ~ops (Ad_hoc { cst; ty_args; eval_a; })

let mk ~env ~eval ty_params term_params body =
  match term_params with
  | [] -> eval env body
  | _ ->
    let func = Lambda { ty_params; term_params; body; } in
    Value.mk ~ops (Partial { args = []; func; })

let builtin ~arity ~cst ?(ty_args=[]) eval_p =
  if arity = 0 then eval_p []
  else begin
    let func = Parametric { arity; cst; ty_args; eval_p; } in
    Value.mk ~ops (Partial { args = []; func; })
  end

let fun_1 ~cst ?ty_args f =
  builtin ~arity:1 ~cst ?ty_args (function
      | [x] -> f x
      | l -> raise (Bad_arity (cst, 1, l))
    )

let fun_2 ~cst ?ty_args f =
  builtin ~arity:2 ~cst ?ty_args (function
      | [x; y] -> f x y
      | l -> raise (Bad_arity (cst, 2, l))
    )

let fun_3 ~cst ?ty_args f =
  builtin ~arity:3 ~cst ?ty_args (function
      | [x; y; z] -> f x y z
      | l -> raise (Bad_arity (cst, 3, l))
    )

let fun_4 ~cst ?ty_args f =
  builtin ~arity:4 ~cst ?ty_args (function
      | [x; y; z; t] -> f x y z t
      | l -> raise (Bad_arity (cst, 4, l))
    )

let fun_n ~cst ?ty_args eval_f =
  let _, params, _ = E.Ty.poly_sig (E.Term.Const.ty cst) in
  let arity = List.length params in
  builtin ~arity ~cst ?ty_args eval_f

let fun_lazy ~cst eval_lazy =
  let _, params, _ = E.Ty.poly_sig (E.Term.Const.ty cst) in
  let arity = List.length params in
  let func = Lazy { arity; cst; eval_lazy; } in
  Value.mk ~ops (Partial { args = []; func; })


(* Application&Reduction *)
(* ************************************************************************* *)

let arity = function
  | Lazy { arity; _ } -> arity
  | Parametric { arity; _ } -> arity
  | Lambda { term_params; _ } -> List.length term_params

(* specific function for function applications directly to values *)
let reduce_val ~eval env func args =
  match func with
  | Lazy _ -> assert false
  | Parametric { arity; eval_p; _ } ->
    assert (List.length args = arity);
    eval_p args
  | Lambda { ty_params = _; term_params; body; } ->
    assert (List.length term_params = List.length args);
    let env =
      List.fold_left2 (fun env var value ->
          Env.update_model env (Model.Var.add var value)
        ) env term_params args
    in
    eval env body

let apply_val ~eval env f new_args =
  match Value.extract_exn ~ops f with
  | Ad_hoc _ -> assert false
  | Partial { func; args = partial_args; } ->
    let f_arity = arity func in
    let n = List.length partial_args in
    let m = List.length new_args in
    assert (n + m = f_arity);
    let partial_args = List.map (eval env) partial_args in
    let all_args = List.rev_append partial_args new_args in
    reduce_val ~eval env func all_args

(* helper function *)
let take_drop n l =
  let rec aux acc n = function
    | r when n <= 0 -> List.rev acc, r
    | [] -> raise (Invalid_argument "take_drop")
    | x :: r -> aux (x :: acc) (n - 1) r
  in
  aux [] n l

(* regular/generic function for function applicaiton *)
let reduce ~eval env func args =
  match func with
  | Lazy { arity; eval_lazy; _ } ->
    assert (List.length args = arity);
    eval_lazy env eval args
  | Parametric { arity; eval_p; _ } ->
    assert (List.length args = arity);
    let args = List.map (eval env) args in
    eval_p args
  | Lambda { ty_params = _; term_params; body; } ->
    assert (List.length term_params = List.length args);
    let env =
      List.fold_left2 (fun env var term ->
          let value = eval env term in
          Env.update_model env (Model.Var.add var value)
        ) env term_params args
    in
    eval env body

let[@specialise] rec apply ~eval env v ty_args term_args =
  match ty_args, term_args with
  | [], [] -> v
  | _, _ ->
    begin match Value.extract ~ops v with
      | None ->
        (* parametric polymorphic function that take no term arguments
           (such as the 'nil' construcotr for lists), are technically
           functions that take some type arguments, but their value is
           not a function value (e.g. it can be an adt value). In that
           case, we make it so that the application simply ignores all
           the type arguments. *)
        begin match term_args with
          | [] -> v
          | _ :: _ -> raise (Function_value_expected v)
        end
      | Some Ad_hoc { eval_a; _ } ->
        begin match ty_args with
          | [] -> assert false
          | ty_arg :: other_ty_args ->
            let v' = eval_a ty_arg in
            apply ~eval env v' other_ty_args term_args
        end
      | Some Partial { func; args = partial_args; } ->
        let f_arity = arity func in
        let n = List.length partial_args in
        let m = List.length term_args in
        if n + m < f_arity then begin
          (* partial application *)
          let args = List.rev_append term_args partial_args in
          Value.mk ~ops (Partial { func; args; })
        end else begin
          let all_args = List.rev_append partial_args term_args in
          let full_args, over_args = take_drop f_arity all_args in
          let v' = reduce ~eval env func full_args in
          (* dependant typing is not supported, we thus drop all type arguments
             after reducing a function application *)
          apply ~eval env v' [] over_args
        end
    end
