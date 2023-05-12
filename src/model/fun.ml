
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr

exception Comparison_of_functional_values
exception Function_value_expected of Value.t
exception Bad_arity of E.Term.Const.t * int * Value.t list
exception Incomplete_ad_hoc_function of E.Term.Const.t

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
  | Poly of {
      arity : int;
      cst : E.Term.Const.t;
      eval_p : E.Ty.t list -> Value.t list -> Value.t;
    } (* parametric polymorphic functions (including non-polymorphic ones) *)
  | Ad_hoc of {
      arity : int;
      ty_arity : int;
      cst : E.Term.Const.t;
      eval_l : (E.Ty.t list * (E.Ty.subst -> value_function)) list;
      (* each element of the list is basically a case of a pattern match on
         the type arguments. *)
    } (* ad-hoc parametric polymorphic functions *)

and t =
  | Closure of {
      func : value_function;
      tys : E.ty list; (* type args *)
      args : E.term list; (* partial applications arguments *)
    }

let return fmt_str out () = Format.fprintf out "%(%)" fmt_str

let print_ad_hoc_case fmt (patterns, _arm) =
  Format.fprintf fmt "%a -> <fun>"
    (Format.pp_print_list ~pp_sep:(return ",@ ") E.Ty.print) patterns

let print fmt (Closure { func; tys; args; }) =
  let f =
    match func with
    | Poly { cst; _ } ->
      E.Term.of_cst cst
    | Lambda { ty_params; term_params; body; } ->
      E.Term.lam (ty_params, term_params) body
    | Lazy { cst; _ } ->
      E.Term.of_cst cst
    | Ad_hoc { cst; arity = _; ty_arity = _; eval_l = _; } ->
      E.Term.of_cst cst
  in
  match tys, args with
  | [], [] ->
    Format.fprintf fmt "< %a >" E.Term.print f
  | [], _ ->
    Format.fprintf fmt "< @[<hov 2>%a(%a)@] >"
      E.Term.print f
      (Format.pp_print_list ~pp_sep:(return ",@ ") E.Term.print) args
  | _, _ ->
    Format.fprintf fmt "< @[<hov 2>%a(%a)(%a)@] >"
      E.Term.print f
      (Format.pp_print_list ~pp_sep:(return ",@ ") E.Ty.print) tys
      (Format.pp_print_list ~pp_sep:(return ",@ ") E.Term.print) args

let compare _ _ =
  raise Comparison_of_functional_values

(* Value definition *)
let ops = Value.ops ~compare ~print ()


(* Creation *)
(* ************************************************************************* *)

let mk_clos func =
  Value.mk ~ops (Closure { func; tys = []; args = []; })

let ad_hoc ~cst ~arity ~ty_arity eval_l =
  Ad_hoc { cst; arity; ty_arity; eval_l; }

let lambda ty_params term_params body =
  match term_params with
  | [] -> failwith "not a function"
  | _ -> Lambda { ty_params; term_params; body; }

let fun_lazy ~cst eval_lazy =
  let _, params, _ = E.Ty.poly_sig (E.Term.Const.ty cst) in
  let arity = List.length params in
  Lazy { arity; cst; eval_lazy; }

let poly ~arity ~cst eval_p =
  if arity = 0 then assert false
  else Poly { arity; cst; eval_p; }

let builtin ~arity ~cst eval_m =
  poly ~arity ~cst (fun _ args -> eval_m args)

let fun_1 ~cst f =
  builtin ~arity:1 ~cst (function
      | [x] -> f x
      | l -> raise (Bad_arity (cst, 1, l))
    )

let fun_2 ~cst f =
  builtin ~arity:2 ~cst (function
      | [x; y] -> f x y
      | l -> raise (Bad_arity (cst, 2, l))
    )

let fun_3 ~cst f =
  builtin ~arity:3 ~cst (function
      | [x; y; z] -> f x y z
      | l -> raise (Bad_arity (cst, 3, l))
    )

let fun_4 ~cst f =
  builtin ~arity:4 ~cst (function
      | [x; y; z; t] -> f x y z t
      | l -> raise (Bad_arity (cst, 4, l))
    )

let fun_n ~cst eval_f =
  let _, params, _ = E.Ty.poly_sig (E.Term.Const.ty cst) in
  let arity = List.length params in
  if arity = 0 then eval_f []
  else mk_clos @@ builtin ~arity ~cst eval_f


(* Application&Reduction *)
(* ************************************************************************* *)

let arity = function
  | Lazy { arity; _ } -> arity
  | Poly { arity; _ } -> arity
  | Ad_hoc { arity; _ } -> arity
  | Lambda { term_params; _ } -> List.length term_params

let add_ty_args partial_tys partial_args new_tys =
  match partial_args, new_tys with
  | _, [] -> partial_tys
  | [], _ -> partial_tys @ new_tys
  | _ :: _, _ :: _ -> failwith "dependant application"

let rec reduce_ty ~eval ~cst env ty_args = function
  | [] -> raise (Incomplete_ad_hoc_function cst)
  | (patterns, eval_a) :: r ->
    begin match E.Ty.match_ patterns ty_args with
      | None -> reduce_ty ~eval ~cst env ty_args r
      | Some subst -> eval_a subst
    end

(* specific function for function applications directly to values *)
let rec reduce_val ~eval env func tys args =
  match func with
  | Lazy _ -> assert false
  | Poly { arity; eval_p; cst = _; } ->
    assert (List.length args = arity);
    eval_p tys args
  | Ad_hoc { arity; ty_arity; eval_l; cst; } ->
    assert (List.length args = arity);
    assert (List.length tys = ty_arity);
    let func = reduce_ty ~eval ~cst env tys eval_l in
    reduce_val ~eval env func [] args
  | Lambda { ty_params = _; term_params; body; } ->
    assert (List.length term_params = List.length args);
    let env =
      List.fold_left2 (fun env var value ->
          Env.update_model env (Model.Var.add var value)
        ) env term_params args
    in
    eval env body

let apply_val ~eval env f new_tys new_args =
  match Value.extract_exn ~ops f with
  | Closure { func; tys; args = partial_args; } ->
    let tys = add_ty_args tys partial_args new_tys in
    let f_arity = arity func in
    let n = List.length partial_args in
    let m = List.length new_args in
    assert (n + m = f_arity);
    let partial_args = List.map (eval env) partial_args in
    let all_args = List.rev_append partial_args new_args in
    reduce_val ~eval env func tys all_args

(* helper function *)
let take_drop n l =
  let rec aux acc n = function
    | r when n <= 0 -> List.rev acc, r
    | [] -> raise (Invalid_argument "take_drop")
    | x :: r -> aux (x :: acc) (n - 1) r
  in
  aux [] n l

(* regular/generic function for function applicaiton *)
let rec reduce ~eval env func tys args =
  match func with
  | Lazy { arity; eval_lazy; _ } ->
    assert (List.length args = arity);
    eval_lazy env eval args
  | Poly { arity; eval_p; _ } ->
    assert (List.length args = arity);
    let args = List.map (eval env) args in
    eval_p tys args
  | Ad_hoc { arity; ty_arity; eval_l; cst; } ->
    assert (List.length args = arity);
    assert (List.length tys = ty_arity);
    let func = reduce_ty ~eval ~cst env tys eval_l in
    reduce ~eval env func [] args
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
      | Some Closure { func; tys = partial_tys; args = partial_args; } ->
        let tys = add_ty_args partial_tys partial_args ty_args in
        let f_arity = arity func in
        let n = List.length partial_args in
        let m = List.length term_args in
        if n + m < f_arity then begin
          (* partial application *)
          let args = List.rev_append term_args partial_args in
          Value.mk ~ops (Closure { func; tys; args; })
        end else begin
          let all_args = List.rev_append partial_args term_args in
          let full_args, over_args = take_drop f_arity all_args in
          let v' = reduce ~eval env func tys full_args in
          (* dependant typing is not supported, we thus drop all type arguments
             after reducing a function application *)
          apply ~eval env v' [] over_args
        end
    end

(* Corner cases helpers *)
(* ************************************************************************* *)

let corner_case ?post_check ~eval env cst tys args =
  match Model.Cst.find_opt cst (Env.model env) with
  | Some value ->
    (* Remove the corner case value to avoid infinite recursive evaluation *)
    let env = Env.update_model env (Model.Cst.remove cst) in
    let res = apply_val ~eval env value tys args in
    begin match post_check with
      | None -> ()
      | Some f -> f res
    end;
    res
  | None -> raise (Model.Partial_interpretation (cst, args))

(* Ad-hoc polymorphism helpers *)
(* ************************************************************************* *)

let add_ad_hoc_instance model ~cst ~ty_args ~term_params ~body =
  let eval_l, arity, ty_arity =
    match Model.Cst.find_opt cst model with
    | None -> [], List.length term_params, List.length ty_args
    | Some v ->
      begin match Value.extract ~ops v with
        | Some Closure {
            tys = []; args = [];
            func = Ad_hoc { eval_l; ty_arity; arity; cst = c; } } ->
          assert (E.Term.Const.equal cst c);
          assert (List.length ty_args = ty_arity);
          assert (List.length term_params = arity);
          eval_l, arity, ty_arity
        | None | Some Closure _ ->
          assert false
      end
  in
  let func = Lambda { ty_params = []; term_params; body; } in
  let eval_l = (ty_args, (fun _ -> func)) :: eval_l in
  let v = mk_clos (ad_hoc ~cst ~ty_arity ~arity eval_l) in
  Model.Cst.add cst v model

