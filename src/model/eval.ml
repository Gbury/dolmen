
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr
module B = Dolmen.Std.Builtin
module Term = Dolmen.Std.Expr.Term
module Var = Dolmen.Std.Expr.Term.Var
module Cst = Dolmen.Std.Expr.Term.Const


(* Exceptions *)
(* ************************************************************************* *)

exception Quantifier
exception Partial_model
exception Incomplete_match
exception Unhandled_builtin of Cst.t
exception Undefined_variable of Var.t
exception Undefined_constant of Cst.t

(* Builtins *)
(* ************************************************************************* *)

let builtins (l : Env.builtins list) : Env.builtins =
  let rec aux ~eval env c = function
    | [] -> None
    | b :: r ->
      begin match b ~eval env c with
        | Some _ as res -> res
        | None -> aux ~eval env c r
      end
  in
  (fun ~eval env c -> aux ~eval env c l)


(* Evaluation *)
(* ************************************************************************* *)

let rec eval env (e : Term.t) : Value.t =
  (* Format.eprintf "[eval] @[<hov>%a -> ?@]@." Term.print e; *)
  let r =
    match e.term_descr with
    | Var v -> eval_var env v
    | Cst c -> eval_cst env c
    | App (f, ty_args, t_args) -> eval_apply env f ty_args t_args
    | Binder (b, body) -> eval_binder env b body
    | Match (t, arms) -> eval_match env t arms
  in
  (* Format.eprintf "[eval] @[<hov>%a -> %a@]@." Term.print e Value.print r; *)
  r

and eval_var env v =
  match Model.Var.find_opt v (Env.model env) with
  | Some value -> value
  | None -> raise (Undefined_variable v)

and eval_cst env (c : Cst.t) =
  match c.builtin with
  | B.Base ->
    begin match Model.Cst.find_opt c (Env.model env) with
      | Some value -> value
      | None -> raise (Undefined_constant c)
    end
  | _ ->
    begin match Env.builtins env ~eval env c with
      | Some res -> res
      | None -> raise (Unhandled_builtin c)
    end

and eval_apply env f ty_args args =
  Fun.apply ~eval env (eval env f) ty_args args

and eval_binder env b body =
  match (b : E.binder) with
  | Let_seq l ->
    let env =
      List.fold_left (fun env (v, expr) ->
          let value = eval env expr in
          Env.update_model env (Model.Var.add v value)
        ) env l
    in
    eval env body
  | Let_par l ->
    (* Note: this parrallel treatment is not strictly speaking necessary,
       since for typed expressions, there is no shadowing of variables,
       and we have pure logic semantics (e.g. no side-effects of evaluating
       expressions), but let's keep that for now and potentially change that
       later. *)
    let l' = List.map (fun (v, expr) -> v, eval env expr) l in
    let env =
      List.fold_left (fun env (v, value) ->
          Env.update_model env (Model.Var.add v value)
        ) env l'
    in
    eval env body
  | Lambda (tys, params) ->
    begin match params with
      | [] -> eval env body
      | _ -> Fun.mk_clos @@ Fun.lambda tys params body
    end
  | Exists (_tys, _terms)
  | Forall (_tys, _terms) ->
    raise Quantifier

and eval_match_aux env scrutinee = function
  | [] -> raise Incomplete_match
  | (pat, arm) :: r ->
    begin match Adt.pattern_match env pat scrutinee with
      | None -> eval_match_aux env scrutinee r
      | Some env -> eval env arm
    end

and eval_match env scrutinee arms =
  let scrutinee = eval env scrutinee in
  eval_match_aux env scrutinee arms


