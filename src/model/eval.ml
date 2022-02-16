
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
exception Unhandled_builtin of Cst.t
exception Undefined_variable of Var.t
exception Undefined_constant of Cst.t


(* Builtins *)
(* ************************************************************************* *)

let rec builtins l c =
  match l with
  | [] -> raise (Unhandled_builtin c)
  | b :: r ->
    begin match b c with
      | Some value -> value
      | None -> builtins r c
    end


(* Evaluation *)
(* ************************************************************************* *)

let rec eval env (e : Term.t) : Value.t =
  match e.term_descr with
  | Var v -> eval_var env v
  | Cst c -> eval_cst env c
  | App (f, _, t_args) -> eval_apply env f t_args
  | Binder (b, body) -> eval_binder env b body
  | Match (t, arms) -> eval_match env t arms

and eval_var env v =
  match Env.Var.find_opt v env with
  | Some value -> value
  | None -> raise (Undefined_variable v)

and eval_cst env (c : Cst.t) =
  match c.builtin with
  | B.Base ->
    begin match Env.Cst.find_opt c env with
      | Some value -> value
      | None -> raise (Undefined_constant c)
    end
  | _ -> Env.builtins env c

and eval_apply env f t_args =
  let f_v = eval env f in
  let args_v = List.map (eval env) t_args in
  Fun.apply ~eval env f_v args_v

and eval_binder env b body =
  match (b : E.binder) with
  | Let_seq l ->
    let env =
      List.fold_left (fun env (v, expr) ->
          let value = eval env expr in
          Env.Var.add v value env
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
          Env.Var.add v value env
        ) env l'
    in
    eval env body
  | Lambda (tys, params) ->
    Fun.mk ~env ~eval tys params body
  | Exists (_tys, _terms)
  | Forall (_tys, _terms) ->
    raise Quantifier

and eval_match _env _scrutinee _arms = assert false

