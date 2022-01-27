
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions *)
(* ************************************************************************* *)

module B = Dolmen.Std.Builtin
module Term = Dolmen.Std.Expr.Term
module Var = Dolmen.Std.Expr.Term.Var
module Cst = Dolmen.Std.Expr.Term.Const


(* Exceptions *)
(* ************************************************************************* *)

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

and eval_binder _env _b _body = assert false

and eval_match _env _scrutinee _arms = assert false

