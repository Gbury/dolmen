
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions & modules *)
(* ************************************************************************* *)

module V = Map.Make(Dolmen.Std.Expr.Term.Var)
module C = Map.Make(Dolmen.Std.Expr.Term.Const)

type t = {
  vars : Value.t V.t;
  csts : Value.t C.t;
  builtins : Dolmen.Std.Expr.Term.Const.t -> Value.t;
}


(* Common functions *)
(* ************************************************************************* *)

let empty ~builtins =
  { vars = V.empty; csts = C.empty; builtins; }

let builtins t = t.builtins


(* Mapped var&cst values *)
(* ************************************************************************* *)

module type S = sig

  type key

  val find_opt : key -> t -> Value.t option

  val add : key -> Value.t -> t -> t

end

(* vars *)

module Var
  : S with type key := Dolmen.Std.Expr.Term.Var.t
= struct

  let[@inline] find_opt v t =
    match V.find v t.vars with
    | res -> Some res
    | exception Not_found -> None

  let add v value t =
    { t with vars = V.add v value t.vars; }

end

(* csts *)

module Cst
  : S with type key := Dolmen.Std.Expr.Term.Const.t
= struct

  let[@inline] find_opt c t =
    match C.find c t.csts with
    | res -> Some res
    | exception Not_found -> None

  let add c value t =
    { t with csts = C.add c value t.csts; }

end
