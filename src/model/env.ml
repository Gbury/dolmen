
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions & modules *)
(* ************************************************************************* *)

module V = Map.Make(Dolmen.Std.Expr.Term.Var)
module C = Map.Make(Dolmen.Std.Expr.Term.Const)

type t = {
  model : Model.t;
  builtins : t -> Dolmen.Std.Expr.Term.Const.t -> Value.t;
}

(* Common functions *)
(* ************************************************************************* *)

let mk model ~builtins =
  { model; builtins; }

let builtins t = t.builtins t

let model { model; _ } = model

let update_model t f = { t with model = f t.model; }


