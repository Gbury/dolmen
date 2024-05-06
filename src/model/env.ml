
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions & modules *)
(* ************************************************************************* *)

module V = Map.Make(Dolmen.Std.Expr.Term.Var)
module C = Map.Make(Dolmen.Std.Expr.Term.Const)

type builtins =
  eval:(t -> Dolmen.Std.Expr.Term.t -> Value.t) ->
  t -> Dolmen.Std.Expr.Term.Const.t -> Value.t option

and t = {
  model : Model.t;
  builtins : builtins;
}

(* Common functions *)
(* ************************************************************************* *)

let mk model ~builtins =
  { model; builtins; }

let builtins t = t.builtins

let model { model; _ } = model

let update_model t f = { t with model = f t.model; }

(* Extensions builtins *)
(* ************************************************************************ *)

module Ext = struct
  type t = {
    name : string;
    builtins : builtins;
  }

  let all = ref []
  let list () = !all
  let name { name; _ } = name
  let builtins { builtins; _ } = builtins

  let create ~name ~builtins =
    let t = { name; builtins; } in
    all := t :: !all;
    t
end
