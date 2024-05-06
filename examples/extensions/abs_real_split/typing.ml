module Type = Dolmen_loop.Typer.T
module Ty = Dolmen.Std.Expr.Ty
module Term = Dolmen.Std.Expr.Term
module Builtin = Dolmen.Std.Builtin
module Base = Dolmen_type.Base

module Const = struct
  let mk' ?pos ?name ?builtin ?tags cname vars args ret =
    let ty = Ty.pi vars (Ty.arrow args ret) in
    Dolmen.Std.Expr.Id.mk ?pos ?name ?builtin ?tags
      (Dolmen.Std.Path.global cname) ty

  module Real = struct
    (* NB: Use [Dolmen.Std.Expr.with_cache] for parameterized builtins. *)
    let abs =
      mk' ~builtin:Abs_real_split.Builtin.Abs_real "abs_real"
        [] [Ty.real] Ty.real
  end
end

let abs_real x = Term.apply_cst Const.Real.abs [] [x]

let builtins _lang env s =
  match s with
  | Type.Id { ns = Term ; name = Simple "abs_real" } ->
    Type.builtin_term (Base.term_app1 (module Type) env s abs_real)
  | _ -> `Not_found

let typing_ext =
  Dolmen_loop.Typer.Ext.create ~name:"abs_real_split" ~builtins
