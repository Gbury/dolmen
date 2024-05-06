module B = Dolmen.Std.Builtin
module Fun = Dolmen_model.Fun

let real = Dolmen_model.Real.mk
let of_real = Dolmen_model.Real.get

let abs_real ~cst =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> real (Q.abs (of_real x))))

let builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | Abs_real_split.Builtin.Abs_real -> abs_real ~cst
  | _ -> None

let model_ext =
  Dolmen_model.Env.Ext.create ~name:"abs_real_split" ~builtins
