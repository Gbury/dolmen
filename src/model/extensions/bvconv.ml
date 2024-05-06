module B = Dolmen.Std.Builtin
module Fun = Dolmen_model.Fun

let int = Dolmen_model.Int.mk
let of_int = Dolmen_model.Value.extract_exn ~ops:Dolmen_model.Int.ops
let bitv ~size x = Dolmen_model.Bitv.mk size (Z.extract x 0 size)
let of_bitv ~size = Dolmen_model.Bitv.ubitv size

let bv2nat ~cst ~size =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> int (of_bitv ~size x)))

let int2bv ~cst ~size =
  Some (Fun.mk_clos @@ Fun.fun_1 ~cst (fun x -> bitv ~size (of_int x)))

let builtins ~eval:_ _ (cst : Dolmen.Std.Expr.Term.Const.t) =
  match cst.builtin with
  | B.Bitv_of_int { n } -> int2bv ~cst ~size:n
  | B.Bitv_to_nat { n } -> bv2nat ~cst ~size:n
  | _ -> None

let model_ext =
  Dolmen_model.Env.Ext.create ~name:"bvconv" ~builtins
