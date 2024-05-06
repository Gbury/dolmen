module Smtlib2_Bvconv =
  Dolmen_type.Bitv.Smtlib2.Bvconv(Dolmen_loop.Typer.T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Bitv)

let typing_extension =
  Dolmen_loop.Typer.Ext.create ~name:"bvconv"
    ~builtins:(function
      | `Logic Dolmen_loop.Logic.Smtlib2 version ->
        Smtlib2_Bvconv.parse (`Script version)
      | _ -> Dolmen_type.Base.noop)
