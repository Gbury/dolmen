
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = Tff_intf.S

(* Typechecking functor *)
(* ************************************************************************ *)

module Make
    (Tag: Dolmen.Intf.Tag.S)
    (Ty: Dolmen.Intf.Ty.Tff
     with type 'a tag := 'a Tag.t
      and type path := Dolmen.Std.Path.t)
    (T: Dolmen.Intf.Term.Tff
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type ty_def := Ty.def
      and type 'a tag := 'a Tag.t
      and type path := Dolmen.Std.Path.t)
= struct

  include Thf.Make(Tag)
      (struct
        include Ty
        let arrow _ _ = assert false
        let pi _ _ = assert false
      end)
      (struct
        include T
        let apply _ _ _ = assert false
      end)

  let empty_env
      ?st ?expect ?var_infer ?sym_infer
      ?(order=First_order) ?poly ?quants
      ?free_wildcards ~warnings ~file builtin_symbols =
    let env =
      empty_env ?st
        ?expect ?var_infer ?sym_infer
        ~order ?poly ?quants ?free_wildcards
        ~warnings ~file builtin_symbols
    in
    match order with
    | First_order -> env
    | Higher_order ->
      _error env (Located Dolmen.Std.Loc.no_loc) Higher_order_env_in_tff_typechecker

end
