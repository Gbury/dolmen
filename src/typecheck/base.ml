
open Dolmen

(** TPTP builtins ($i, $o, etc..) *)
(* ************************************************************************ *)

module Tptp = struct

  (** Type constants required to typecheck tptp builtins *)
  module type Ty = sig
    type t
    val prop : t

    module Const : sig
      type t
      val base : t
    end
  end

  module type T = sig
    module Const : sig
      type t
      val _true : t
      val _false : t
    end
  end

  module Tff
      (Type : Tff_intf.S)
      (Ty : Ty with type t = Type.Ty.t
                and type Const.t = Type.Ty.Const.t)
      (T : T with type Const.t = Type.T.Const.t) = struct

    let parse env ast s args =
      match s with
      (*
      | Type.Id ({ Id.name = "$_"; ns = Id.Term } as id) ->
        Some (Type.wildcard env ast id args)
      *)
      | Type.Id { Id.name = "$tType"; ns = Id.Term } ->
        Some Type.Ttype
      | Type.Id { Id.name = "$o"; ns = Id.Term } ->
        begin match args with
          | [] -> Some (Type.Ty Ty.prop)
          | _ ->
            let err = Type.Bad_op_arity ("$o", 0, List.length args) in
            raise (Type.Typing_error (err, env, ast))
        end
      | Type.Id { Id.name = "$i"; ns = Id.Term } ->
        Some (Type.parse_app_ty env ast Ty.Const.base args)
      | Type.Id { Id.name = "$true"; ns = Id.Term } ->
        Some (Type.parse_app_term env ast T.Const._true args)
      | Type.Id { Id.name = "$false"; ns = Id.Term } ->
        Some (Type.parse_app_term env ast T.Const._false args)
      | Type.Id id when Id.equal id Id.tptp_role ->
        Some (Type.Tags [])
      | _ -> None

  end

end

(** Smtlib builtins ($i, $o, etc..) *)
(* ************************************************************************ *)

module Smtlib = struct

  module type Tag = sig

    type 'a t

    val rwrt : unit t

  end

  module type Ty = sig

    module Const : sig

      type t

      val prop : t

    end

  end

  module type T = sig

    type t

    val eqs : t list -> t

    module Const : sig

      type t

      val _true : t

      val _false : t

    end

  end

  module Tff
      (Type : Tff_intf.S)
      (Tag : Tag with type 'a t = 'a Type.Tag.t)
      (Ty : Ty with type Const.t = Type.Ty.Const.t)
      (T : T with type t = Type.T.t
              and type Const.t = Type.T.Const.t) = struct

    let parse_f env ast cstr args =
      let loc = Term.(ast.loc) in
      let t = Term.apply ?loc cstr args in
      Type.Term (Type.parse_term env t)

    let parse env ast s args =
      match s with
      (* Boolean operators *)
      | Type.Id { Id.name = "Bool"; ns = Id.Sort } ->
        Some (Type.parse_app_ty env ast Ty.Const.prop args)
      | Type.Id { Id.name = "true"; ns = Id.Term } ->
        Some (Type.parse_app_term env ast T.Const._true args)
      | Type.Id { Id.name = "false"; ns = Id.Term } ->
        Some (Type.parse_app_term env ast T.Const._false args)
      | Type.Id { Id.name = "not"; ns = Id.Term } ->
        Some (parse_f env ast (Term.not_t ()) args)
      | Type.Id { Id.name = "and"; ns = Id.Term } ->
        Some (parse_f env ast (Term.and_t ()) args)
      | Type.Id { Id.name = "or"; ns = Id.Term } ->
        Some (parse_f env ast (Term.or_t ()) args)
      | Type.Id { Id.name = "xor"; ns = Id.Term } ->
        Some (parse_f env ast (Term.xor_t ()) args)
      | Type.Id { Id.name = "=>"; ns = Id.Term } ->
        Some (parse_f env ast (Term.implies_t ()) args)
      | Type.Id { Id.name = "ite"; ns = Id.Term } ->
        begin match args with
          | [c; a; b] ->
            let loc = ast.Term.loc in
            let ast = Term.ite ?loc c a b in
            Some (Type.Term (Type.parse_term env ast))
          | _ ->
            let err = Type.Bad_op_arity ("ite", 3, List.length args) in
            raise (Type.Typing_error (err, env, ast))
        end

      (* Equality *)
      | Type.Id { Id.name = "distinct"; ns = Id.Term } ->
        Some (parse_f env ast (Term.neq_t ()) args)
      | Type.Id { Id.name = "="; ns = Id.Term } ->
        let l = List.map (Type.parse_term env) args in
        Some (Type.Term (T.eqs l))

      (* Rewrite rules *)
      | Type.Id id when Id.equal id Id.rwrt_rule ->
        Some (Type.Tags [Type.Any (Tag.rwrt, ())])

      | _ -> None

  end

end
