
open Dolmen

(* Smtlib arithmetic (integer and reals) *)
(* ************************************************************************ *)

module Smtlib = struct

  module Int = struct

    module type Ty = sig
      type t
      val int : t
    end

    module type T = sig
      type t
      val int : string -> t
      val neg : t -> t
      val add : t -> t -> t
      val sub : t -> t -> t
      val mul : t -> t -> t
      val div : t -> t -> t
      val modulo : t -> t -> t
      val abs : t -> t
      val lt : t -> t -> t
      val le : t -> t -> t
      val gt : t -> t -> t
      val ge : t -> t -> t
      val divisible : string -> t -> t
    end

    module Tff
        (Type : Tff_intf.S)
        (Ty : Ty with type t = Type.Ty.t)
        (T : T with type t = Type.T.t) = struct

      let app1 env ast args name mk =
        Base.make_op1 (module Type) env ast name args
          (fun t -> Type.Term (mk (Type.parse_term env t)))

      let app2 env ast args name mk =
        Base.make_op2 (module Type) env ast name args
          (fun (a, b) ->
             Type.Term (mk (Type.parse_term env a) (Type.parse_term env b)))

      let app_left env ast args name mk =
        Base.make_assoc (module Type) env ast name args
          (fun l -> Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env ast args name mk =
        Base.make_chain (module Type) env ast name args (fun l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let split_id = Dolmen_std.Misc.split_on_char '\000'

      let parse env ast s args =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          Base.make_op0 (module Type) env ast "Int" args
            (fun () -> Type.Ty Ty.int)
        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.int name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name, args with
            | "-", [x] ->
              Some (Type.Term (T.neg (Type.parse_term env x)))
            | "-", _ -> app_left env ast args "-" T.sub
            | "+", _ -> app_left env ast args "+" T.add
            | "*", _ -> app_left env ast args "*" T.mul
            | "div", _ -> app_left env ast args "div" T.div
            | "mod", _ -> app2 env ast args "mod" T.modulo
            | "abs", _ -> app1 env ast args "abs" T.abs
            | "<=", _ -> app_chain env ast args "<=" T.le
            | "<", _ -> app_chain env ast args "<" T.lt
            | ">=", _ -> app_chain env ast args ">=" T.ge
            | ">", _ -> app_chain env ast args ">" T.gt
            | _ -> begin match split_id name with
                | "divisible" :: r ->
                  begin match r with
                    | [n] ->
                      Base.make_op1 (module Type) env ast "divisible" args
                        (fun t -> Type.Term (T.divisible n (Type.parse_term env t)))
                    | _ ->
                      let err = Type.Bad_op_arity ("divisible", 1, List.length r) in
                      raise (Type.Typing_error (err, env, ast))
                  end
                | _ -> None
              end
          end
        | _ -> None

    end
  end

  module Real = struct

    module type Ty = sig
      type t
      val real : t
    end

    module type T = sig
      type t
      val real : string -> t
      val neg : t -> t
      val add : t -> t -> t
      val sub : t -> t -> t
      val mul : t -> t -> t
      val div : t -> t -> t
      val lt : t -> t -> t
      val le : t -> t -> t
      val gt : t -> t -> t
      val ge : t -> t -> t
    end

    module Tff
        (Type : Tff_intf.S)
        (Ty : Ty with type t = Type.Ty.t)
        (T : T with type t = Type.T.t) = struct

      let app_left env ast args name mk =
        Base.make_assoc (module Type) env ast name args
          (fun l -> Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env ast args name mk =
        Base.make_chain (module Type) env ast name args (fun l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let parse env ast s args =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          Base.make_op0 (module Type) env ast "Real" args
            (fun () -> Type.Ty Ty.real)
        (* values *)
        | Type.Id { Id.ns = Id.Value (Id.Integer | Id.Real); name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.real name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name, args with
            | "-", [x] ->
              Some (Type.Term (T.neg (Type.parse_term env x)))
            | "-", _ -> app_left env ast args "-" T.sub
            | "+", _ -> app_left env ast args "+" T.add
            | "*", _ -> app_left env ast args "*" T.mul
            | "/", _ -> app_left env ast args "/" T.div
            | "<=", _ -> app_chain env ast args "<=" T.le
            | "<", _ -> app_chain env ast args "<" T.lt
            | ">=", _ -> app_chain env ast args ">=" T.ge
            | ">", _ -> app_chain env ast args ">" T.gt
            | _ -> None
          end
        | _ -> None

    end
  end

  module Real_Int = struct

    module type Ty = sig
      type t
      val int : t
      val real : t
      val equal : t -> t -> bool
    end

    module type T = sig
      type t
      type ty

      val ty : t -> ty

      val int : string -> t
      val real : string -> t

      module Int : sig
        val neg : t -> t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val modulo : t -> t -> t
        val abs : t -> t
        val lt : t -> t -> t
        val le : t -> t -> t
        val gt : t -> t -> t
        val ge : t -> t -> t
        val divisible : string -> t -> t
        val to_real : t -> t
      end

      module Real : sig
        val neg : t -> t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val lt : t -> t -> t
        val le : t -> t -> t
        val gt : t -> t -> t
        val ge : t -> t -> t
        val is_int : t -> t
        val to_int : t -> t
      end

    end

    module Tff
        (Type : Tff_intf.S)
        (Ty : Ty with type t = Type.Ty.t)
        (T : T with type t = Type.T.t and type ty := Type.Ty.t) = struct

      type Type.err +=
        | Expected_arith_type of Ty.t

      let dispatch1 env ast (mk_int, mk_real) t =
        let ty = T.ty t in
        if Ty.(equal int) ty then mk_int t
        else if Ty.(equal real) ty then mk_real t
        else begin
          let err = Expected_arith_type ty in
          raise (Type.Typing_error (err, env, ast))
        end

      let dispatch2 env ast (mk_int, mk_real) a b =
        let a_ty = T.ty a in
        let b_ty = T.ty b in
        if Ty.(equal int) a_ty then
          if Ty.(equal real) b_ty then
            mk_real (T.Int.to_real a) b
          else
            mk_int a b
        else if Ty.(equal real) a_ty then
          if Ty.(equal int) b_ty then
            mk_real a (T.Int.to_real b)
          else
            mk_real a b
        else begin
          let err = Expected_arith_type a_ty in
          raise (Type.Typing_error (err, env, ast))
        end

      let app1 env ast args name mk =
        Base.make_op1 (module Type) env ast name args
        (fun t -> Type.Term (mk (Type.parse_term env t)))

      let app2 env ast args name mk =
        Base.make_op2 (module Type) env ast name args
          (fun (a, b) ->
             Type.Term (mk (Type.parse_term env a) (Type.parse_term env b)))

      let app_left env ast args name mk =
        Base.make_assoc (module Type) env ast name args
          (fun l -> Type.Term (Base.fold_left_assoc mk (List.map (Type.parse_term env) l)))

      let app_chain env ast args name mk =
        Base.make_chain (module Type) env ast name args (fun l ->
            let l' = List.map (Type.parse_term env) l in
            Type.Term (Base.map_chain (module Type) mk l')
          )

      let split_id = Dolmen_std.Misc.split_on_char '\000'

      let parse env ast s args =
        match s with
        (* type *)
        | Type.Id { Id.ns = Id.Sort; name = "Int"; } ->
          Base.make_op0 (module Type) env ast "Int" args
            (fun () -> Type.Ty Ty.int)
        | Type.Id { Id.ns = Id.Sort; name = "Real"; } ->
          Base.make_op0 (module Type) env ast "Real" args
            (fun () -> Type.Ty Ty.real)
        (* values *)
        | Type.Id { Id.ns = Id.Value Id.Integer; name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.int name))
        | Type.Id { Id.ns = Id.Value Id.Real; name; } ->
          Base.make_op0 (module Type) env ast name args
            (fun () -> Type.Term (T.real name))
        (* terms *)
        | Type.Id { Id.ns = Id.Term; name; } ->
          begin match name, args with
            | "-", [_] ->
              app1 env ast args "-"
                (dispatch1 env ast (T.Int.neg, T.Real.neg))
            | "-", _ ->
              app_left env ast args "-"
                (dispatch2 env ast (T.Int.sub, T.Real.sub))
            | "+", _ ->
              app_left env ast args "+"
                (dispatch2 env ast (T.Int.add, T.Real.add))
            | "*", _ ->
              app_left env ast args "*"
                (dispatch2 env ast (T.Int.mul, T.Real.mul))
            | "div", _ -> app_left env ast args "div" T.Int.div
            | "mod", _ -> app2 env ast args "mod" T.Int.modulo
            | "abs", _ -> app1 env ast args "abs" T.Int.abs
            | "/", _ -> app_left env ast args "/" T.Real.div
            | "<=", _ ->
              app_chain env ast args "<="
                (dispatch2 env ast (T.Int.le, T.Real.le))
            | "<", _ ->
              app_chain env ast args "<"
                (dispatch2 env ast (T.Int.lt, T.Real.lt))
            | ">=", _ ->
              app_chain env ast args ">="
                (dispatch2 env ast (T.Int.ge, T.Real.ge))
            | ">", _ ->
              app_chain env ast args ">"
                (dispatch2 env ast (T.Int.gt, T.Real.gt))
            | _ -> begin match split_id name with
                | "divisible" :: r ->
                  begin match r with
                    | [n] ->
                      Base.make_op1 (module Type) env ast "divisible" args
                        (fun t -> Type.Term (T.Int.divisible n (Type.parse_term env t)))
                    | _ ->
                      let err = Type.Bad_op_arity ("divisible", 1, List.length r) in
                      raise (Type.Typing_error (err, env, ast))
                  end
                | _ -> None
              end
          end
        | _ -> None

    end

  end

end

