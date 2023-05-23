
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions & modules *)
(* ************************************************************************* *)

exception Multiple_definition_of_var of Dolmen.Std.Expr.Term.Var.t
exception Multiple_definition_of_cst of Dolmen.Std.Expr.Term.Const.t

exception Partial_interpretation of
    Dolmen.Std.Expr.Term.Const.t * Value.t list
exception Incorrect_extension of
    Dolmen.Std.Expr.Term.Const.t * Value.t list * Value.t

module V = Map.Make(Dolmen.Std.Expr.Term.Var)
module C = Map.Make(Dolmen.Std.Expr.Term.Const)

type t = {
  vars : Value.t V.t;
  csts : Value.t C.t;
}


(* Common functions *)
(* ************************************************************************* *)

let empty =
  { vars = V.empty; csts = C.empty; }

let vars { vars; _ } = vars
let csts { csts; _ } = csts

let print fmt model =
  Format.fprintf fmt "@[<hv>@[<hv 2>{";
  V.iter (fun var value ->
      Format.fprintf fmt "@ %a -> @[<hov 2>%a@]"
        Dolmen.Std.Expr.Term.Var.print var Value.print value
    ) model.vars;
  C.iter (fun cst value ->
      Format.fprintf fmt "@ %a -> @[<hov 2>%a@]"
        Dolmen.Std.Expr.Term.Const.print cst Value.print value
    ) model.csts;
  Format.fprintf fmt "@]@ }@]"

let disjoint_union m m' =
  let vars =
    V.union
      (fun v _ _ -> raise (Multiple_definition_of_var v))
      m.vars m'.vars
  in
  let csts =
    C.union
      (fun c _ _ -> raise (Multiple_definition_of_cst c))
      m.csts m'.csts
  in
  { vars; csts; }


(* Mapped var&cst values *)
(* ************************************************************************* *)

module type S = sig

  type key

  val find_opt : key -> t -> Value.t option

  val add : key -> Value.t -> t -> t

  val remove : key -> t -> t

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

  let remove v t =
    { t with vars = V.remove v t.vars; }

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

  let remove c t =
    { t with csts = C.remove c t.csts; }

end
