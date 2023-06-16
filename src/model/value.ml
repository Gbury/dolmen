
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module E = Dolmen.Std.Expr

(* Type definitions *)
(* ************************************************************************* *)

type 'a witness = ..
type any_witness = Any : _ witness -> any_witness [@@unboxed]

module type S = sig

  type t

  type _ witness += Val : t witness

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val abstract : (E.Term.Const.t -> t) option

end

type 'a ops = (module S with type t = 'a)
type any_ops = Ops : _ ops -> any_ops

type t = Value : 'a witness * 'a ops * 'a -> t

exception Extraction_failed of t * any_ops


(* Creating ops and values *)
(* ************************************************************************* *)

let ops (type a)
    ?(abstract : (E.Term.Const.t -> a) option)
    ~(compare : a -> a -> int)
    ~(print : Format.formatter -> a -> unit)
    () : a ops =
  let module M = struct
    type t = a
    type _ witness += Val : t witness
    let print = print
    let compare = compare
    let abstract = abstract
  end in
  (module M : S with type t = a)

let mk_ops = ops

let[@inline] mk (type a) ~(ops: a ops) (x : a) =
  let (module V) = ops in
  Value (V.Val, ops, x)

let dummy =
  let ops = ops ~compare ~print:(fun fmt () -> Format.fprintf fmt "()") () in
  mk ~ops ()

(* Std operations *)
(* ************************************************************************* *)

let print fmt t =
  let Value (_, (module V), x) = t in
  V.print fmt x

let compare t t' =
  let Value (w, (module V), x) = t in
  match t' with
  | Value (V.Val, _, y) -> (V.compare x y : int)
  | Value (w', _, _) -> Stdlib.compare (Any w) (Any w')

(* Sets/Maps *)
(* ************************************************************************* *)

module Aux = struct
  type aux = t
  type t = aux
  let compare = compare
end

module Set = Set.Make(Aux)
module Map = Map.Make(Aux)

(* Abstract values *)
(* ************************************************************************* *)

module Abstract = struct

  type t =
    | Cst of { cst : E.Term.Const.t; }

  let print fmt = function
    | Cst { cst } -> Format.fprintf fmt "%a" E.Term.Const.print cst

  let compare t t' =
    match t, t' with
    | Cst { cst = c; }, Cst { cst = c'; } -> E.Term.Const.compare c c'

  let ops = mk_ops ~compare ~print ()

end

let abstract_cst cst =
  mk ~ops:Abstract.ops (Cst { cst; })


(* Extracting values *)
(* ************************************************************************* *)

let[@inline] extract (type a) ~(ops: a ops) (t : t) : a option =
  let (module V) = ops in
  let (module A) = Abstract.ops in
  match t with
  | Value (V.Val, _, x) -> Some x
  | Value (A.Val, _, Cst { cst }) ->
    begin match V.abstract with
      | None -> None
      | Some f -> Some (f cst)
    end
  | _ -> None

let[@inline] extract_exn (type a) ~(ops: a ops) (t : t) : a =
  let (module V) = ops in
  let (module A) = Abstract.ops in
  match t with
  | Value (V.Val, _, x) -> x
  | Value (A.Val, _, Cst { cst }) ->
   begin match V.abstract with
     | Some f -> f cst
     | None -> raise (Extraction_failed (t, (Ops ops)))
   end
  | _ -> raise (Extraction_failed (t, (Ops ops)))

