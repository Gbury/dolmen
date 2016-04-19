
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Conjonctive normal forms.
    For input languages suchs as dimacs. *)

type term = Term.t
(** The type of terms *)

type t =
  | Cnf of ParseLocation.t option * term list (** A cnf, with optional position. *)
(** The type of clauses. A clause is a list of literals. *)

let clause ?loc l = Cnf (loc, l)
(** Create a new clause. *)

