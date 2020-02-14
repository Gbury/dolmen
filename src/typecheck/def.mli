
(** Definitions

    Helpful helpers to handle defined symbols.
*)

(** Handle definitions as declaring new constants. *)
module Declare(Type : Tff_intf.S) : sig

  val define_ty :
    Dolmen.Id.t -> Type.Ty.Var.t list -> Type.Ty.t -> unit
  (** Define a type constant. *)

  val define_term :
    Dolmen.Id.t -> Type.Ty.Var.t list -> Type.T.Var.t list -> Type.T.t -> unit
  (** Define a term constant. *)

  val parse : Type.builtin_symbols
  (** Adequate builtin symbols function for constants defined/declared. *)

end
