
(** Smtlib array builtins *)
module Smtlib : sig

  module type Ty = sig
    type t
    (** The type of types *)
    val mk_array : t -> t -> t
    (** [mk_array src dst] createe a functionnal array type,
        from the source type [src] to the destination type [dst].*)
  end

  module type T = sig
    type t
    (** The type of terms *)
    val mk_select : t -> t -> t
    (** [mk_select arr idx] creates the get operation on functionnal
        array [arr] for index [idx]. *)
    val mk_store : t -> t -> t -> t
    (** [mk_store arr idx value] creates the set operation on
        functional array [arr] for value [value] at index [idx]. *)
  end

  module Tff
      (Type : Tff_intf.S)
      (Ty : Ty with type t = Type.Ty.t)
      (T : T with type t = Type.T.t) : sig

    val parse : Type.builtin_symbols
  end

end
