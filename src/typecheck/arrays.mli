
(** Smtlib array builtins *)
module Smtlib : sig

  module type Ty = sig

    type t
    (** The type of types *)

    val array : t -> t -> t
    (** [array src dst] createe a functionnal array type,
        from the source type [src] to the destination type [dst].*)

  end

  module type T = sig

    type t
    (** The type of terms *)

    val select : t -> t -> t
    (** [select arr idx] creates the get operation on functionnal
        array [arr] for index [idx]. *)

    val store : t -> t -> t -> t
    (** [store arr idx value] creates the set operation on
        functional array [arr] for value [value] at index [idx]. *)

  end

  module Tff
      (Type : Tff_intf.S)
      (Ty : Ty with type t = Type.Ty.t)
      (T : T with type t = Type.T.t) : sig

    val parse : Type.builtin_symbols
  end

end
