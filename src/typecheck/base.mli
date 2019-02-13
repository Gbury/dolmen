
(** TPTP builtins ($i, $o, etc..) *)
module Tptp : sig

  (** Type constants required to typecheck tptp builtins *)
  module type Ty = sig
    type t
    (** The type of types *)
    val prop : t
    (** The type constructor of propositions. *)
    module Const : sig
      type t
      (** The type of type constants (i.e. type constructors) *)
      val base : t
      (** An arbitrary already existing type constructor *)
    end
  end

  (** Term constants, aka function symbols. *)
  module type T = sig
    module Const : sig
      type t
      (** The type of function symbols *)
      val _true : t
      (** The smybol for [true] *)
      val _false : t
      (** The symbol for [false] *)
    end
  end

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Ty : Ty with type t = Type.Ty.t
                and type Const.t = Type.Ty.Const.t)
      (T : T with type Const.t = Type.T.Const.t) : sig

    val parse : Type.builtin_symbols

  end

end


(** Smtlib builtin *)
module Smtlib : sig

  (** Tags *)
  module type Tag = sig
    type 'a t
    (** Polymorphic tags *)
    val rwrt : unit t
    (** A flag (i.e. unit tag), indicatgin that the tagged term/formula
        is to be considered as a rewrite rule. *)
  end

  (** Type constants required to typecheck tptp builtins *)
  module type Ty = sig
    type t
    (** The type of type constants (i.e. type constructors) *)
    val prop : t
    (** The type constructor of propositions. *)
  end

  (** Terms *)
  module type T = sig
    type t
    (** The type fo terms. *)
    val eqs : t list -> t
    (** Create a chain of equalities. *)

    (** Term constants, aka function symbols. *)
    module Const : sig
      type t
      (** The type of function symbols *)
      val _true : t
      (** The smybol for [true] *)
      val _false : t
      (** The symbol for [false] *)
    end
  end

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Tag with type 'a t = 'a Type.Tag.t)
      (Ty : Ty with type t = Type.Ty.t)
      (T : T with type t = Type.T.t
              and type Const.t = Type.T.Const.t) : sig

    val parse : Type.builtin_symbols
  end
end

(** Zf builtins *)
module Zf : sig

  (** Tags *)
  module type Tag = sig
    type 'a t
    (** Polymorphic tags *)
    val rwrt : unit t
    (** A flag (i.e. unit tag), indicatgin that the tagged term/formula
        is to be considered as a rewrite rule. *)

    type name
    (** A type used to specify a name for printing identifiers *)
    val name : name t
    (** A tag used to specify what to print when printing an identifier *)
    val exact : string -> name
    (** Print the identifier with this exact string. *)

    type pos
    (** A type to indicate how to print identifiers *)
    val pos : pos t
    (** A tag to specify how to print identifiers*)
    val infix : pos
    (** The tagged identifier is an infix symbol *)
    val prefix : pos
    (** The tagged identifier is a prefix symbol *)
  end

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Tag with type 'a t = 'a Type.Tag.t) : sig

    val parse : Type.builtin_symbols
  end
end
