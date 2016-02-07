

module type Ty = sig

  type t

end

module type Term = sig

  type t
  type ty

end

module type Formula = sig

  type t
  type ty
  type term

end

module type Phrase = sig

  type t
  type role

  val fof : string * role * Formula.t -> t

end

