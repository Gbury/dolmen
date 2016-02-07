
module type S = sig

  type atom
  type clause

  val mk_atom : string -> atom
  val mk_clause : atom list -> clause

end

