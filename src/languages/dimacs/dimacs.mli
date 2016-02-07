
module type Ast = Ast_dimacs.S

module Make(L : ParseLocation.S)(T : Ast) : sig

  val parse_file : string -> T.clause list

end

