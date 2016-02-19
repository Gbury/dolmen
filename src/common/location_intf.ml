
module type S = sig

  type t

  exception Uncaught of t * exn
  exception Lexing_error of t * string
  exception Syntax_error of t * string

  val mk_pos : Lexing.position -> Lexing.position -> t

  val of_lexbuf : Lexing.lexbuf -> t

end

