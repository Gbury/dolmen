
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Functor instantiation *)
(* ************************************************************************* *)

module Config = struct
  type token = Tokens.token
  let version = Dolmen_smtlib2_print.Print.V2_7
end

include Dolmen_smtlib2_print.Print.Make(Config)(Lexer)

