
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interface for token descriptions
    This doe snot define an interface for token descriptions but rather defines
    a type for token description, that will then be used by others interfaces.
*)

type descr = {
  article : string;
  kind : string;
  lexeme : string;
  hint : string option;
}
(** The description fo a token, to be used when pretty-printing error messages.
    The idea is to be able to ouput a message such as
    "the parser expetced something, but got %{article} %{kind}: '%{lexeme}'" *)

