
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Make
    (L : ParseLocation.S)
    (T : Term_intf.Full with type location := L.t)
    (S : Stmt_intf.Full with type location := L.t and type term := T.t): sig

  type language =
    | Dimacs
    | Smtlib
    | Tptp
    | Zf

  module type S = Language_intf.S with type statement := S.t

  val of_language : language -> language * string * (module S)
  val of_extension : string -> language * string * (module S)

  val parse_file : string -> language * S.t list

  val parse_input :
    [ `File of string | `Stdin of language ] ->
    language * (unit -> S.t option)

end
