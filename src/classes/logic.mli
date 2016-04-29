
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Logic languages for formal proofs *)

module Make
    (L : ParseLocation.S)
    (T : Term_intf.Logic with type location := L.t)
    (S : Stmt_intf.Logic with type location := L.t and type term := T.t): sig

  (** {2 Supported languages} *)

  type language =
    | Dimacs
    (** Dimacs CNF format *)
    | Smtlib
    (** Smtlib format *)
    | Tptp
    (** TPTP format (including THF) *)
    | Zf
    (** Zipperposition format *)
  (** The languages supported by the Logic class. *)

  (** {2 High-level parsing} *)

  val parse_file : string -> language * S.t list
  (** Given a filename, parse the file, and return the detected language
      together with the list of statements parsed. *)

  val parse_input :
    [ `File of string | `Stdin of language ] ->
    language * (unit -> S.t option)
  (** Incremental parsing of either a file, or stdin. *)

  (** {2 Mid-level parsing} *)

  module type S = Language_intf.S with type statement := S.t
  (** The type of language modules. *)

  val of_language : language -> language * string * (module S)
  val of_extension : string -> language * string * (module S)
  (** These function take as argument either a language, or a filename,
      and return a triple:
      - language
      - language file extension (starting with a dot)
      - appropriate parsing module
  *)

end
