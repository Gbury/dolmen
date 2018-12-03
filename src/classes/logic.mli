
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Logic languages for formal proofs *)

module Make
    (L : Dolmen_intf.Location.S)
    (I : Dolmen_intf.Id.Logic)
    (T : Dolmen_intf.Term.Logic with type location := L.t
                                 and type id := I.t)
    (S : Dolmen_intf.Stmt.Logic with type location := L.t
                                 and type id := I.t
                                 and type term := T.t): sig

  exception Extension_not_found of string
  (** Raised when trying to find a language given a file extension. *)

  (** {2 Supported languages} *)

  type language =
    | Dimacs
    (** Dimacs CNF format *)
    | ICNF
    (** iCNF format *)
    | Smtlib
    (** Smtlib format *)
    | Tptp
    (** TPTP format (including THF) *)
    | Zf
    (** Zipperposition format *)
  (** The languages supported by the Logic class. *)

  val enum : (string * language) list
  (** Enumeration of languages together with an appropriate
      name. Can be used for command-line arguments (for instance,
      with cmdliner). *)

  val string_of_language : language -> string
  (** String representation of the variant *)

  (** {2 High-level parsing} *)

  val find :
    ?language:language ->
    ?dir:string -> string -> string option
  (** Tries and find the given file, using the language specification. *)

  val parse_file :
    ?language:language ->
    string -> language * S.t list
  (** Given a filename, parse the file, and return the detected language
      together with the list of statements parsed.
      @param language specify a language; overrides auto-detection. *)

  val parse_input :
    ?language:language ->
    [ `File of string | `Stdin of language ] ->
    language * (unit -> S.t option) * (unit -> unit)
  (** Incremental parsing of either a file (see {!parse_file}), or stdin
      (with given language). Returns a triplet [(lan, gen, cl)], containing
      the language detexted [lan], a genratro function [gen] for parsing the input,
      and a cleanup function [cl] to call in order to cleanup the file descriptors.
      @param language specify a language for parsing, overrides auto-detection
      and stdin specification. *)

  (** {2 Mid-level parsing} *)

  module type S = Dolmen_intf.Language.S with type statement := S.t
  (** The type of language modules. *)

  val of_language   : language  -> language * string * (module S)
  val of_extension  : string    -> language * string * (module S)
  val of_filename   : string    -> language * string * (module S)
  (** These function take as argument either a language, a filename,
      or an extension, and return a triple:
      - language
      - language file extension (starting with a dot)
      - appropriate parsing module

      Extensions should start with a dot (for instance : [".smt2"])
      @raise Extension_not_found when the extension is not recognized.
  *)

end
