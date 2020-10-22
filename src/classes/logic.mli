
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Logic languages for formal proofs *)

module type S = sig

  type file
  (** File location meta-data. *)

  type statement
  (** The type of statements. *)

  exception Extension_not_found of string
  (** Raised when trying to find a language given a file extension. *)

  (** {2 Supported languages} *)

  type language =
    | Alt_ergo
    (** Alt-ergo's native language *)
    | Dimacs
    (** Dimacs CNF format *)
    | ICNF
    (** iCNF format *)
    | Smtlib2 of Dolmen_smtlib2.version
    (** Smtlib v2 latest version *)
    | Tptp of Dolmen_tptp.version
    (** TPTP format (including THF), latest version *)
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
    string -> language * file * statement list
  (** Given a filename, parse the file, and return the detected language
      together with the list of statements parsed.
      @param language specify a language; overrides auto-detection. *)

  val parse_file_lazy :
    ?language:language ->
    string -> language * file * statement list Lazy.t
  (** Given a filename, parse the file, and return the detected language
      together with the list of statements parsed.
      @param language specify a language; overrides auto-detection. *)

  val parse_input :
    ?language:language ->
    [< `File of string
    | `Stdin of language
    | `Raw of string * language * string ] ->
    language * file * (unit -> statement option) * (unit -> unit)
  (** Incremental parsing of either a file (see {!parse_file}), stdin
      (with given language), or some arbitrary contents, of the form
      [`Raw (filename, language, contents)].
      Returns a triplet [(lan, gen, cl)], containing
      the language detexted [lan], a genratro function [gen] for parsing the input,
      and a cleanup function [cl] to call in order to cleanup the file descriptors.
      @param language specify a language for parsing, overrides auto-detection
      and stdin specification. *)

  (** {2 Mid-level parsing} *)

  module type S = Dolmen_intf.Language.S
    with type statement := statement
     and type file := file
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

module Make
    (L : Dolmen_intf.Location.S)
    (I : Dolmen_intf.Id.Logic)
    (T : Dolmen_intf.Term.Logic with type location := L.t
                                 and type id := I.t)
    (S : Dolmen_intf.Stmt.Logic with type location := L.t
                                 and type id := I.t
                                 and type term := T.t)
  : S with type statement := S.t and type file := L.file

