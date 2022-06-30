# Dolmen Tutorial

## Introduction

Dolmen's goal is to provide programmers with flexible parsers for input languages.
Currently, Dolmen supports languages for automated theorem proving such as tptp
and smtlib.

## Quick start

If you're looking for a quick way to parse languages for automated theorem provers
the following code will give you a quick start:

```ocaml

open Dolmen

(* Instantiate a module for parsing logic languages *)
module M = Class.Logic.Make(Std.Loc)(Std.Id)(Std.Term)(Std.Statement)

(* An arbitrary file *)
let file = "example.smt2"

(* Parse the file, and we get a tuple:
  - format: the guessed format (according to the file extension)
  - statements: the list of top-level firectives found in the file *)
let format, _, statements = M.parse_file file

(* You can match on the detected format of the input *)
let () = match format with
| M.Dimacs | M.ICNF -> Format.printf "Hurray for CNF !@."
| M.Alt_ergo | M.Smtlib2 _ | M.Tptp _ | M.Zf ->
  Format.printf "First (or higher) order formulas ! Yay !@."

(* Now you can analyse the statements, and prove the needed theorems *)
let () = List.iter ignore statements
```

## Global architecture

In order to stay flexible, Dolmen does not provide parsers, but rather functors
that generate parsers. The functors are there to abstract over the
implementation of the input language. For instance, in the context of languages
for theorem proving, the parsers abstract over the representation of terms
and formulas, so that programmers can use their own AST rather than being forced
to use the library's.

## Parsing a single language

Let's say you want a parser for a specific language, for instance TPTP.
The `Tptp` module provides the functor to generate tptp parsers.
It provides a functor of the following type:

```ocaml
module type S = sig
  module Make
    (L : Dolmen_intf.Location.S)
    (I : Dolmen.Tptp.Latest.Id)
    (T : Dolmen.Tptp.Latest.Term with type location := L.t
                                  and type id := I.t)
    (S : Dolmen.Tptp.Latest.Statement with type location := L.t
                                       and type id := I.t
                                       and type term := T.t) :
  Dolmen.Intf.Language.S with type statement = S.t
end
```

The functor takes four modules, of signatures:
- `Dolmen_intf.Location.s`:
  An implementation of locations in files, the locations are used
  for two things:
  - attaching locations to terms and formulas
  - reporting errors during parsing
- `Dolmen.Tptp.Latest.Id`:
  An implementation of identifiers. Identifiers are used to distinguish lexical
  scopes, for instance the scope of terms constants and the scope of
  declaration names in tptp.
- `Dolmen.Tptp.Latest.Term`:
  An implementation of terms (and formulas). It provides an abstract type `t`
  for expressions as well as functions to build these expressions (conjunction
  of formulas, universal quantification, etc...)
- `Dolmen.Tptp.Latest.Statement`:
  An implementation of top-level directives found in tptp files.

Once provided with the required modules, the functor will return a module
with the following interface:

```ocaml
(** file: src/interface/language_intf.ml *)
module type S = sig

  type token
  (** The type of tokens produced by the language lexer. *)

  type statement
  (** The type of top-level directives recognised by the parser. *)

  module Lexer : Dolmen.Intf.Lex.S
    with type token := token
  (** The Lexer module for the language. *)

  module Parser : Dolmen.Intf.Parse.S
    with type token := token
     and type statement := statement
  (** The Parser module for the language. *)

  val parse_file : string -> statement list
  (** Parse the given file *)

  val parse_input :
    [ `Stdin | `File of string ] -> (unit -> statement option)
  (** Incremental parsing. Given an input to read (either a file, or stdin),
      returns a generator that will incrementally parse the statements.
      Useful to process input from [stdin], or even large files where it would
      be impractical to parse the entire file before processing it. *)

end
```

This module lets you access the actual lexer and parser used to parse the tptp
language, for also provides you with high-level functions to quickly parse an
input file, or the standard input.

## Terms and Statements

Given the broad scope of some languages, the requirements for terms implementation
can sometimes be far from trivial; for instance, the tptp language specification includes
THF, which describes higher-order terms, and comes with quite a lot of builtin
quantifiers such as definite and indefinite description.

While great care is taken to document the interface requirements of each language
(usually found in the `Ast` module of the sub-library `dolmen_foobar` for language foobar),
it can be bothersome for users to have to implement all the required functions,
so Dolmen provides default implementation of functor arguments.

The modules `Dolmen.Std.Loc`, `Dolmen.Std.Id`, `Dolmen.Std.Term`,
`Dolmen.Std.Statement` implement all interfaces required by the functors in the
library, and can be used to instantiate any functor.

## The Logic class

Having parsers for languages is a good start, but most of the time you'd like
to parse all languages that make sens for your use-case, for instance in the
case of an automated theorem prover, you might want to parse all languages
that describe logic formulas.

That is precisely the goal of the `Logic` module that parses the languages
suited to be input for theorem proving. It provides a functor, which basically
takes the same arguments as the single-language parsers, but with richer
interfaces:

```ocaml
(** file: src/classes/logic.mli *)
module type S = sig
  module Make
    (L : Dolmen.Intf.Location.S)
    (I : Dolmen.Intf.Id.Logic)
    (T : Dolmen.Intf.Term.Logic with type location := L.t
                                 and type id := I.t)
    (S : Dolmen.Intf.Stmt.Logic with type location := L.t
                                 and type id := I.t
                                 and type term := T.t): sig

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

  (** {2 High-level parsing} *)

  val parse_file : string -> language * S.t list
  (** Given a filename, parse the file, and return the detected language
      together with the list of statements parsed. *)

  val parse_input :
    [ `File of string | `Stdin of language ] ->
    language * (unit -> S.t option)
  (** Incremental parsing of either a file, or stdin. *)

  (** {2 Mid-level parsing} *)

  module type S = Dolmen.Intf.Language.S with type statement := S.t
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
end
```

This allows to easily parse many input languages, and also to automatically
benefit from new languages added to the library without having to manually
instantiate the new functors in one's project.

## Documentation

Now that you have a grasp on how Dolmen works, take a look at the documentation at
<http://gbury.github.io/dolmen>


