# Dolmen Tutorial

## Introduction

Dolmen's goal is to provide programmers with flexible parsers for input languages.
Currently, Dolmen supports languages for automated theorem proving such as tptp
and smtlib.

## Quick start

If you're looking for a quick way to parse languages for automated theorem provers
the following code will give you a quick start:

<!-- $MDX env=e1 -->
```ocaml
open Dolmen

(* Instantiate a module for parsing logic languages *)
module Logic = Class.Logic.Make(Std.Loc)(Std.Id)(Std.Term)(Std.Statement)

(* instantiate the modules for typechecking *)
module State = Dolmen_loop.State
module Typer_aux = Dolmen_loop.Typer.Typer(State)
module Typer = Dolmen_loop.Typer.Make(Std.Expr)(Std.Expr.Print)(State)(Typer_aux)

let test file =

  (* *** Parsing ********************************************************** *)


  (* Parse the file, and we get a tuple:
    - format: the guessed format (according to the file extension)
    - loc: some meta-data used for source file locations
    - statements: the list of top-level directives found in the file *)
  let format, loc, parsed_statements = Logic.parse_file file in

  (* You can match on the detected format of the input *)
  let () =
    match format with
    | Logic.Dimacs | Logic.ICNF -> ()
    | Logic.Alt_ergo | Logic.Smtlib2 _ | Logic.Tptp _ | Logic.Zf -> ()
  in

  (* *** Typing *********************************************************** *)

  (* Typing errors have a retcode associated to them, so that any typing
     error results in *)

  (* create the logic file corresponding to our input *)
  let lang : Dolmen_loop.Logic.language = Smtlib2 `Latest in
  let logic_file = State.mk_file ~lang ~loc "./" (`File file) in
  let response_file = State.mk_file "" (`File "this is unused") in

  (* let's create the initial state *)
  let state =
    State.empty
    |> State.init
       ~debug:false ~report_style:Contextual ~max_warn:max_int
       ~reports:(Dolmen_loop.Report.Conf.mk ~default:Enabled)
       ~response_file
       (* these limits are ignored in this example; to actually enforce
          the limits, one has to use the `run` function from `Dolmen_loop.Pipeline` *)
       ~time_limit:0. ~size_limit:0.
    |> State.set State.logic_file logic_file
    |> Typer_aux.init
    |> Typer.init ~type_check:true
  in

  (* We can loop over the parsed statements to generated the typed statements *)
  let final_state, rev_typed_stmts =
    List.fold_left (fun (state, acc) parsed_stmt ->
      let state, typed_stmt = Typer.check state parsed_stmt in
      (state, typed_stmt :: acc)
    ) (state, []) parsed_statements
  in
  let typed_stmts = List.rev rev_typed_stmts in

  (* let's print the typed statements *)
  List.iter (fun typed_stmt ->
    Format.printf "%a@\n@." Typer.print typed_stmt
  ) typed_stmts
```

We can now use our function to see what happens on various files:

<!-- $MDX env=e1 -->
```ocaml
# test "example.smt2";;
other_1[0-15]:
  set-logic: LIA =
    { theories: int, core;
      features: { free_sorts : false;
                  free_functions : false;
                  datatypes : false;
                  quantifiers : true;
                  arithmetic : linear/strict;
                  arrays : all; }; }]}

decl_1[16-37]:
  decls:
    term-decl: x : int

hyp_1[38-60]:
  hyp: 2 = (1 + x)

prove_1[61-72]:
  solve:  assuming:

- : unit = ()
# (* on errors, the default behaviour of Dolmen_loop.State is to print the error
     message, and then exit with the retcode of the error. You can customize this
     behaviour by defining your own [State] module, and particularly the [erorr]
     function. Additionally, since we didn't set a retcode for typing errors,
     we get a [Failure _] exception here. *)
  test "typing_error.smt2";;
File "typing_error.smt2", line 3, character 13-20:
3 | (assert (= 2 (* x x)))
                 ^^^^^^^
Error Non-linear expressions are forbidden by the logic.
Hint: multiplication in strict linear arithmetic expects an integer or
  rational literal and a symbol (variable or constant) but was given:
  - a symbol (or quantified variable)
  - a symbol (or quantified variable)
Exception: Failure "missing retcode".
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


