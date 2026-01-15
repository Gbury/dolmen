
# The Dolmen Parsing library

## Architecture

This library provides functors to create parsers from a given
representation of terms. This allows users to have a parser that
directly outputs terms in the desired form, without needing to
translate the parser output into the specific AST used in a project.

Parsers (actually, the functors which generate parsers) typically take
four module arguments:

- A representation of locations in files. This is used for reporting
  parsing and lexing errors, but also to attach to each expression parsed
  its location.
- A representation of identifiers. In some languages, there are syntactic
  scopes, which are handled using namespaces for variable names. In order
  not to pollute the Term module with them, the namespaces are dealt with
  in this module separately.
- A representation of terms. The functions in this module are used by the
  parser to build the various types, terms and formulas corresponding
  to the grammar of the input language. All functions of this module
  typically take as their first (optional) argument a location (of the type
  defined by the previous argument) so that it is possible to have
  locations for expressions.
- A representation of top-level directives. Languages usually define
  several top-level directives to more easily distinguish type definitions,
  axioms, lemmas, theorems to prove, new assertions, or even sometimes direct
  commands for the solver (to set some options for instance). Again, the functions
  in this module usually have a first optional argument to set the location
  of the directives.

Some simple implementations of these modules are provided in this library.
See the next section for more information.

## Example

Examples of how to use the parsers can be found in src/main.ml. As mentioned
in the previous section, default implementations for the required functor arguments
are provided and can be used.

For instance, the following code instantiates a parser for the smtlib language
and tries to parse a file named "example.smt2" in the home of the current user:

```ocaml
module P =
  Dolmen.Smtlib2.Script.Latest.Make
    (Dolmen.Std.Loc)(Dolmen.Std.Id)(Dolmen.Std.Term)(Dolmen.Std.Statement)
    (Dolmen.Std.Extensions.Smtlib2)

let _, lazy_l = P.parse_all (`File "example.smt2")
let statements = Lazy.force lazy_l
```

For more examples, see the [tutorial](https://github.com/Gbury/dolmen/tree/master/doc/tuto.md).

