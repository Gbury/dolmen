
next
----

### Model verification

- Added model verification. This currently supports for all
  builtins (except for the String builtins).

### Parsing

- Added a parser for the smtlib model specification language
- Fix doc comments mentionning removed parameters
  (PR #107, issue #106)

### Typing

- Cleaned up handling of definitions: instead of using
  the functors in `Def`, definitions are now simply declared
  using the functions exposed by the typechecker

### Loop

- Changed the state type from a record to an heterogeneous
  map. This simplifies interfaces for all Loop modules,
  and makes it much more extensible.


v0.7
----

### UI

- Added source input snippet printing for errors and warnings
- Fix a bug affecting warning options (e.g.
  `dolmen --warn=+all` triggered an uncaught exception that is
  now fixed)

### Parsing

* Fix bug in SMTLIB syntax (v2.6 and poly), where the
  define-funs-rec syntax construction expected an open
  paren at the end instead of a closing paren

### Typing

- Complete the typing of alt-ergo's builtins
  PR#89
- Added exhaustivity and redundant pattern matching analysis
  (redundant patterns trigger a warning, whereas inexhaustive
  pattern matching trigger a typing error)
  part of PR#89
- Removed the typing of real and extended bitvector literals
  from the Float theory. These are not part of the FP
  specification, so it's better for Dolmen to be strict.
  Additionally, dependengin on the order of theories, they
  could shadow the proper typing of such literals and
  result in bogus warnings/errors
  PR#79 (see also Issue#43 Issue#74)
- Fixed the handling of the `reset` and `reset-assertions`
  commands of smtlibv2.6. Previsously reset was ignored,
  and reset-assertions was treated as reset (meaning that
  any set-logic were erased). These two commands should
  now be correctly implemented in the typing loop.
  PR#80
- Added a warning for multiple set-logics
  PR#82
- Added a hint to suggest a missing theory when a literal
  is unbound.
  PR#81


### API

- Added proper abstractions for names and paths.
  Names are used instead of strings for parsed
  identifiers (Id.t), while Paths are used instead of
  strings for typed identifiers (Expr.id).
  This results in a speedup on some smtlib problems
  because indexed identifiers no longer need to be
  encoded and then split.
- Added to Dolmen a custom implementation of Radix tries
  for a better indexation of strings. This results
  in signifcant speedup on large problem.
- Added some convenience modules for testing and profiling
  (Timer and Stats)
- The pipeline now delegates the task of printing backtraces
  for excpetions to the caller/finally argument of the run
  function
- the `Dolmen_loop` library now has an added dependency on
  `pp_loc` (used for the source input printing)
- updated version bounds on `cmdliner` and `pp_locs`


v0.6
----

### Release

- The official github release now provides access to
  already built binaries for `dolmen` and `dolmenls`,
  for linux (ubuntu) and macos
- The LSP server has been updated to depend on
  `linol~0.2`

### Bugfixes

- Smtlib2 let-bindings were treated as sequential, but are
  now treated as parrallel as specified by the spec;
  i.e. the following is now correctly rejected:
  `(let (x 0) (y x) (...))`

### Features

- Added support for higher order, including tptp's THF and Zf
- Optimized some corner cases of the typechecker to avoid
  exponential blowups

### API

- The interface of the `Expr` module has changed to support
  higher-order
- Additionally, there is now proper support for type aliases
  (which are expanded on demand as necessary), in `Expr`
- There is now a new typechecker module exposed as Thf for
  typing higher order expressions


v0.5
----

### Additions

- Added a functorized typechecker for all language supported by Dolmen
- Added a LSP server for all language supported by Dolmen

