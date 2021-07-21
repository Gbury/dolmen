
next
----

### Typing

- Removed the typing of real and extended bitvector literals
  from the Float theory. These are not part of the FP
  specification, so it's better for Dolmen to be strict.
  Additionally, dependengin on the order of theories, they
  could shadow the proper typing of such literals and
  result in bogus warnings/errors
  PR#79 (see also Issue#43 Issue#74)

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

