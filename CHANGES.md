
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

