
next
----

### Doc

- Add examples in the doc and tuto for type-checking (including
  a minimal working example in the tutorial).

### UI

- Make the unknown logic fatal by default, and simply enabled
  in non-strict mode (PR#158)
- Add the `--check-flow` option to checks coherence of sequences
  of statements (PR#135)
- Ensure stability of error codes for the `dolmen` binary

### Parsing

- Add `attrs` fields for all declarations and definition types,
  and correctly attach predicate attribute to individual definitions
  (PR#130)
- Restore support for toplevel "and" in non-recursive predicate/function
  definitions in Alt-Ergo syntax (PR #147, fixes issue #144)
- Add support for hexadecimal floats in Alt-Ergo syntax (PR #148, fixes
  issue #145)
- Add local goals to the `Prove` statement (PR#140)
- Add a check-sat/prove-sat statement to ae's language (PR#140)

### Typing

- Ignore arithmetic restrictions when typing model values. This
  particularly affects difference logic (PR#141)
- Rename theory-specific configuration to `config` (instead of
  `arith`, `arrays`, etc..) (PR#142)
- Add printing function for logics (PR#142)
- Attach type definitions to type-defs (PR#157)
- Add a proper reason for reserved builtins (PR#155)
- Add bitvector builtins for alt-ergo's language (PR#136)

### Loop

- Add possibility for users of the loop library to choose the
  exit/return code for a `Code.t` (PR#134)
- Add the `Flow` module for flow checking (PR#135)
- Add the `check` function in `typer.ml`/`typer_intf.ml`
- Add `update` and `update_opt` in `State` (PR#156)
- Print type definitions in the printer of typed statements (PR#157)
- Prelude statements have been removed and replaced with prelude files (PR#160)
- `Typer.additional_builtins` is now a `State.key` and takes the current state
  and language as arguments (PR#160)

### Model

- Fix bug in bitvector implementation: negative inputs to `bvsmod`
  would raise an internal error (PR#138)
- Remove the "error" keyword and statement from smtlib2
  response (model) files (PR#139)
- Correctly compare abstract array values (PR#143)
- Accept extensions of functions/symbols with only partially defined
  semantics, for e.g. `fp.to_ubv`, `div`, etc.. (PR#151)

v0.8.1
------

### UI

- Fix handling of size/time limits on windows (PR#117)
- Fix spurious printing of backtraces (PR#118)
- Add release binaries for windows

### LSP

- Add option for the lsp to read preludes before checking
  each file (PR#116)
- The LSP now sends an empty list of diagnostics upon closing
  a file (PR#116)

### Parsing

- Fix a bug related to alt-ergos function definition, which
  were previously alwyas non-recursive. Now, alt-ergo's function
  definitions are always recursive (PR#123)
- Add `parse_raw_lazy` to parse a string into a lazy list of
  statements (PR#125)
- Add support for mutually recursive functions and predicates
  in Alt-ergo's native language (PR#129)

### Typing

- Properly add binding locations for implicit type variables
  (PR#123)
- Ensure that type of recursively defined symbols are freshened
  to avoid type variables sharing between declaration and
  definition (PR#123)

### Loop

- Use `GC.finalise` instead of `Gc.finalise_last` in `loop/parser.ml`
  in order to avoid a bug in the ocaml 5.0 runtime, see ocaml/ocaml#12001
  (PR#128)
- New module to implement Alarms (size/time limits) (PR#117)
- Add optional argument to `Pipeline.run` to specify
  an alarm implementation (PR#117)
- Add a `bt` key to the state to record whether we should print
  backtraces (PR#118)
- Use `parse_raw_lazy` to parse raw contents in full mode if/when
  necessary (PR#125)


v0.8
----

### UI

- Add a minimal reporting style accessible via the `--report-style`
  option. When used, the `dolmen` binary will use at most one line
  to output the result of processing the input file
- Add an option to the `dolmen` binary to force a specific smtlib2
  logic, overriding the one given in the file. This is accessible
  via the `--force-smtlib2-logic` option
- Add some documentation for setting up the lsp with neovim (PR#114)

### Model verification

- Added model verification. This currently supports all
  builtins, except for String/Regular expressions.

### Parsing

- Fix long compilation time of tptp parser due to flambda (PR#111)
- Replace some `assert false` by proper error messages when
  there is not the same number of function signatures as function
  definitions in a `define-funs-rec` command in smtlib2
- Accept all reserved words in s-exprs in smtlib (mainly affects
  parsing of attributes)
- Added a parser for the smtlib model specification language
- Fix doc comments mentionning removed parameters
  (PR #107, issue #106)
- Add an option to print syntax error identifiers (mainly to be
  used for debug)
- Register a printer for the `Uncaught_exn` exception (mainly useful
  for library users)
- Add a tag to differentiate predicates from functions in alt-ergo
  (PR#104)


### Typing

- Properly typecheck s-expressions in attributes for smtlib2
  (most notably in :patterns attributes for psmt2)
- Cleaned up handling of definitions: instead of using
  the functors in `Def`, definitions are now simply declared
  using the functions exposed by the typechecker
- Stop emitting unused warnings for type wildcards
- Expose term constants in the `Std.Expr` module (PR#112)

### Loop

- Changed the state type from a record to an heterogeneous
  map. This simplifies interfaces for all Loop modules,
  and makes it much more extensible.
- Added initialization functions for each pipe in order to
  correctly init the expecteds keys in the state
- Allow users to better control the interactive prompt when
  parsing from stdin (PR#113)


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

