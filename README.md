# Dolmen ![build](https://github.com/Gbury/dolmen/workflows/build/badge.svg) ![test&doc](https://github.com/Gbury/dolmen/workflows/test/badge.svg)

A library providing flexible parsers for input languages.

## LICENSE

BSD2, see file LICENSE.

## Goals

The Dolmen project aims at providing an assortiment of tools to help
handle languages that are used in automated deduction and formal logic.

More precisely, the Dolmen project provides:
- A few OCaml libraries for:
  - parsing, see [the parsing doc](https://github.com/Gbury/dolmen/tree/master/doc/parsing.md)
  - typechecking, see [the typing doc](https://github.com/Gbury/dolmen/tree/master/doc/type.md)
  - writing main loops for binaries, [the loop doc](https://github.com/Gbury/dolmen/tree/master/doc/loop.md)
- A binary (which is using the above libraries), to parse and typecheck input files.
  This could be used to check a file against its language specification, and/or
  obtain detailed errors. See [the bin doc](https://github.com/Gbury/dolmen/tree/master/doc/bin.md)
- A LSP server so that the feature sof the above binary can also be used inside
  your favorite editor.
  See [the dolmen lsp doc](https://github.com/Gbury/dolmen/tree/master/doc/lsp.md)


## Documentation

Online documentation for the libraries can be found at <http://gbury.github.io/dolmen>.
There is also [a tutorial](https://github.com/Gbury/dolmen/tree/master/doc/tuto.md).

## Supported languages

Currently the following parsers are working:

- dimacs
- iCNF
- smtlib
- tptp
- zf (zipperposition format)

The following parsers are either work in progress, or soon to be
work in progress:

- coq
- dedukti

## Build & Install

You need [opam](https://opam.ocaml.org/) to install the developpement version of dolmen.
You can installations instructions for opam [here](https://opam.ocaml.org/doc/Install.html).

To install all packages provided by dolmen:

    opam pin add https://github.com/Gbury/dolmen.git

Manually, you'll need to first install the dependencies needed by dolmen, the easiest way
is to pin your local copy of dolmen like this:

    # At the root of your local dolmen repository
    opam pin add --no-action ./
    opam install --deps-only dolmen dolmen_type dolmen_loop dolmen_bin dolmen_lsp

Once the dependencies have been installed, you can build the project with:

    make

