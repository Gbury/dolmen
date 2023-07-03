# Dolmen ![build](https://github.com/Gbury/dolmen/workflows/build/badge.svg) ![install](https://github.com/Gbury/dolmen/workflows/install/badge.svg)

A library providing flexible parsers and typecheckers for languages used in automated deduction.

LICENSE
-------

BSD2, see file LICENSE.

Documentation
-------------

Online documentation for the libraries can be found at <http://gbury.github.io/dolmen>.
There is also [a tutorial](https://github.com/Gbury/dolmen/tree/master/doc/tuto.md).


Installation
------------

The main method of installation is to use [opam](https://opam.ocaml.org/).
See [this page](https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system)
for information about how to install opam on your system. Once you have installed
and configured opam, you can use the following command to install the dolmen
cli and lsp binaries:

```
opam install dolmen_bin dolmen_lsp
```

Additionally, pre-built binaries can be found on the release pages
(starting from the v0.6 release for Linux and MacOS, and starting from the
v0.8.1 release for Windows). You can get them on
[the latest release page](https://github.com/Gbury/dolmen/releases/latest).


The libraries can be installed using:

```
opam install dolmen dolmen_type dolmen_loop dolmen_model
```


Goals
-----

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
- A LSP server so that the features of the above binary can also be used inside
  your favorite editor.
  See [the dolmen lsp doc](https://github.com/Gbury/dolmen/tree/master/doc/lsp.md)



Supported languages
-------------------

| Language              | Parsing             | Typing              | Model verification  |
| :---                  |       :---:         |        :---:        |        :---:        |
| ae (alt-ergo)         | :heavy_check_mark:  | :heavy_check_mark:  |                     |
| dimacs                | :heavy_check_mark:  | :heavy_check_mark:  |                     |
| iCNF                  | :heavy_check_mark:  | :heavy_check_mark:  |                     |
| smtlib                | :heavy_check_mark:  | :heavy_check_mark:  | :heavy_check_mark:  |
| tptp                  | :heavy_check_mark:  | :heavy_check_mark:  |                     |
| zf (zipperposition)   | :heavy_check_mark:  | :heavy_check_mark:  |                     |


Acknowledgements
----------------

This work is currently in part funded by [OCamlPro](https://ocamlpro.com)

