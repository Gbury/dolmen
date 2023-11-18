# Hacking on Dolmen

This file is incomplete, but may have interesting tips if you want to start
hacking on dolmen. If you're having troubles, don't hesitate to contact me
directly, ^^

## Build locally

You need to have [ocaml](https://ocaml.org/) installed to compile dolmen.
Additionally, some ocaml dependencies are needed. The easiest way to
get a working ocaml installation, and install the dependencies is to
use [opam](https://opam.ocaml.org/).
See [this page](https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system)
for information about how to install opam on your system.

Once you have opam installed, you can use the following commands to isntall
the needed dependencies to build dolmen:
```
opam pin add -n .
opam install --deps-only .
```

You can then build the project with
```
make
```


## Tests

Most tests are setup through dune files, associated with tune "runtest" alias.

To run the tests:
```
make test
```

To promote the results of tests (if e.g. you have changed some error message,
or added new tests):
```
make promote
```


### Unit tests

There a few (though not that many), unit tests. These are basically to test
internal functions of the library, in order to check internal assumptions
that may otherwise not be easy to check with complete parsing/typing tests.

Follow the example in `tests/unit/bitv/` to add more unit tests.


### Binary tests

There are quite a lot of tests for parsing, typing and other features that make
use of the `dolmen` binary to check the behaviour of the whole library. Some of
these tests verify that some well-formed inputs are correctly parsed/typed, but
most of the tests are there to check the behaviour in case of an error.  It is
planned that in the long term, each error message should have a corresponding
test case (see coverage tests).

These tests are found in the subfolders `tests/` (except for `unit` which
is reserved for unit tests of the library functions). These tests are run by
dune, and a script is used to generate the dune files, so that adding a new
test is as easy as creating a file with an extension recognized by dolmen
under the `tests` folder (in any subfolder, since they are scanned recursively
by the script).

The check performed for each of these test files is the following:
- given a test file `foo.ext`, there must exists a `foo.expected` corresponding
  file (if it does not exists, it will be created as an empty file by the
  script)
- for both incremental mode and full mode, do the following
  + run dolmen on the input file, using additional options from the flags.dune
    file (one option per line, with no space)
  + if `foo.expected` is empty, check that dolmen exited with error code 0,
    else, check that dolmen exited with error code 0 or 1
  + compare the output of dolmen with the `foo.expected` file

### Coverage tests

This is done using `bisect_ppx`. Once you have it installed (usually via opam),
just run `make coverage` and it should open the coverage report in a browser
window for you (via xdg-open).


## Guidelines and coding recommandations

This section sums up useful guidelines and recommendations conerning the various
parts of the code.

### Lexers

- Lexers should alwyas have a fallback case that raises the `Error` exception
  (else, ocamllex-generated parsers raise weird errors when no match can be found)

### Parsers

- In general, parsers should be as simple as possible and, as much as possible
  strive to only generate a structured representation of the input, without doing
  anything smart or semantic-related. Instead, most (if not all) of the processing
  should be done during typechecking. For instance, if a language does not define
  a specific syntax rule for equality, and instead just treat equality as a regular
  application (either a regular prefix application, or a generic infix appllication),
  then a parser should generate a regular application node (and **not** use the `Equal`
  builtin).


## Hand-written syntax error messages

Dolmen now has support for customizing syntax error messages, using menhir's
mechanism for syntax errors. If you want to contribute and/or help, you might
first want to read [menhir's
manual](http://cambium.inria.fr/~fpottier/menhir/manual.html#sec67) about the
error mecanism (and/or LR(1) parsers in general if you're not familiar with
them).

### Messages files and build rules

### Adding new error messages

Adding new error messages is as simple as editing the `syntax.messages`
file, and then re-building the project. Each error message in the
`syntax.messages` is composed of the following:
- an input sentence (in the form of a list of tokens) that makes the parser
  automaton go into some error state
- a comment detailing the state of the parser at that point, most notably
  the production being reduced/recognized; this, together with the `parseFoo.mly`
  file, should help one understand why the error happens, and what token would
  allow the parser to continue at that point
- the error message properly. Dolmen currently has the following convention for how
  messages are written:
```
XXX
production parsed (on a single line)
what is expected at that point,
possibly on multiple lines
```
  with on the first line an error message number (typically with 3 number, i.e. `042`),
  on the second line the name of the construction being parsed (e.g. a term, a sort,
  a function definition, etc...), and finally on the lines after that a description
  of what would have been acceptable inputs (warning: there must be no blank lines,
  else menhir will have trouble parsing the file).

Each syntax error must be exercised by at least one test file, typically in the
following file:
`tests/parsing/language/version/errors/XXX_some_informal_description_of_the_error.ext`.
It is always better to have more than one test case, for instance, by varying the token
that produces the error (in the `syntax.messages` file, the last token of each sentence
is assured to produce an error, but it is most ot the time not the only one; dolmen already
has a mechanism to inform the user of what token was read, so the error message written
should be agnostic of what the bad token is).

Note the following: dolmen currently has two modes of reading files: incremental and full.
In full mode, the whole file is read and parsed, and then each sentence sent for processing,
whereas in incremental mode, each sentence is read, parsed and processed before the next one
is read. Due to this, all parsers in dolmen have two main entry point: one entry point for
parsing a whole file, and one for parsing a single input sentence[1]. This will often
make it so that there are the same sentences twice in a `*.messages` file, once for
for entry point of the file and once for the entry point of a single sentence. These
two error cases must share the exact same error message (i.e. same error number, prodution
parsed, etc...). This coherence is automatically and always checked by the tests which use
both incremental mode and full mode and expect the same output in both cases.

On syntax error message numbers (i.e. the `XXX` in error messages):
- for uniformity, they are all on 3 numbers, which should be enough for all reasonable languages
  (mor than 1000 hand-written error messages seem a lot), but there should be no real limitation
- as much as possible, the error numbers should be contiguous starting from 0, however
  because editing messages by hand is already quite tiresome (and changing an error number
  means renaming test files that exercize it), it's not a problem is some numbers
  are unused/skipped

[1]: sadly, some languages (currently only the alt-ergo native format) cannot support
     incremental parsing because it is not possible to know when a sentence ends, except
     when another sentence begins. Concretely, this means such languages would have
     end-of-stream conflicts according to menhir. For those languages, inremental mode
     is not supported, but this should be an exception.

### Changing the grammar

When changing the grammar/parser for a language that has hand-written error
messages, you'll need to update the `syntax.messages` file according to the new
grammar. To do so, the simplest way is to run `dune build @runtest`, which will
check that the `syntax.messages` is correct with regards to the new grammar,
and if not, will print a diff to update the `syntax.messages` file. Running
`dune build @runtest --promote` will promote the diff and change the
`syntax.messages` file.


## Release workflow

Release checklist/workflow:

- update version number in `VERSION`
- update version number in src/bin/main.ml
- update `CHANGES.md` with the new version number
- commit the changes
- run `dune-release lint`
- run `dune-release tag`
- run `dune-release distrib`
- run `dune-release publish distrib`
- run `dune-release opam pkg`
- run `dune-release opam submit`
- add line for new version in `doc/scripts/index.txt`
- run `./doc/script/release` to update the doc on github-pages

