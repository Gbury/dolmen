# Hacking on Dolmen

This file is incomplete, but may have interesting tips if you want to start
hacking on dolmen. If you're having troubles, don't hesitate to contact me
directly, ^^


## Tests

Most tests are setup through dune files, associated with tune "runtest" alias.

To run the tests:
```
make test
```

To promote the results of tests (if e.g. you have changed some error message,
or added new tests):
```
make test-promote
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


## Hand-written syntax error messages

Dolmen now has support for customizing syntax error messages, using menhir's
mechanism for syntax errors. If you want to contribute and/or help, you might
first want to read [menhir's
manual](http://cambium.inria.fr/~fpottier/menhir/manual.html#sec67) about the
error mecanism (and/or LR(1) parsers in general if you're not familiar with
them).

### Messages files and build rules

Not all languages in dolmen currently have the build rules needed to have
customized error messages, but it should be fairly easy to add them. For that,
you'll likely want to use the smtlib's support as reference. Concretely, for a
language `foobar`, this means comparing `src/language/foobar/dune` with
`src/language/smtlib2/v2.6/dune` and copying the missing parts.

Once setup, the build for error messages define a few different build targets,
that are described later. However, due to some limitations of dune (and/or my
own knowledge of it), not everything has been automated, but the manual
interventions should be limited to copying a file now and then. The files
related to error messages are the following:
- `syntax.messages` the checked-out file containing the customized/hand-written
  error messages. It must follow the menhir `.messages` file format in order
  for the build to succeed.
- `new.messages`: a useful tagret that allows one to ask menhir to generate a
  template `.messages` file. This target should only be used once at the start
  of setting up error messages, in order to copy it into a checked-out
  `syntax.messages`. The `new.messages` file is not meant to be committed, it
  is only a temporary file promoted by dune into the build treee for
  convenience.
- `updated.messages`: a target to ask menhir to generate an updated `.messages`
  file after a change in then parser/grammar of the language.  It should retain
  all of the messages from `syntax.messages` that are still valid/relevant for
  the updated grammar, easing the process of updating the error messages when
  updating a grammar.

Additionally, the dune file defines rule so that when running the tests, menhir
checks that the `syntax.messages` file is up-to-date with the current syntax.

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
grammar. There are three steps in that process:
- first, try and build the project with the new grammar, if some errors from
  the old grammar in the `syntax.messages` file are not errors anymore, this
  will produce a build error with the information about which errors in the
  `syntax.messages` file are invalid. You can either remove these error, or
  adjust the input sentence leading to the error if you know what you're doing.
- Second is to use dune to build the `updated.messages` file for the new
  grammar in order to update the state numbers in the error messages.
  language, and then copy it to overwrite the current `syntax.messages` file
  (this process can not easily be automated by dune because of the circular
  dependency it creates: `syntax.messages` actually depends on itself in this
  case).
- Third is to check that there are messages for all errors. Running `dune build
  @runtest` will produce an error if some error messages are missing from the
  `syntax.messages` file. For those errors, you'll have to copy the relevant
  part from the `new.messages` file into the `syntax.messages` file, and write
  an error message in the correct format (see above for more info on that).


