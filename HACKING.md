# Hacking on Dolmen

This file is incomplete, but may have interesting tips if you want to start
hacking on dolmen. If you're having troubles, don't hesitate to contact me
directly, ^^

## Hand-written error messages

Dolmen now has support for customizing syntax error messages, using menhir's
mechanism for syntax errors. If you want to contribute and/or help, you might
first want to read [menhir's manual](http://cambium.inria.fr/~fpottier/menhir/manual.html#sec67)
about the error mecanism (and/or LR(1) parsers in general if you're not
familiar with them).

### Adding new error messages

Adding new error messages is as simple as editing the `syntax.messages`
file, and then re-building the project.

### Messages files and build rules

Not all languages in dolmen currently have the build rules needed to have
customized error messages, but it should be fairly easy to add them. For that,
you'll likely want to use the smtlib's support as reference. Concretely, for
a language `foobar`, this means comparing `src/language/foobar/dune` with
`src/language/smtlib2/v2.6/dune` and copying the missing parts.

Once setup, the build for error messages define a few different build targets,
that are described later. However, due to some limitations of dune (and/or my
own knowledge of it), not everything has been automated, but the manual
interventions should be limited to copying a file now and then. The files related
to error messages are the following:
- `syntax.messages` the checked-out file containing the customized/hand-written
  error messages. It must follow the menhir `.messages` file format in order
  for the build to succeed.
- `new.messages`: a useful tagret that allows one to ask menhir to generate
  a template `.messages` file. This target should only be used once at the
  start of setting up error messages, in order to copy it into a checked-out
  `syntax.messages`. The `new.messages` file is not meant to be committed,
  it is only a temporary file promoted by dune into the build treee for
  convenience.
- `updated.messages`: a target to ask menhir to generate an updated
  `.messages` file after a change in then parser/grammar of the language.
  It should retain all of the messages from `syntax.messages` that are still
  valid/relevant for the updated grammar, easing the process of updating the
  error messages when updating a grammar.

Additionally, the dune file defines rule so that when running the tests,
menhir checks that the `syntax.messages` file is up-to-date with the
current syntax.

### Changing the grammar

When changing the grammar/parser for a language that has hand-writtent error
messages, you'll need to update the `syntax.messages` file according to the
new grammar. The simplest for that is to ask dune to build the `updated.messages`
file for the givne language, and then copy it to overwrite the current
`syntax.messages` file (this process can not easily be automated by dune because
of the circular dependency it creates: `syntax.messages` actually depends on itself
in this case).



