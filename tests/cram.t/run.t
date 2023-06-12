First we check that all the files are errors without preludes:

  $ dolmen twice.ae
  File "twice.ae", line 1, character 8-13:
  1 | goal g: twice(2) = 4
              ^^^^^
  Error Unbound identifier: `twice`
  [4]

  $ dolmen twice.smt2
  File "twice.smt2", line 2, character 19-24:
  2 | (assert (distinct (twice 2) 4))
                         ^^^^^
  Error Unbound identifier: `twice`
  [4]

  $ dolmen thrice.smt2
  File "thrice.smt2", line 2, character 19-25:
  2 | (assert (distinct (thrice 2) 6))
                         ^^^^^^
  Error Unbound identifier: `thrice`
  [4]

Now we check that everything works with .ae preludes, except for thrice.smt2
that uses an undefined function:

  $ dolmen --prelude prelude.ae twice.ae

  $ dolmen --prelude prelude.ae twice.smt2

  $ dolmen --prelude prelude.ae thrice.smt2
  File "thrice.smt2", line 2, character 19-25:
  2 | (assert (distinct (thrice 2) 6))
                         ^^^^^^
  Error Unbound identifier: `thrice`
  [4]

And also with .zf preludes, for good measure:

  $ dolmen --prelude prelude.zf twice.ae

  $ dolmen --prelude prelude.zf twice.smt2

  $ dolmen --prelude prelude.zf thrice.smt2
  File "thrice.smt2", line 2, character 19-25:
  2 | (assert (distinct (thrice 2) 6))
                         ^^^^^^
  Error Unbound identifier: `thrice`
  [4]

  $ dolmen --prelude prelude.smt2 twice.ae

For smt2 preludes there are obtuse set-logic warnings, which is a bit
unfortunate. Probably not worth fixing, at least until someone actually wants
to use smt2 preludes.

  $ dolmen --prelude prelude.smt2 twice.smt2
  File "twice.smt2", line 1, character 0-18:
  1 | (set-logic QF_LIA)
      ^^^^^^^^^^^^^^^^^^
  Warning Logic was already set at line 1, character 0-18

  $ dolmen --prelude prelude.smt2 thrice.smt2
  File "thrice.smt2", line 1, character 0-18:
  1 | (set-logic QF_LIA)
      ^^^^^^^^^^^^^^^^^^
  Warning Logic was already set at line 1, character 0-18
  File "thrice.smt2", line 2, character 19-25:
  2 | (assert (distinct (thrice 2) 6))
                         ^^^^^^
  Error Unbound identifier: `thrice`
  [4]
