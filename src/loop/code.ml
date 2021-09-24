
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Exit codes *)
(* ************************************************************************* *)

type t = {
  code : int; (* codes are unique for each exit code *)
  descr : string;
  category : string;
  mutable abort : bool;
}

let hash t = t.code
let equal t t' = t.code = t'.code
let compare t t' = compare t.code t'.code


(* Exit with a code and code status *)
(* ************************************************************************* *)

let is_abort t = t.abort
let abort t = t.abort <- true
let error t = t.abort <- false

let exit t =
  if t.abort then Unix.kill (Unix.getpid ()) Sys.sigabrt;
  exit t.code


(* Manipulation *)
(* ************************************************************************* *)

let counter = ref 0
let errors = ref []

(* The create function should only be used for error exit codes,
   the ok exit code (i.e. [0]) is create manually and not included
   in the errors list. *)
let create ~category ~descr =
  incr counter;
  let code = !counter in
  (* cmdliner uses retcode 124 for cli errors *)
  assert (0 < code && code < 124);
  let t = {
    code; descr;
    category;
    abort = false;
  } in
  errors := t :: !errors;
  t

(*  *)
let errors () = List.rev !errors

let descr t = t.code, t.descr
let category t = t.category

(* Special values *)
(* ************************************************************************* *)

let ok = {
  code = 0;
  descr = "the success exit code";
  category = "N/A";
  abort = false;
}

let bug = {
  code = 125;
  descr = "on unexpected internal errors (bugs)";
  category = "Internal";
  abort = false;
}

(* Predefined values *)
(* ************************************************************************* *)

let generic =
  create
    ~category:"Generic"
    ~descr:"on generic error"
let limit =
  create
    ~category:"Limits"
    ~descr:"upon reaching limits (time, memory, etc..)"
let parsing =
  create
    ~category:"Parsing"
    ~descr:"on parsing errors"
let typing =
  create
    ~category:"Typing"
    ~descr:"on typing errors"

