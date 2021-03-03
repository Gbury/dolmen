
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Exit codes *)
(* ************************************************************************* *)

type t = {
  code : int; (* codes are unique for each exit code *)
  descr : string;
  mutable active : bool;
}

let hash t = t.code
let equal t t' = t.code = t'.code
let compare t t' = compare t.code t'.code


(* Exit with a code and code status *)
(* ************************************************************************* *)

let is_active t = t.active
let enable t = t.active <- true
let disable t = t.active <- false
let toggle t = t.active <- not t.active

let exit t =
  if is_active t then exit t.code else exit 0


(* Manipulation *)
(* ************************************************************************* *)

let counter = ref 0
let errors = ref []

(* The create function should only be used for error exit codes,
   the ok exit code (i.e. [0]) is create manually and not included
   in the errors list. *)
let create descr =
  incr counter;
  let code = !counter in
  (* cmdliner uses retcode 124 for cli errors *)
  assert (0 < code && code < 124);
  let t = {
    code; descr;
    active = true;
  } in
  errors := t :: !errors;
  t

(*  *)
let errors () = List.rev !errors

let descr t = t.code, t.descr


(* Special values *)
(* ************************************************************************* *)

let ok = {
  code = 0;
  descr = "the success exit code";
  active = true;
}

let bug = {
  code = 125;
  descr = "on unexpected internal errors (bugs)";
  active = true;
}

(* Predefined values *)
(* ************************************************************************* *)

let generic = create "on generic errore"
let limit = create "upon reaching limits (time, memory, etc..)"
let parsing = create "on parsing errors"
let typing = create "on typing errore"

