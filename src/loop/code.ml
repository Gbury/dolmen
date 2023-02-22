
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

let descr t = t.code, t.descr
let category t = t.category


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

(* cmdliner uses codes 123, 124 and 125 *)
let max_code = 122
let all_codes = Array.make (max_code + 1) None

let code_used code =
  code <= 0 || code > max_code ||
  match all_codes.(code) with
  | Some _ -> true
  | None -> false

let find_code () =
  let i = ref 1 in
  while !i <= max_code && code_used !i do i := !i + 1 done;
  if !i > max_code
  then assert false (* no available error code *)
  else !i


(* The create function should only be used for error exit codes,
   the ok exit code (i.e. [0]) is create manually and not included
   in the errors list. *)
let create ?code ~category ~descr () =
  let code =
    match code with
    | Some c -> assert (not (code_used c)); c
    | None -> find_code ()
  in
  (* cmdliner uses retcode 124 for cli errors *)
  assert (0 < code && code < 124);
  let t = {
    code; descr;
    category;
    abort = false;
  } in
  all_codes.(code) <- Some t;
  t

(*  *)
let errors () =
  let r = ref [] in
  for i = Array.length all_codes - 1 downto 1 do
    match all_codes.(i) with
    | Some t -> r := t :: !r
    | None -> ()
  done;
  !r

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
  create ()
    ~code:1
    ~category:"Generic"
    ~descr:"on generic error"
let limit =
  create ()
    ~code:2
    ~category:"Limits"
    ~descr:"upon reaching limits (time, memory, etc..)"
let parsing =
  create ()
    ~code:3
    ~category:"Parsing"
    ~descr:"on parsing errors"
let typing =
  create ()
    ~code:4
    ~category:"Typing"
    ~descr:"on typing errors"

