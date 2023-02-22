
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Exit codes *)
(* ************************************************************************* *)

type t = {
  mutable code : int;
  (* codes are set later (to accomodate users of the lib choosing error codes
     that do not conflict with theirs), and should be unique for each exit code *)
  descr : string;
  category : string;
  mutable abort : bool;
}

let hash t = t.code
let equal t t' = t.code = t'.code
let compare t t' = compare t.code t'.code

let descr t = t.code, t.descr
let category t = t.category

(* Setting exact return codes *)
(* ************************************************************************* *)

(* cmdliner uses codes 123, 124 and 125, and codes greater then 125 are
   usually reserved for the shells. *)
let max_code = 122
let all_errors = ref []
let code_array = Array.make (max_code + 1) None

let code_used code =
  code <= 0 || code > max_code ||
  match code_array.(code) with
  | Some _ -> true
  | None -> false

let find_code () =
  let i = ref 1 in
  while !i <= max_code && code_used !i do i := !i + 1 done;
  if !i > max_code
  then assert false (* no available error code *)
  else !i

let set_retcode (t, code) =
  assert (t.code = -1);
  assert (not (code_used code));
  code_array.(code) <- Some t;
  t.code <- code;
  ()

let init ?(full=false) l =
  List.iter set_retcode l;
  List.iter (fun t ->
      if t.code < 0 then begin
        if full then failwith "partial retcode init"
        else begin
          let code = find_code () in
          set_retcode (t, code)
        end
      end) (List.rev !all_errors)


(* Exit with a code and code status *)
(* ************************************************************************* *)

let is_abort t = t.abort
let abort t = t.abort <- true
let error t = t.abort <- false

let exit t =
  if t.code < 0 then failwith "missing retcode"
  else if t.abort then (Unix.kill (Unix.getpid ()) Sys.sigabrt; assert false)
  else exit t.code


(* Manipulation *)
(* ************************************************************************* *)

(* The create function should only be used for error exit codes,
   the ok exit code (i.e. [0]) is create manually and not included
   in the errors list. *)
let create ~category ~descr =
  let t = {
    code = -1;
    abort = false;
    descr;
    category;
  } in
  all_errors := t :: !all_errors;
  t

(*  *)
let errors () =
  List.rev !all_errors

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

