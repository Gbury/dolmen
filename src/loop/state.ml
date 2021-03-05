
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition & Exceptions *)
(* ************************************************************************* *)

type perm =
  | Allow
  | Warn
  | Error

exception File_not_found of Dolmen.Std.Loc.full * string * string

exception Input_lang_changed of Logic.language * Logic.language

(* Type definition *)
(* ************************************************************************* *)

type lang = Logic.language
type ty_state = Typer.ty_state
type solve_state = unit

type 'solve state = {

  (* Debug option *)
  debug             : bool;

  (* Warning/Error options *)
  context           : bool;
  max_warn          : int;
  cur_warn          : int;

  (* Limits for time and size *)
  time_limit        : float;
  size_limit        : float;

  (* Input settings *)
  input_dir         : string;
  input_lang        : lang option;
  input_mode        : [ `Full
                      | `Incremental ] option;
  input_source      : [ `Stdin
                      | `File of string
                      | `Raw of string * string ];

  input_file_loc    : Dolmen.Std.Loc.file;

  (* Header check *)
  header_check      : bool;
  header_state      : Headers.t;
  header_licenses   : string list;
  header_lang_version : string option;

  (* Typechecking state *)
  type_state        : ty_state;
  type_check        : bool;
  type_strict       : bool;

  (* Solving state *)
  solve_state       : 'solve;

  (* Output settings *)
  export_lang       : (lang * Format.formatter) list;

}

type t = solve_state state

(* State and locations *)
(* ************************************************************************* *)

let pp_loc fmt o =
  match o with
  | None -> ()
  | Some loc ->
    if Dolmen.Std.Loc.is_dummy loc then ()
    else Format.fprintf fmt "%a:@ " Dolmen.Std.Loc.fmt loc

let error ?(code=Code.bug) ?loc _ format =
  let loc = Dolmen.Std.Misc.opt_map loc Dolmen.Std.Loc.full_loc in
  Format.kfprintf (fun _ -> Code.exit code) Format.err_formatter
    ("@[<v>%a%a @[<hov>" ^^ format ^^ "@]@]@.")
    Fmt.(styled `Bold @@ styled (`Fg (`Hi `White)) pp_loc) loc
    Fmt.(styled `Bold @@ styled (`Fg (`Hi `Red)) string) "Error"

let warn ?loc st format =
  let loc = Dolmen.Std.Misc.opt_map loc Dolmen.Std.Loc.full_loc in
  let aux _ = { st with cur_warn = st.cur_warn + 1; } in
  if st.cur_warn >= st.max_warn then
    Format.ikfprintf aux Format.err_formatter format
  else
    Format.kfprintf aux Format.err_formatter
      ("@[<v>%a%a @[<hov>" ^^ format ^^ "@]@]@.")
      Fmt.(styled `Bold @@ styled (`Fg (`Hi `White)) pp_loc) loc
      Fmt.(styled `Bold @@ styled (`Fg (`Hi `Magenta)) string) "Warning"

let flush st () =
  let aux _ = { st with cur_warn = 0; } in
  if st.cur_warn <= st.max_warn then
    aux ()
  else
    Format.kfprintf aux Format.err_formatter
      ("@[<v>%a @[<hov>%s@ %d@ %swarnings@]@]@.")
      Fmt.(styled `Bold @@ styled (`Fg (`Hi `Magenta)) string) "Warning"
      (if st.max_warn = 0 then "Counted" else "Plus")
      (st.cur_warn - st.max_warn)
      (if st.max_warn = 0 then "" else "additional ")

(* Getting/Setting options *)
(* ************************************************************************* *)

let time_limit t = t.time_limit
let size_limit t = t.size_limit

let input_dir t = t.input_dir
let input_mode t = t.input_mode
let input_lang t = t.input_lang
let input_source t = t.input_source

let input_file_loc st = st.input_file_loc
let set_input_file_loc st f = { st with input_file_loc = f; }

let set_mode t m = { t with input_mode = Some m; }

let header_state { header_state; _ } = header_state
let set_header_state st header_state = { st with header_state; }

let check_headers { header_check; _ } = header_check
let allowed_licenses { header_licenses; _ } = header_licenses
let allowed_lang_version { header_lang_version; _ } = header_lang_version

let ty_state { type_state; _ } = type_state
let set_ty_state st type_state = { st with type_state; }

let typecheck st = st.type_check
let strict_typing { type_strict; _ } = type_strict

let is_interactive = function
  | { input_source = `Stdin; _ } -> true
  | _ -> false

let prelude st =
  match st.input_lang with
  | None -> "prompt> @?"
  | Some l ->
    Format.asprintf "(%s)# @?" (Logic.string_of_language l)

(* Setting language *)
(* ************************************************************************* *)

let switch_to_full_mode lang t =
  let old_mode = input_mode t in
  let t = set_mode t `Full in
  match old_mode with
  | Some `Incremental ->
    warn t
      "The@ %s@ format@ does@ not@ support@ \
       incremental@ mode,@ switching@ to@ full@ mode"
      lang
  | _ -> t

let set_lang_aux t l =
  let t = { t with input_lang = Some l; } in
  match l with
  | Logic.Alt_ergo -> switch_to_full_mode "Alt-Ergo" t
  | _ -> t

let set_lang t l =
  match t.input_lang with
  | None -> set_lang_aux t l
  | Some l' ->
    if l = l'
    then set_lang_aux t l
    else raise (Input_lang_changed (l', l))

(* Full state *)
(* ************************************************************************* *)

let start _ = ()
let stop _ = ()

let file_not_found ~loc ~dir ~file =
  raise (File_not_found (loc, dir, file))

