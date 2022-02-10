
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

type ty_state = Typer.ty_state
type solve_state = unit

type 'lang file = 'lang State_intf.file

type 'solve state = {

  (* Debug, warnings, and error options *)
  debug             : bool;
  reports           : Report.Conf.t;
  loc_style         : [ `Short | `Contextual ];
  max_warn          : int;
  cur_warn          : int;

  (* Limits for time and size *)
  time_limit        : float;
  size_limit        : float;

  (* Input files *)
  logic_file        : Logic.language file;
  response_file     : Response.language file;

  (* Header check *)
  header_check      : bool;
  header_state      : Headers.t;
  header_licenses   : string list;
  header_lang_version : string option;

  (* Typechecking state *)
  type_state        : ty_state;
  type_check        : bool;

  (* Solving state *)
  solve_state       : 'solve;

  (* Model checking *)
  check_model       : bool;
  check_state       : 'solve state Check.t;
}

type t = solve_state state

exception Error of t


(* State and locations *)
(* ************************************************************************* *)

let loc_input ?file st (loc : Dolmen.Std.Loc.loc) =
  (* sanity check to avoid pp_loc trying to read and/or print
     too much when printing the source code snippet) *)
  if loc.max_line_length >= 150 ||
     loc.stop_line - loc.start_line >= 100 then
    None
  else begin
    match st.loc_style, (file : _ file option) with
    | _, None -> None
    | _, Some { source = `Stdin; _ } -> None
    | `Short, _ -> None
    | `Contextual, Some { source = `File filename; dir; _ } ->
      let full_filename = Filename.concat dir filename in
      let input = Pp_loc.Input.file full_filename in
      Some input
    | `Contextual, Some { source = `Raw (_, contents); _ } ->
      let input = Pp_loc.Input.string contents in
      Some input
  end

let pp_loc ?file st fmt o =
  match o with
  | None -> ()
  | Some loc ->
    if Dolmen.Std.Loc.is_dummy loc then ()
    else begin
      match loc_input ?file st loc with
      | None ->
        Format.fprintf fmt "%a:@ "
          Fmt.(styled `Bold @@ styled (`Fg (`Hi `White)) Dolmen.Std.Loc.fmt) loc
      | Some input ->
        let locs = Dolmen.Std.Loc.lexing_positions loc in
        Format.fprintf fmt "%a:@ %a"
          Fmt.(styled `Bold @@ styled (`Fg (`Hi `White)) Dolmen.Std.Loc.fmt) loc
          (Pp_loc.pp ~max_lines:3 ~input) [locs]
    end

let error ?file ?loc st error payload =
  let loc = Dolmen.Std.Misc.opt_map loc Dolmen.Std.Loc.full_loc in
  let aux _ = Code.exit (Report.Error.code error) in
  Format.kfprintf aux Format.err_formatter
    ("@[<v>%a%a @[<hov>%a@]%a@]@.")
    (pp_loc ?file st) loc
    Fmt.(styled `Bold @@ styled (`Fg (`Hi `Red)) string) "Error"
    Report.Error.print (error, payload)
    Report.Error.print_hints (error, payload)

let warn ?file ?loc st warn payload =
  let loc = Dolmen.Std.Misc.opt_map loc Dolmen.Std.Loc.full_loc in
  match Report.Conf.status st.reports warn with
  | Disabled -> st
  | Enabled ->
    let aux _ = { st with cur_warn = st.cur_warn + 1; } in
    if st.cur_warn >= st.max_warn then
      aux st
    else
      Format.kfprintf aux Format.err_formatter
        ("@[<v>%a%a @[<hov>%a@]%a@]@.")
        (pp_loc ?file st) loc
        Fmt.(styled `Bold @@ styled (`Fg (`Hi `Magenta)) string) "Warning"
        Report.Warning.print (warn, payload)
        Report.Warning.print_hints (warn, payload)

  | Fatal ->
    let aux _ = Code.exit (Report.Warning.code warn) in
    Format.kfprintf aux Format.err_formatter
      ("@[<v>%a%a @[<hov>%a@]%a@]@.")
      (pp_loc ?file st) loc
      Fmt.(styled `Bold @@ styled (`Fg (`Hi `Red)) string) "Fatal Warning"
      Report.Warning.print (warn, payload)
      Report.Warning.print_hints (warn, payload)

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

let logic_file t = t.logic_file
let set_logic_file t logic_file = { t with logic_file; }

let response_file t = t.response_file
let set_response_file t response_file = { t with response_file; }

let header_state { header_state; _ } = header_state
let set_header_state st header_state = { st with header_state; }

let check_headers { header_check; _ } = header_check
let allowed_licenses { header_licenses; _ } = header_licenses
let allowed_lang_version { header_lang_version; _ } = header_lang_version

let check_model t = t.check_model
let check_model_eager _ = false
let check_state t = t.check_state
let set_check_state t check_state = { t with check_state; }

let ty_state { type_state; _ } = type_state
let set_ty_state st type_state = { st with type_state; }

let typecheck st = st.type_check

let is_interactive = function
  | { logic_file = { source = `Stdin; _ }; _ } -> true
  | _ -> false

let prelude st =
  match st.logic_file.lang with
  | None -> "prompt> @?"
  | Some l ->
    Format.asprintf "(%s)# @?" (Logic.string_of_language l)

