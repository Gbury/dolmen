
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

type lang = Logic.language
type ty_state = Typer.ty_state
type solve_state = unit

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

  (* Solving state *)
  solve_state       : 'solve;

  (* Output settings *)
  export_lang       : (lang * Format.formatter) list;

}

type t = solve_state state

exception Error of t


(* State and locations *)
(* ************************************************************************* *)

let loc_input st (loc : Dolmen.Std.Loc.loc) =
  if loc.max_line_length >= 80 then
    None
  else begin
    match st.loc_style, st.input_source with
    | _, `Stdin -> None
    | `Short, _ -> None
    | `Contextual, `File filename ->
      let full_filename = Filename.concat st.input_dir filename in
      let input = Pp_loc.Input.file full_filename in
      Some input
    | `Contextual, `Raw (_, contents) ->
      let input = Pp_loc.Input.string contents in
      Some input
  end

let pp_loc st fmt o =
  match o with
  | None -> ()
  | Some loc ->
    if Dolmen.Std.Loc.is_dummy loc then ()
    else begin
      match loc_input st loc with
      | None ->
        Format.fprintf fmt "%a:@ "
          Fmt.(styled `Bold @@ styled (`Fg (`Hi `White)) Dolmen.Std.Loc.fmt) loc
      | Some input ->
        let locs = Dolmen.Std.Loc.lexing_positions loc in
        Format.fprintf fmt "%a:@ %a"
          Fmt.(styled `Bold @@ styled (`Fg (`Hi `White)) Dolmen.Std.Loc.fmt) loc
          (Pp_loc.pp ~max_lines:3 ~input) [locs]
    end

let error ?loc st error payload =
  let loc = Dolmen.Std.Misc.opt_map loc Dolmen.Std.Loc.full_loc in
  let aux _ = Code.exit (Report.Error.code error) in
  Format.kfprintf aux Format.err_formatter
    ("@[<v>%a%a @[<hov>%a@]%a@]@.")
    (pp_loc st) loc
    Fmt.(styled `Bold @@ styled (`Fg (`Hi `Red)) string) "Error"
    Report.Error.print (error, payload)
    Report.Error.print_hints (error, payload)

let warn ?loc st warn payload =
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
        (pp_loc st) loc
        Fmt.(styled `Bold @@ styled (`Fg (`Hi `Magenta)) string) "Warning"
        Report.Warning.print (warn, payload)
        Report.Warning.print_hints (warn, payload)

  | Fatal ->
    let aux _ = Code.exit (Report.Warning.code warn) in
    Format.kfprintf aux Format.err_formatter
      ("@[<v>%a%a @[<hov>%a@]%a@]@.")
      (pp_loc st) loc
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

let full_mode_switch =
  Report.Warning.mk ~code:Code.generic ~mnemonic:"full-mode-switch"
    ~message:(fun fmt lang ->
        Format.fprintf fmt
          "The@ %s@ format@ does@ not@ support@ \
           incremental@ mode,@ switching@ to@ full@ mode"
          lang)
    ~name:"Forced switch to full mode" ()

let switch_to_full_mode lang t =
  let old_mode = input_mode t in
  let t = set_mode t `Full in
  match old_mode with
  | Some `Incremental -> warn t full_mode_switch lang
  | _ -> t

let set_lang t l =
  let t = { t with input_lang = Some l; } in
  match l with
  | Logic.Alt_ergo ->
    switch_to_full_mode "Alt-Ergo" t
  | _ -> t

