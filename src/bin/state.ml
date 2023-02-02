
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

include Dolmen_loop.State

(* Erros, Warnings & locations *)
(* ************************************************************************* *)

let loc_input ?file st (loc : Dolmen.Std.Loc.loc) =
  (* sanity check to avoid pp_loc trying to read and/or print
     too much when printing the source code snippet) *)
  if loc.max_line_length >= 150 ||
     loc.stop_line - loc.start_line >= 100 then
    None
  else begin
    match get report_style st, (file : _ file option) with
    | _, None -> None
    | _, Some { source = `Stdin; _ } -> None
    | (Minimal | Regular), _ -> None
    | Contextual, Some { source = `File filename; dir; _ } ->
      let full_filename = Filename.concat dir filename in
      let input = Pp_loc.Input.file full_filename in
      Some input
    | Contextual, Some { source = `Raw (_, contents); _ } ->
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
        let loc_start, loc_end = Dolmen.Std.Loc.lexing_positions loc in
        let locs = Pp_loc.Position.of_lexing loc_start, Pp_loc.Position.of_lexing loc_end in
        Format.fprintf fmt "%a:@ %a"
          Fmt.(styled `Bold @@ styled (`Fg (`Hi `White)) Dolmen.Std.Loc.fmt) loc
          (Pp_loc.pp ~max_lines:5 ~input) [locs]
    end

let flush st () =
  let aux _ = set cur_warn 0 st in
  let cur = get cur_warn st in
  let max = get max_warn st in
  if cur <= max then
    aux ()
  else
    match get report_style st with
    | Minimal ->
      Format.kfprintf aux Format.err_formatter
        "W:%d@." (cur - max)
    | Regular | Contextual ->
      Format.kfprintf aux Format.err_formatter
        ("@[<v>%a @[<hov>%s@ %d@ %swarnings@]@]@.")
        Fmt.(styled `Bold @@ styled (`Fg (`Hi `Magenta)) string) "Warning"
        (if max = 0 then "Counted" else "Plus")
        (cur - max) (if max = 0 then "" else "additional ")

let error ?file ?loc st error payload =
  let () = Tui.finalise_display () in
  let st = flush st () in
  let loc = Dolmen.Std.Misc.opt_map loc Dolmen.Std.Loc.full_loc in
  let aux _ = Dolmen_loop.Code.exit (Dolmen_loop.Report.Error.code error) in
  match get report_style st with
  | Minimal ->
    Format.kfprintf aux Format.err_formatter
      "E:%s@." (Dolmen_loop.Report.Error.mnemonic error)
  | Regular | Contextual ->
    Format.kfprintf aux Format.err_formatter
      ("@[<v>%a%a @[<hov>%a@]%a@]@.")
      (pp_loc ?file st) loc
      Fmt.(styled `Bold @@ styled (`Fg (`Hi `Red)) string) "Error"
      Dolmen_loop.Report.Error.print (error, payload)
      Dolmen_loop.Report.Error.print_hints (error, payload)

let warn ?file ?loc st warn payload =
  let loc = Dolmen.Std.Misc.opt_map loc Dolmen.Std.Loc.full_loc in
  match Dolmen_loop.Report.Conf.status (get reports st) warn with
  | Disabled -> st
  | Enabled ->
    let aux _ = update cur_warn ((+) 1) st in
    if get cur_warn st >= get max_warn st then
      aux st
    else
      begin match get report_style st with
        | Minimal ->
          Tui.kfprintf aux Format.err_formatter
            "W:%s@." (Dolmen_loop.Report.Warning.mnemonic warn)
        | Regular | Contextual ->
          Tui.kfprintf aux Format.err_formatter
            ("@[<v>%a%a @[<hov>%a@]%a@]@.")
            (pp_loc ?file st) loc
            Fmt.(styled `Bold @@ styled (`Fg (`Hi `Magenta)) string) "Warning"
            Dolmen_loop.Report.Warning.print (warn, payload)
            Dolmen_loop.Report.Warning.print_hints (warn, payload)
      end
  | Fatal ->
    let () = Tui.finalise_display () in
    let st = flush st () in
    let aux _ = Dolmen_loop.Code.exit (Dolmen_loop.Report.Warning.code warn) in
    begin match get report_style st with
      | Minimal ->
        Format.kfprintf aux Format.err_formatter
          "F:%s@." (Dolmen_loop.Report.Warning.mnemonic warn)
      | Regular | Contextual ->
        Format.kfprintf aux Format.err_formatter
          ("@[<v>%a%a @[<hov>%a@]%a@]@.")
          (pp_loc ?file st) loc
          Fmt.(styled `Bold @@ styled (`Fg (`Hi `Red)) string) "Fatal Warning"
          Dolmen_loop.Report.Warning.print (warn, payload)
          Dolmen_loop.Report.Warning.print_hints (warn, payload)
    end


