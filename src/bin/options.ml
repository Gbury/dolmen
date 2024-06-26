
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

open Cmdliner

(* Sections *)
(* ************************************************************************* *)

let gc_section = "GC OPTIONS"
let model_section = "MODEL CHECKING"
let error_section = "ERROR HANDLING"
let flow_section = "FLOW CHECKING"
let header_section = "HEADER CHECKING"
let option_section = Manpage.s_options
let common_section = Manpage.s_common_options
let profiling_section = "PROFILING"
let internal_section = "INTERNAL"

(* Initialise error codes *)
(* ************************************************************************* *)

let () =
  Dolmen_loop.Code.init ~full:true [
    Dolmen_loop.Code.generic, 1;
    Dolmen_loop.Code.limit, 2;
    Dolmen_loop.Code.parsing, 3;
    Dolmen_loop.Code.typing, 4;
    Dolmen_loop.Headers.code, 5;
    Dolmen_model.Loop.code, 6;
    Dolmen_loop.Flow.code, 7;
  ]

(* Main commands *)
(* ************************************************************************* *)

type cmd =
  | Run of {
      state : Loop.State.t;
      preludes: Dolmen_loop.Logic.language Loop.State.file list;
      logic_file : Dolmen_loop.Logic.language Loop.State.file;
    }
  | List_reports of {
      conf : Dolmen_loop.Report.Conf.t;
    }
  | Doc of {
      report : Dolmen_loop.Report.T.t;
      conf : Dolmen_loop.Report.Conf.t;
    }
  | List_extensions

(* Color options *)
(* ************************************************************************* *)

type color =
  | Auto
  | Always
  | Never

let color_list = [
  "auto", Auto;
  "always", Always;
  "never", Never;
]

let color_conv = Arg.enum color_list

let set_color file_descr formatter color =
  let style =
    match color with
    | Always -> `Ansi_tty
    | Never -> `None
    | Auto -> if Unix.isatty file_descr then `Ansi_tty else `None
  in
  Fmt.set_style_renderer formatter style


(* Input format converter *)
(* ************************************************************************* *)

let input_format_conv = Arg.enum Dolmen_loop.Logic.enum
let response_format_conv = Arg.enum Dolmen_loop.Response.enum


(* Input source converter *)
(* ************************************************************************* *)

(* Converter for input file/stdin *)
let input_to_string = function
  | `Stdin -> "<stdin>"
  | `Raw _ -> "<raw>"
  | `File f -> f

let input_source_conv =
  let parse x = Ok (`File x) in
  let print fmt i = Format.fprintf fmt "%s" (input_to_string i) in
  Arg.conv (parse, print)


(* Output converters *)
(* ************************************************************************* *)

let output_to_string = function
  | `Stdout -> "<stdout>"
  | `File f -> f

let parse_output = function
  | "stdout" -> Ok `Stdout
  | f -> Ok (`File f)

let output_conv =
  let print fmt o = Format.fprintf fmt "%s" (output_to_string o) in
  Arg.conv (parse_output, print)


(* Input modes *)
(* ************************************************************************* *)

let input_mode_list = [
    "full", `Full;
    "incremental", `Incremental;
  ]

let input_mode_conv = Arg.enum input_mode_list

(* Mnemonic converter *)
(* ************************************************************************ *)

let mnemonic_parser s =
  match Dolmen_loop.Report.T.find_mnemonic s with
  | Some ((`All | `Error _ | `Warning _) as res) ->
    Ok res
  | None ->
    Error (`Msg (
        Format.asprintf
          "@;the mnemonic '%s' is unknown, please check the spelling." s))

let mnemonic_printer fmt t =
  let s = Dolmen_loop.Report.T.mnemonic t in
  Format.fprintf fmt "%s" s

let mnemonic_conv = Arg.conv (mnemonic_parser, mnemonic_printer)


(* Warning modifiers *)
(* ************************************************************************ *)

type warn_mod =
  [ `Disable | `Enable | `Fatal | `Non_fatal ] *
  [ Dolmen_loop.Report.T.all | Dolmen_loop.Report.T.warn ]

let warn_parser s =
  if String.length s = 0 then
    Error (`Msg "empty warning modifier")
  else begin
    let aux change modif s =
      match mnemonic_parser s with
      | Ok ((`All | `Warning _) as res) -> Ok (modif, res)
      | Error _ as res -> res
      | Ok `Error _ ->
        Error (`Msg (
            Format.asprintf
              "@;the mnemonic '%s' refers to an error, \
               but only warnings can be %s" s change))
    in
    match s.[0] with
    | '@' -> aux "made fatal" `Fatal (String.sub s 1 (String.length s - 1))
    | '-' -> aux "disabled" `Disable (String.sub s 1 (String.length s - 1))
    | '+' -> aux "enabled" `Enable (String.sub s 1 (String.length s - 1))
    | '=' -> aux "enabled" `Non_fatal (String.sub s 1 (String.length s - 1))
    | _ -> aux "enabled" `Enable s
  end

let warn_printer fmt (modif, r) =
  let c =
    match modif with
    | `Disable -> '-'
    | `Enable -> '+'
    | `Fatal -> '@'
    | `Non_fatal -> '='
  in
  Format.fprintf fmt "%c%s" c (Dolmen_loop.Report.T.mnemonic r)

let c_warn = Arg.conv (warn_parser, warn_printer)
let c_warn_list = Arg.list c_warn


(* Argument converter for integer with multiplier suffix *)
(* ************************************************************************ *)

let nb_sec_minute = 60
let nb_sec_hour = 60 * nb_sec_minute
let nb_sec_day = 24 * nb_sec_hour

let time_string f =
  let n = int_of_float f in
  let aux n div = n / div, n mod div in
  let n_day, n = aux n nb_sec_day in
  let n_hour, n = aux n nb_sec_hour in
  let n_min, n = aux n nb_sec_minute in
  let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
  (print_aux "d" n_day) ^
  (print_aux "h" n_hour) ^
  (print_aux "m" n_min) ^
  (print_aux "s" n)

let print_time fmt f = Format.fprintf fmt "%s" (time_string f)

let parse_time arg =
  let l = String.length arg in
  let multiplier m =
    let arg1 = String.sub arg 0 (l-1) in
    `Ok (m *. (float_of_string arg1))
  in
  assert (l > 0);
  try
    match arg.[l-1] with
    | 's' -> multiplier 1.
    | 'm' -> multiplier 60.
    | 'h' -> multiplier 3600.
    | 'd' -> multiplier 86400.
    | '0'..'9' -> `Ok (float_of_string arg)
    | _ -> `Error "bad numeric argument"
  with Failure _ -> `Error "bad numeric argument"

let size_string f =
  let n = int_of_float f in
  let aux n div = n / div, n mod div in
  let n_giga, n = aux n 1_000_000_000 in
  let n_mega, n = aux n 1_000_000 in
  let n_kilo, n = aux n 1_000 in
  let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
  (print_aux "G" n_giga) ^
  (print_aux "M" n_mega) ^
  (print_aux "k" n_kilo) ^
  (print_aux "" n)

let print_size fmt f =
  Format.fprintf fmt "%s" (size_string f)

let parse_size arg =
  let l = String.length arg in
  let multiplier m =
    let arg1 = String.sub arg 0 (l-1) in
    `Ok (m *. (float_of_string arg1))
  in
  assert (l > 0);
  try
    match arg.[l-1] with
    | 'k' -> multiplier 1e3
    | 'M' -> multiplier 1e6
    | 'G' -> multiplier 1e9
    | 'T' -> multiplier 1e12
    | '0'..'9' -> `Ok (float_of_string arg)
    | _ -> `Error "bad numeric argument"
  with Failure _ -> `Error "bad numeric argument"

let c_time = parse_time, print_time
let c_size = parse_size, print_size


(* Location styles *)
(* ************************************************************************* *)

let report_style =
  Arg.enum [
    "minimal", Loop.State.Minimal;
    "regular", Loop.State.Regular;
    "contextual", Loop.State.Contextual;
  ]

(* Dolmen extensions *)
(* ************************************************************************ *)

let extension =
  let print ppf (t, _) = Extensions.pp ppf t in
  Cmdliner.Arg.conv (Extensions.parse, print)

(* Smtlib2 logic and extensions *)
(* ************************************************************************ *)

let smtlib2_logic =
  let parse s =
    match Dolmen_type.Logic.Smtlib2.parse s with
    | Some _ -> Result.Ok s
    | None -> Result.Error (`Msg (
        Format.asprintf "'%s' is not a valid smtlib2 logic" s))
  in
  let print fmt _s = Format.fprintf fmt "" in
  Arg.conv (parse, print)

let smtlib2_ext_list =
  List.map
    (fun ext -> Dolmen_std.Extensions.Smtlib2.name ext, ext)
    (Dolmen_std.Extensions.Smtlib2.exts ())

let smtlib2_ext =
  Arg.enum smtlib2_ext_list


(* State creation *)
(* ************************************************************************* *)

let gc_opts use_env
    minor_heap_size major_heap_increment
    space_overhead max_overhead allocation_policy =
  if use_env then None
  else begin
    let default = Gc.get () in
    Some { default with
           minor_heap_size; major_heap_increment;
           space_overhead; max_overhead; allocation_policy;
         }
  end

let profiling_opts stats
    memtrace_filename memtrace_sampling_rate =
  Dolmen_std.Stats.enabled := stats;
  match (memtrace_filename : _ option) with
  | None -> `Ok ()
  | Some filename ->
    if Memory_profiler.available then begin
      Memory_profiler.start filename memtrace_sampling_rate;
      `Ok ()
    end else begin
      let msg =
        "Memtrace is not available, try to install memtrace and recompile Dolmen."
      in
      `Error (false, msg)
    end

let reports_opts strict warn_modifiers =
  let conf = Dolmen_loop.Report.Conf.mk ~default:Enabled in
  let conf =
    if not strict then conf
    else begin
      let fatal warn conf = Dolmen_loop.Report.Conf.fatal conf warn in
      conf
      |> fatal
        (`Warning (Dolmen_loop.Report.Any_warn Dolmen_loop.Typer.bad_arith_expr))
      |> fatal
        (`Warning (Dolmen_loop.Report.Any_warn Dolmen_loop.Typer.unknown_logic))
    end
  in
  let res =
    List.fold_left (fun conf l ->
        List.fold_left (fun conf -> function
            | `Disable, w -> Dolmen_loop.Report.Conf.disable conf w
            | `Enable, w -> Dolmen_loop.Report.Conf.enable conf w
            | `Fatal, w -> Dolmen_loop.Report.Conf.fatal conf w
            | `Non_fatal, w -> Dolmen_loop.Report.Conf.set_enabled conf w
          ) conf l
      ) conf warn_modifiers
  in
  `Ok res

let mk_run_state
    () gc gc_opt bt colors
    abort_on_bug
    time_limit size_limit
    response_file
    flow_check
    header_check header_licenses header_lang_version
    smtlib2_forced_logic smtlib2_exts
    type_check extensions
    check_model (* check_model_mode *)
    debug report_style max_warn reports syntax_error_ref
  =
  (* Side-effects *)
  let () = Option.iter Gc.set gc_opt in
  let () = set_color Unix.stdout Format.std_formatter colors in
  let () = set_color Unix.stderr Format.err_formatter colors in
  let () = List.iter Dolmen_std.Extensions.Smtlib2.enable smtlib2_exts in
  (* base (which is a transitive dependency, due to farith),
     unconditionally enables backtraces, which is fine. But that means that
     for our purpose, we need to store whether to print the backtraces somewhere
     else, since we cannot reuse the [backtrace_status ()]. *)
  let () = if bt then Printexc.record_backtrace true in
  let () = if gc then at_exit (fun () -> Gc.print_stat stdout;) in
  let () = if abort_on_bug then Dolmen_loop.Code.abort Dolmen_loop.Code.bug in
  let () = Hints.model ~check_model (* ~check_model_mode *) in
  (* State creation *)
  let st =
    Loop.State.empty
    |> Loop.State.init
      ~bt ~debug ~report_style ~reports
      ~max_warn ~time_limit ~size_limit
      ~response_file
    |> Loop.Parser.init
      ~syntax_error_ref
      ~interactive_prompt:Loop.Parser.interactive_prompt_lang
    |> Loop.Typer.init
      ~smtlib2_forced_logic
    |> Loop.Typer_Pipe.init ~type_check
    |> Loop.Check.init
      ~check_model
      (* ~check_model_mode *)
    |> Loop.Flow.init ~flow_check
    |> Loop.Header.init
      ~header_check
      ~header_licenses
      ~header_lang_version
  in
  (* Extensions *)
  let st =
    List.fold_left (fun st (ext, kind) ->
      match kind with
      | None | Some Extensions.Typing ->
        Result.bind st (Extensions.load_typing_extension ext)
      | Some _ -> st
    ) (Ok st) extensions
  in
  if check_model then
    List.fold_left (fun st (ext, kind) ->
      match kind with
      | None | Some Extensions.Model ->
        Result.bind st (Extensions.load_model_extension ext)
      | Some _ -> st
    ) st extensions
  else
    st

(* Profiling *)
(* ************************************************************************* *)

let profiling_t =
  let docs = profiling_section in
  let stats =
    let doc = "Enable statistics collecting and printing" in
    Arg.(value & flag & info ["stats"] ~doc ~docs)
    in
  let memtrace_filename =
    let doc = "Filename for the memory profiling trace" in
    Arg.(value & opt (some string) None & info ["memtrace"] ~doc ~docs ~docv:"FILE")
  in
  let memtrace_sampling_rate =
    let doc = "Sampling rate for the memory profiler" in
    Arg.(value & opt float 1e-6 & info ["memtrace-rate"] ~doc ~docs ~docv:"RATE")
  in
  Term.(ret (const profiling_opts $ stats $
             memtrace_filename $ memtrace_sampling_rate))


(* Gc Options parsing *)
(* ************************************************************************* *)

let gc_t =
  let docs = gc_section in
  let use_env =
    let doc = "Use the gc settings from the OCAMLRUNPARAM env variable, \
               and ignore the dolmen settings on the cli." in
    Arg.(value & opt bool false & info ["gc-env"] ~doc ~docs)
  in
  let minor_heap_size =
    let doc = "Set Gc.minor_heap_size" in
    Arg.(value & opt int 1_000_000 & info ["gc-s"] ~doc ~docs)
  in
  let major_heap_increment =
    let doc = "Set Gc.major_heap_increment" in
    Arg.(value & opt int 100 & info ["gc-i"] ~doc ~docs)
  in
  let space_overhead =
    let doc = "Set Gc.space_overhead" in
    Arg.(value & opt int 200 & info ["gc-o"] ~doc ~docs)
  in
  let max_overhead =
    let doc = "Set Gc.max_overhead" in
    Arg.(value & opt int 500 & info ["gc-O"] ~doc ~docs)
  in
  let allocation_policy =
    let doc = "Set Gc.allocation policy" in
    Arg.(value & opt int 0 & info ["gc-a"] ~doc ~docs)
  in
  Term.((const gc_opts $ use_env $
         minor_heap_size $ major_heap_increment $
         space_overhead $ max_overhead $ allocation_policy))

(* Warning controls *)
(* ************************************************************************* *)

let reports =
  let docs = error_section in
  let warns =
    let doc = "Change the status of a warning. Accepts a list of      \
               comma-separated modifiers of the form @mnemonic, where \
               '@' is an (optional) modifier among '+' (default) to   \
                enable a warning, '-' to disable a warning and '!' to \
                make the warning fatal, and 'mnemonic' is the short   \
               (mnemonic) name of the warning." in
    Arg.(value  & opt_all c_warn_list [] & info ["w"; "warn"] ~docs ~doc)
  in
  let strict =
    let doc = "Be strict or more lenient wrt to typing" in
    Arg.(value & opt bool true & info ["strict"] ~doc ~docs:error_section)
  in
  Term.(ret (const reports_opts $ strict $ warns))

(* File inputs *)
(* ************************************************************************* *)

let mk_file lang mode input =
  let dir,source = Loop.State.split_input input in
  Loop.State.mk_file ?lang ?mode dir source

let logic_file =
  let docs = option_section in
  let in_lang =
    let doc = Format.asprintf
        "Set the input language to $(docv); must be %s."
        (Arg.doc_alts_enum ~quoted:true Dolmen_loop.Logic.enum) in
    Arg.(value & opt (some input_format_conv) None &
         info ["i"; "input"; "lang"] ~docv:"INPUT" ~doc ~docs)
  in
  let in_mode =
    let doc = Format.asprintf
        "Set the input mode, must be %s.
         The full mode parses the entire file before iterating over its
         contents whereas the incremental mode processes each declaration
         before parsing the next one. Default is incremental mode."
        (Arg.doc_alts_enum ~quoted:true input_mode_list) in
    Arg.(value & opt (some input_mode_conv) None & info ["m"; "mode"] ~doc ~docs)
  in
  let input =
    let doc = "Input problem file. If no file is specified,
               dolmen will enter interactive mode and read on stdin." in
    Arg.(value & pos 0 input_source_conv `Stdin & info [] ~docv:"FILE" ~doc)
  in
  Term.(const mk_file $ in_lang $ in_mode $ input)

let response_file =
  let docs = model_section in
  let response_lang =
    let doc = Format.asprintf
        "Set the language for the response file; must be %s."
        (Arg.doc_alts_enum ~quoted:true Dolmen_loop.Response.enum) in
    Arg.(value & opt (some response_format_conv) None &
         info ["response-lang"] ~docv:"FORMAT" ~doc ~docs)
  in
  let response_mode =
    let doc = Format.asprintf
        "Set the response reading mode, must be %s.
         The full mode parses the entire file before iterating over its
         contents whereas the incremental mode processes each declaration
         before parsing the next one. Default is incremental mode."
        (Arg.doc_alts_enum ~quoted:true input_mode_list) in
    Arg.(value & opt (some input_mode_conv) None & info ["response-mode"] ~doc ~docs)
  in
  let response =
    let doc = "Response file." in
    Arg.(value & opt input_source_conv `Stdin & info ["r"; "response"] ~doc ~docs)
  in
  Term.(const mk_file $ response_lang $ response_mode $ response)

let mk_preludes =
  List.map (fun f -> mk_file None None (`File f))

let preludes =
  let docs = option_section in
  let preludes =
    let doc = "Optional prelude file to be loaded before the input file." in
    Arg.(value & opt_all string [] & info ["p"; "prelude"] ~doc ~docs)
  in
  Term.(const mk_preludes $ preludes)

(* State term *)
(* ************************************************************************* *)

let state =
  let docs = option_section in
  let gc =
    let doc = "Print statistics about the gc upon exiting" in
    Arg.(value & flag & info ["g"; "gc"] ~doc ~docs:gc_section)
  in
  let bt =
    let doc = "Enables printing of backtraces." in
    Arg.(value & flag & info ["b"; "backtrace"] ~doc ~docs:error_section)
  in
  let colors =
    let doc =
      "Activate coloring of output. Available options are: auto, always, never."
    in
    Arg.(value & opt color_conv Auto & info ["color"] ~doc ~docs:common_section)
  in
  let abort_on_bug =
    let doc = Format.asprintf
        "Abort instead of exiting properly when an internal bug
        is detected (i.e. corresponds to an exit code of 125)." in
    Arg.(value & flag & info ["abort-on-bug"] ~doc ~docs:error_section)
  in
  let time =
    let doc = "Stop the program after a time lapse of $(docv).
                 Accepts usual suffixes for durations : s,m,h,d.
                 Without suffix, default to a time in seconds." in
    Arg.(value & opt c_time 300. & info ["t"; "time"] ~docv:"TIME" ~doc ~docs)
  in
  let size =
    let doc = "Stop the program if it tries and use more the $(docv) memory space. " ^
              "Accepts usual suffixes for sizes : k,M,G,T. " ^
              "Without suffix, default to a size in bytes." in
    Arg.(value & opt c_size 1_000_000_000. &
         info ["s"; "size"] ~docv:"SIZE" ~doc ~docs)
  in
  let flow_check =
    let doc = "If true, then check the coherence of top-level statements.
               This is mainly useful for the smtlib language." in
    Arg.(value & opt bool false & info ["check-flow"] ~doc ~docs:flow_section)
  in
  let header_check =
    let doc = "If true, then the presence of headers will be checked in the
               input file (and errors raised if they are not present)." in
    Arg.(value & opt bool false &
         info ["check-headers"] ~doc ~docs:header_section)
  in
  let header_licenses =
    let doc = "Set the allowed set of licenses in the headers.
               An empty list means allow everything." in
    Arg.(value & opt (list string) [] &
         info ["header-licenses"] ~doc ~docs:header_section)
  in
  let header_lang_version =
    let doc = "Set the only allowed language version for headers. If not set,
               all conforming version numbers are allowed." in
    Arg.(value & opt (some string) None &
         info ["header-lang-version"] ~doc ~docs:header_section)
  in
  let typing =
    let doc = "Decide whether to type-check input expressions. If false, only parsing
               is done. " in
    Arg.(value & opt bool true & info ["type"] ~doc ~docs)
  in
  let force_smtlib2_logic =
    let doc = "Force the smtlib2 logic to the given logic. In effect this
               replaces the string in the set-logic command by instead using
               the one given on the command line." in
    Arg.(value & opt (some smtlib2_logic) None & info ["force-smtlib2-logic"] ~doc ~docs)
  in
  let smtlib2_extensions =
    let doc = Format.asprintf
        "Activate smtlib2 extension. Currently an experimental option. \
        $(docv) must be %s" (Arg.doc_alts_enum smtlib2_ext_list)
    in
    Arg.(value & opt_all smtlib2_ext [] &
         info ["internal-smtlib2-extension"] ~docs:internal_section ~doc)
  in
  let ext =
    let doc = Format.asprintf
        "Enable extensions. Use $(b,--list-extensions) to list available \
        extensions."
    in
    Arg.(value & opt_all extension [] &
         info ["ext"] ~docs ~doc)
  in
  let debug =
    let doc = Format.asprintf
        "Activate debug mode. Among other things, this will make dolmen \
         print every statement it parses, and every statement after type \
         checking, as well as activate unique id printing." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let check_model =
    let doc = Format.asprintf
        "Whether to check models (require providing a response file)." in
    Arg.(value & opt bool false & info ["check-model"] ~doc ~docs)
  in
  (*
  let check_model_mode =
    let doc = Format.asprintf
        "Choose the mode for model verification. Must be %s.

         The $(b,full) mode will parse and type
         the problem file, and then the model file, and lastly check all assertions
         from the problem file. This requires storing in memory the type
         representation of all assertions from the problem file, which may require
         large amounts of memory.

         The $(b,interleave) mode will interleave the parsing
         and typing of the problem file and of the model file, so as to not need to
         store anything from the problem file in memory, but it requires that all
         symbols/constants are defined in the model file in the same order as they
         are declared in the input problem file."
        (Arg.doc_alts_enum ~quoted:true model_mode_list) in
    Arg.(value & opt model_mode_conv Full & info ["check-model-mode"] ~doc ~docs)
  in
  *)
  let report_style =
    let doc = Format.asprintf
        "Control the way locations are printed for error and warnings messages.
         $(b,regular) only prints the location for the message, while
         $(b,contextual) also displays the source code snippet corresponding
         to the location of the message (except in some cases where the snippet
         would be too long), and lastly $(b,minimal) prints each report on one line"
    in
    Arg.(value & opt report_style Contextual & info ["report-style"] ~doc ~docs:error_section)
    in
  let max_warn =
    let doc = Format.asprintf
        "Maximum number of warnings to display (excess warnings will be
         counted and a count of silenced warnings reported at the end)." in
    Arg.(value & opt int max_int & info ["max-warn"] ~doc ~docs:error_section)
    in
    let syntax_error_ref =
      let doc = Format.asprintf
         "Print the syntax error reference number when a syntax error is raised."
      in
      Arg.(value & opt bool false & info ["syntax-error-ref"] ~doc ~docs:error_section)
    in
  Term.(term_result (
    const mk_run_state $ profiling_t $
        gc $ gc_t $ bt $ colors $
        abort_on_bug $
        time $ size $
        response_file $
        flow_check $
        header_check $ header_licenses $ header_lang_version $
        force_smtlib2_logic $ smtlib2_extensions $
        typing $ ext $
        check_model $ (* check_model_mode $ *)
        debug $ report_style $ max_warn $ reports $ syntax_error_ref))


(* List command term *)
(* ************************************************************************* *)

let cli =
  let docs = option_section in
  let aux state preludes logic_file list doc list_extensions =
    match list, doc, list_extensions with
    | false, None, false ->
      `Ok (Run { state; logic_file; preludes })
    | false, Some report, false ->
      let conf = Loop.State.get Loop.State.reports state in
      `Ok (Doc { report; conf; })
    | true, None, false ->
      let conf = Loop.State.get Loop.State.reports state in
      `Ok (List_reports { conf; })
    | false, None, true ->
      `Ok List_extensions
    | _ ->
      `Error (false,
              "at most one of --list, --doc and --list-extensions might be \
               present on the command line")
  in
  let list =
    let doc = "List all reports (i.e. warnings and errors),
               that dolmen can emit." in
    Arg.(value & flag & info ["list"] ~doc ~docs)
  in
  let doc =
    let doc = "The warning or error of which to show the documentation." in
    Arg.(value & opt (some mnemonic_conv) None & info ["doc"] ~doc ~docv:"mnemonic" ~docs)
  in
  let list_extensions =
    let doc = "List all available extensions." in
    Arg.(value & flag & info ["list-extensions"] ~doc ~docs)
  in
  Term.(ret (
    const aux $ state $ preludes $ logic_file $ list $ doc
    $ list_extensions
  ))
