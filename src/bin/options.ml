
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

open Cmdliner

(* Sections *)
(* ************************************************************************* *)

let gc_section = "GC OPTIONS"
let error_section = "ERROR HANDLING"
let header_section = "HEADER CHECKING"
let common_section = Manpage.s_options
let profiling_section = "PROFILING"

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

let mode_list = [
    "full", `Full;
    "incremental", `Incremental;
  ]

let mode_conv = Arg.enum mode_list

(* Warning modifiers *)
(* ************************************************************************ *)

type warn_mod =
  | Disable of string
  | Enable of string
  | Fatal of string
  | Exact of string

let warn_parser s =
  if String.length s = 0 then
    Error (`Msg "empty warning modifier")
  else begin
    match s.[0] with
    | '@' -> Ok (Fatal (String.sub s 1 (String.length s - 1)))
    | '-' -> Ok (Disable (String.sub s 1 (String.length s - 1)))
    | '+' -> Ok (Enable (String.sub s 1 (String.length s - 1)))
    | '=' -> Ok (Exact (String.sub s 1 (String.length s - 1)))
    | _ -> Ok (Enable s)
  end

let warn_printer fmt = function
  | Disable s -> Format.fprintf fmt "-%s" s
  | Enable s -> Format.fprintf fmt "+%s" s
  | Fatal s -> Format.fprintf fmt "@%s" s
  | Exact s -> Format.fprintf fmt "=%s" s

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
  (print_aux "Go" n_giga) ^
  (print_aux "Mo" n_mega) ^
  (print_aux "ko" n_kilo) ^
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
  let exception Jump of string in
  let conf = Dolmen_loop.Report.Conf.mk ~default:Enabled in
  let conf =
    if not strict then conf
    else Dolmen_loop.Report.Conf.fatal conf Dolmen_loop.Typer.almost_linear
  in
  try
    let handle s modif = function
      | Ok conf -> conf
      | Error `Error_mnemonic ->
        raise (Jump (
            Format.asprintf
              "the mnemonic '%s refers to an error, but only warnings can be %s."
              s modif))
      | Error `Unknown_mnemonic ->
        raise (Jump (
            Format.asprintf
              "The mnemonic '%s' is unknown, please check the spelling." s))
    in
    let res =
      List.fold_left (fun conf l ->
          List.fold_left (fun conf -> function
              | Disable s ->
                handle s "disabled " @@
                Dolmen_loop.Report.Conf.disable_mnemonic conf s
              | Enable s ->
                handle s "enabled" @@
                Dolmen_loop.Report.Conf.enable_mnemonic conf s
              | Fatal s ->
                handle s "made fatal" @@
                Dolmen_loop.Report.Conf.fatal_mnemonic conf s
              | Exact s ->
                handle s "exactly enabled" @@
                Dolmen_loop.Report.Conf.set_enabled_mnemonic conf s
            ) conf l
        ) conf warn_modifiers
    in
    `Ok res
  with Jump msg ->
    `Error (false, msg)

let split_input = function
  | `Stdin ->
    Sys.getcwd (), `Stdin
  | `File f ->
    Filename.dirname f, `File (Filename.basename f)

let mk_state
    () gc gc_opt bt colors
    abort_on_bug
    time_limit size_limit
    input_lang input_mode input
    header_check header_licenses
    header_lang_version
    type_check
    debug context max_warn reports
  =
  (* Side-effects *)
  let () = Option.iter Gc.set gc_opt in
  let () = set_color Unix.stdout Format.std_formatter colors in
  let () = set_color Unix.stderr Format.err_formatter colors in
  let () = if bt then Printexc.record_backtrace true in
  let () = if gc then at_exit (fun () -> Gc.print_stat stdout;) in
  let () = if abort_on_bug then Dolmen_loop.Code.abort Dolmen_loop.Code.bug in
  (* State creation *)
  let input_dir, input_source = split_input input in
  let st : Loop.State.t = {
    debug; reports;

    context; max_warn;
    cur_warn = 0;

    time_limit; size_limit;

    input_dir; input_lang;
    input_mode; input_source;
    input_file_loc = Dolmen.Std.Loc.mk_file "";

    header_check; header_licenses; header_lang_version;
    header_state = Dolmen_loop.Headers.empty;

    type_check;
    type_state = Dolmen_loop.Typer.new_state ();

    solve_state = ();

    export_lang = [];
  } in
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


(* Main Options parsing *)
(* ************************************************************************* *)

let state =
  let docs = common_section in
  let gc =
    let doc = "Print statistics about the gc upon exiting" in
    Arg.(value & flag & info ["g"; "gc"] ~doc ~docs:gc_section)
  in
  let bt =
    let doc = "Enables printing of backtraces." in
    Arg.(value & flag & info ["b"; "backtrace"] ~doc ~docs:error_section)
  in
  let colors =
    let doc = "Activate coloring of output" in
    Arg.(value & opt color_conv Auto & info ["color"] ~doc ~docs)
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
              "Without suffix, default to a size in octet." in
    Arg.(value & opt c_size 1_000_000_000. &
         info ["s"; "size"] ~docv:"SIZE" ~doc ~docs)
  in
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
        (Arg.doc_alts_enum ~quoted:true mode_list) in
    Arg.(value & opt (some mode_conv) None & info ["m"; "mode"] ~doc ~docs)
  in
  let input =
    let doc = "Input problem file. If no file is specified,
               dolmen will enter interactive mode and read on stdin." in
    Arg.(value & pos 0 input_source_conv `Stdin & info [] ~docv:"FILE" ~doc)
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
  (*
  let locs =
    let doc = "Whether to keep location information during typing. \
               Setting this to true results in better warning/error \
               messages, but will use more memory when running." in
    Arg.(value & opt bool true & info ["locs"] ~doc ~docs)
  in
  *)
  let debug =
    let doc = Format.asprintf
        "Activate debug mode. Among other things, this will make dolmen \
         print every statement it parses, and every statement after type \
         checking, as well as activate unique id printing." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let context =
    let doc = Format.asprintf
        "Print the context / fragment of parsed AST with errors" in
    Arg.(value & flag & info ["context"] ~doc ~docs:error_section)
  in
  let max_warn =
    let doc = Format.asprintf
        "Maximum number of warnings to display (excess warnings will be
         counted and a count of silenced warnings reported at the end)." in
    Arg.(value & opt int max_int & info ["max-warn"] ~doc ~docs:error_section)
  in
  Term.(const mk_state $ profiling_t $
        gc $ gc_t $ bt $ colors $ abort_on_bug $
        time $ size $ in_lang $ in_mode $ input $
        header_check $ header_licenses $ header_lang_version $
        typing $ debug $ context $ max_warn $ reports)


