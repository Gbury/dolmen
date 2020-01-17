
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

open Cmdliner

(* State creation *)
(* ************************************************************************* *)

let gc_opts
    minor_heap_size major_heap_increment
    space_overhead max_overhead allocation_policy =
  Gc.({ (get ()) with
        minor_heap_size; major_heap_increment;
        space_overhead; max_overhead; allocation_policy;
      }
     )

let split_input = function
  | `Stdin ->
    Sys.getcwd (), `Stdin
  | `File f ->
    Filename.dirname f, `File (Filename.basename f)

let mk_state
    gc gc_opt bt colors
    time_limit size_limit
    input_lang input
    type_check type_infer type_shadow
  =
  (* Side-effects *)
  let () = Gc.set gc_opt in
  let () =
    let style = if colors then `Ansi_tty else `None in
    Fmt.set_style_renderer Format.err_formatter style;
  in
  let () = if bt then Printexc.record_backtrace true in
  let () = if gc then at_exit (fun () -> Gc.print_stat stdout;) in
  (* State creation *)
  let input_dir, input_source = split_input input in
  let st : State.t = {
    time_limit; size_limit;

    input_dir; input_lang; input_source;

    type_state = (); type_check;
    type_infer; type_shadow;
    type_smtlib_logic = None;

    solve_state = ();

    export_lang = None;
  } in
  st

(* Input source converter *)
(* ************************************************************************* *)

(* Converter for input formats/languages *)
let input_format_conv = Arg.enum Dolmen_loop.Parse.enum

(* Converter for input file/stdin *)
let input_to_string = function
  | `Stdin -> "<stdin>"
  | `File f -> f

let input_source_conv =
  let parse x = Ok (`File x) in
  let print fmt i = Format.fprintf fmt "%s" (input_to_string i) in
  Arg.conv (parse, print)

(* Converter for permissions *)
let perm_conv = Arg.enum [
    "allow", State.Allow;
    "warn", State.Warn;
    "error", State.Error;
  ]

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
  let n_tera, n = aux n 1_000_000_000_000 in
  let n_giga, n = aux n 1_000_000_000 in
  let n_mega, n = aux n 1_000_000 in
  let n_kilo, n = aux n 1_000 in
  let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
  (print_aux "To" n_tera) ^
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

(* Gc Options parsing *)
(* ************************************************************************* *)

let gc_t =
  let minor_heap_size =
    let doc = "Set Gc.minor_heap_size" in
    Arg.(value & opt int 1_000_000 & info ["gc-s"] ~doc)
  in
  let major_heap_increment =
    let doc = "Set Gc.major_heap_increment" in
    Arg.(value & opt int 100 & info ["gc-i"] ~doc)
  in
  let space_overhead =
    let doc = "Set Gc.space_overhead" in
    Arg.(value & opt int 200 & info ["gc-o"] ~doc)
  in
  let max_overhead =
    let doc = "Set Gc.max_overhead" in
    Arg.(value & opt int 500 & info ["gc-O"] ~doc)
  in
  let allocation_policy =
    let doc = "Set Gc.allocation policy" in
    Arg.(value & opt int 0 & info ["gc-a"] ~doc)
  in
  Term.((const gc_opts $ minor_heap_size $ major_heap_increment $
         space_overhead $ max_overhead $ allocation_policy))

(* Main Options parsing *)
(* ************************************************************************* *)

let state =
  let gc =
    let doc = "Print statistics about the gc upon exiting" in
    Arg.(value & flag & info ["g"; "gc"] ~doc)
  in
  let bt =
    let doc = "Enables printing of backtraces." in
    Arg.(value & flag & info ["b"; "backtrace"] ~doc)
  in
  let colors =
    let doc = "Activate coloring of output" in
    Arg.(value & opt bool true & info ["color"] ~doc)
  in
  let time =
    let doc = "Stop the program after a time lapse of $(docv).
                 Accepts usual suffixes for durations : s,m,h,d.
                 Without suffix, default to a time in seconds." in
    Arg.(value & opt c_time 300. & info ["t"; "time"] ~docv:"TIME" ~doc)
  in
  let size =
    let doc = "Stop the program if it tries and use more the $(docv) memory space. " ^
              "Accepts usual suffixes for sizes : k,M,G,T. " ^
              "Without suffix, default to a size in octet." in
    Arg.(value & opt c_size 1_000_000_000. & info ["s"; "size"] ~docv:"SIZE" ~doc)
  in
  let in_lang =
    let doc = Format.asprintf
        "Set the input language to $(docv) (%s)."
        (Arg.doc_alts_enum ~quoted:false Dolmen_loop.Parse.enum) in
    Arg.(value & opt (some input_format_conv) None & info ["i"; "input"; "lang"] ~docv:"INPUT" ~doc)
  in
  let input =
    let doc = "Input problem file. If no file is specified,
               dolmen will enter interactive mode and read on stdin." in
    Arg.(value & pos 0 input_source_conv `Stdin & info [] ~docv:"FILE" ~doc)
  in
  let typing =
    let doc = "Decide whether to type-check input expressions. If false, only parsing
               is done. " in
    Arg.(value & opt bool true & info ["type"] ~doc)
  in
  let infer =
    let doc = Format.asprintf
        "Decide the permissions for type inference" in
    Arg.(value & opt (some perm_conv) None & info ["infer"] ~doc)
  in
  let shadow =
    let doc = Format.asprintf
        "Decide the permissions for shadowing of symbols" in
    Arg.(value & opt (some perm_conv) None & info ["shadow"] ~doc)
  in
  Term.(const mk_state $ gc $ gc_t $ bt $ colors $
        time $ size $ in_lang $ input $ typing $ infer $ shadow)


