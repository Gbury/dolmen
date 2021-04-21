
open Res
open Ret
open Cmdliner

let int_range_conv =
  let print fmt (min, step, max) =
    Format.fprintf fmt "(%d, %d, %d)" min step max
  in
  let parse s =
    let parse_int = Arg.(conv_parser int) in
    match String.split_on_char ',' s with
    | [ min; step; max] ->
      let+ min = parse_int min in
      let+ step = parse_int step in
      let+ max = parse_int max in
      Ok (min, step, max)
    | _ ->
      Error (`Msg "expected three comma-separated integers")
  in
  Arg.conv ~docv:"MIN,STEP,MAX" (parse, print)


(* Config cli *)

let config =
  let aux cmds
      minor_heap_size major_heap_increment
      space_overhead max_overhead allocation_policy =
    let* cmd =
      match cmds with
      | [] -> `Error (false, "empty command")
      | l -> `Ok (String.concat " " l)
    in
    let* minor_heap_size =
      Range.Int.mk
        "minor-heap-size" minor_heap_size
        ~min_limit:1_000 ~max_limit:max_int
    in
    let* major_heap_increment =
      Range.Int.mk
        "major-heap-increment" major_heap_increment
        ~min_limit:1 ~max_limit:1_001
    in
    let* space_overhead =
      Range.Int.mk
        "space-overhead" space_overhead
        ~min_limit:1 ~max_limit:max_int
    in
    let* max_overhead =
      Range.Int.mk
        "max-overhead" max_overhead
        ~min_limit:0 ~max_limit:1000001
    in
    let* allocation_policy =
      Range.Int.mk
        "allocation-policy" allocation_policy
        ~min_limit:0 ~max_limit:2
    in
    `Ok Config.{
      cmd; minor_heap_size; major_heap_increment;
      space_overhead; max_overhead; allocation_policy;
    }
  in
  let cmds =
    let doc = "command to run" in
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"CMD")
  in
  let minor_heap_size =
    let doc = "Range for the minor heap size parameter" in
    Arg.(value & opt int_range_conv (250_000,250_000,1_000_000) &
         info ["minor-heap-size"] ~doc)
  in
  let major_heap_increment =
    let doc = "Range for the major heap increment parameter" in
    Arg.(value & opt int_range_conv (15,85,100) &
         info ["major-heap-increment"] ~doc)
  in
  let space_overhead =
    let doc = "Range for the space overhead parameter" in
    Arg.(value & opt int_range_conv (80,120,200) &
         info ["space-overhead"] ~doc)
  in
  let max_overhead =
    let doc = "Range for the max overhead parameter" in
    Arg.(value & opt int_range_conv (500,500,1000) &
         info ["max-overhead"] ~doc)
  in
  let allocation_policy =
    let doc = "Range for the allocation policy parameter" in
    Arg.(value & opt int_range_conv (0,1,2) &
         info ["allocation-policy"] ~doc)
  in
  Term.(ret (const aux $ cmds $
             minor_heap_size $ major_heap_increment $
             space_overhead $ max_overhead $ allocation_policy))


(* Input/Output state files *)
let input_state =
  let doc = "Input state file" in
  Arg.(required & opt (some file) None & info ["i";"input"] ~doc)

let output_state =
  let doc = "Output state file" in
  Arg.(required & opt (some string) None & info ["o";"output"] ~doc)


(* Parralelism option *)
let j =
  let doc = "Parralelism" in
  Arg.(value & opt int 1 & info ["j"] ~doc)

