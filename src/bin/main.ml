
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Pipeline = Dolmen_loop.Pipeline.Make(Bin_state)
module Pipe = Dolmen_loop.Pipes.Make(Dolmen.Expr)(Bin_state)(Typer)

let handle_exn st = function
  | Pipeline.Sigint ->
    Format.pp_print_flush Format.std_formatter ();
    let _ = Errors.sigint st in
    exit 1
  | Pipeline.Out_of_time ->
    Format.pp_print_flush Format.std_formatter ();
    let _ = Errors.out_of_time st in
    exit 1
  | Pipeline.Out_of_space ->
    Format.pp_print_flush Format.std_formatter ();
    let _ = Errors.out_of_space st in
    exit 1
  | exn ->
    let _ = Errors.exn st exn in
    exit 1

let finally st e =
  match e with
  | None -> st
  | Some exn -> handle_exn st exn

let () =
  let info = Cmdliner.Term.info ~version:"0.1" "dolmen" in
  let st = match Cmdliner.Term.eval (Options.state, info) with
    | `Version | `Help -> exit 0
    | `Error `Parse | `Error `Term | `Error `Exn -> exit 1
    | `Ok opt -> opt
  in
  let st, g =
    try Pipe.parse [] st
    with exn -> handle_exn st exn
  in
  let _st =
    let open Pipeline in
    run ~finally g st (
      (fix (apply ~name:"expand" Pipe.expand) (
          (apply ~name:"typecheck" Pipe.typecheck)
          @>|> ((apply fst) @>>> _end)
        )
      )
    )
  in
  ()

