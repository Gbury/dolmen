
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let handle_exn st exn =
  let () = Errors.exn st exn in
  exit 1

let finally st e =
  match e with
  | None -> st
  | Some exn -> handle_exn st exn

let debug_pipe ((st, c) as res) =
  if st.Loop.State.debug then
    Format.eprintf "%a@." Dolmen.Std.Statement.print c;
  res

let () =
  let man = [
    `S Options.common_section;
    `P "Common options for the dolmen binary";
    `S Options.gc_section;
    `P "Options to fine-tune the gc, only experts should use these.";
    `S Cmdliner.Manpage.s_bugs;
    `P "You can report bugs at https://github.com/Gbury/dolmen/issues";
    `S Cmdliner.Manpage.s_authors;
    `P "Guillaume Bury <guillaume.bury@gmail.com>"
  ] in
  let info = Cmdliner.Term.info ~man ~version:"0.1" "dolmen" in
  let st = match Cmdliner.Term.eval (Options.state, info) with
    | `Version | `Help -> exit 0
    | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
    | `Ok opt -> opt
  in
  let st, g =
    try Loop.Parser.parse [] st
    with exn -> handle_exn st exn
  in
  let st =
    let open Loop.Pipeline in
    run ~finally g st (
      (fix (apply ~name:"expand" Loop.Parser.expand) (
          (apply ~name:"debug" debug_pipe)
          @>>> (apply ~name:"headers" Loop.Header.inspect)
          @>>> (apply ~name:"typecheck" Loop.Typer.typecheck)
          @>|> ((apply fst) @>>> _end)
        )
      )
    )
  in
  let st = Loop.Header.check st in
  let _st = Dolmen_loop.State.flush st () in
  ()

