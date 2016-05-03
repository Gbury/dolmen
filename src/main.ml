
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module M = Logic.Make(ParseLocation)(Term)(Statement)

let help_msg =
  "Small utility to parse various file formats.

   Usage: `dolmen [options] file`
   The file format is guessed based on the extension of the file name."

let parse_opts () =
  let input = ref (`Stdin M.Smtlib) in
  let print = ref false in
  let set_file f = match !input with
    | `Stdin _ -> input := `File f
    | `File _ -> failwith "can only parse one file"
  in
  let opts =
    Arg.align
      [ "--print", Arg.Set print, " print the parsed statements and format"
      ]
  in
  Arg.parse opts set_file help_msg;
  !input, !print

let rec iter gen f =
  match gen () with
  | None | Some { Statement.descr = Statement.Exit } -> ()
  | Some s ->
    f s; iter gen f

let () =
  let input, print = parse_opts () in
  try
    let lang, stmts = M.parse_input input in
    begin match input with
      | `File f -> Format.printf "%s: ok@." f
      | `Stdin _ -> Format.printf "reading stdin@."
    end;
    if print then
      Format.printf "guessed format : %s@." (M.string_of_language lang);
    iter stmts (fun _s ->
        if print then Format.printf "<opaque>@."
      );
  with
    | ParseLocation.Lexing_error (pos, lexeme) ->
      Format.printf "%a@\nLexing error, unrecognised lexeme: '%s'@." ParseLocation.fmt pos lexeme;
      exit 2
    | ParseLocation.Syntax_error (pos, msg) ->
      Format.printf "%a@\nSyntax error: %s@." ParseLocation.fmt pos msg;
      exit 3
    | ParseLocation.Uncaught (pos, e) ->
      Format.printf "%a@\n%s@." ParseLocation.fmt pos (Printexc.to_string e);
      exit 4


