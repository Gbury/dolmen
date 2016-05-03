
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let help_msg =
  "Small utility to parse various file formats.

   Usage: `dolmen [options] file`
   The file format is guessed based on the extension of the file name."

let parse_opts () =
  let file = ref None in
  let print = ref false in
  let set_file f = match !file with
    | None -> file := Some f
    | Some _ -> failwith "can only parse one file"
  in
  let opts =
    Arg.align
      [ "--print", Arg.Set print, " print the parsed statements and format"
      ]
  in
  Arg.parse opts set_file help_msg;
  match !file with
    | None ->
      Arg.usage opts help_msg;
      exit 0
    | Some f -> f, !print

let () =
  let file, print = parse_opts () in
  try
    let module M = Logic.Make(ParseLocation)(Term)(Statement) in
    let lang, stmts = M.parse_file file in
    Format.printf "%s: ok@." file;
    if print then (
      Format.printf "guessed format : %s@." (M.string_of_language lang);
      Format.printf "@[<2>statements: %d <opaque>@]@." (List.length stmts);
    )
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


