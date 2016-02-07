
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module D = Dimacs.Make(ParseLocation)(Dummy.Dimacs)

let () =
  if Array.length Sys.argv <= 2 then
    (Format.printf "usage: dolmen format file@."; exit 1);

  let parse =
    match Sys.argv.(1) with
    | "dimacs" -> D.parse_file
    | _ -> assert false
  in
  try
    let _ = parse Sys.argv.(2) in
    Format.printf "%s: ok@." Sys.argv.(2)
  with
  | ParseLocation.Syntax_error (pos, msg) ->
    Format.printf "%a@\nSyntax error: %s@." ParseLocation.fmt pos msg
  | ParseLocation.Lexing_error (pos, lexeme) ->
    Format.printf "%a@\nLexing error, unrecognised lexeme: '%s'@." ParseLocation.fmt pos lexeme
  | ParseLocation.Uncaught (pos, e) ->
    Format.printf "%a@\n%s@." ParseLocation.fmt pos (Printexc.to_string e)


