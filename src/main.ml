
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)


let () =
  if Array.length Sys.argv < 2 then
    (Format.printf "usage: dolmen file@."; exit 1);

  let file = Sys.argv.(1) in

  try
    begin match Misc.get_extension (Filename.basename file) with
      | ".cnf" ->
        let module Parse = Dimacs.Make(ParseLocation)(Cnf) in
        let _ = Parse.parse_file file in
        ()
      | ".smt2" ->
        let module Parse = Smtlib.Make(ParseLocation)(Term)(Statement) in
        let _ = Parse.parse_file file in
        ()
      | fmt ->
        Format.printf "Unrecognised format: '%s'@." fmt;
        exit 1
    end;
    Format.printf "%s: ok@." Sys.argv.(1)
  with
  | ParseLocation.Syntax_error (pos, msg) ->
    Format.printf "%a@\nSyntax error: %s@." ParseLocation.fmt pos msg
  | ParseLocation.Lexing_error (pos, lexeme) ->
    Format.printf "%a@\nLexing error, unrecognised lexeme: '%s'@." ParseLocation.fmt pos lexeme
  | ParseLocation.Uncaught (pos, e) ->
    Format.printf "%a@\n%s@." ParseLocation.fmt pos (Printexc.to_string e)


