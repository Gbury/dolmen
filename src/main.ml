
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let opts =
  let open Cmdliner in
  let aux file = file in
  let input =
    let docv = "FILE" in
    let doc = "Input file (format is guessed based on the extension of the filename)." in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv ~doc)
  in
  Term.(pure aux $ input)

let info =
  let open Cmdliner in
  let doc = "Small utility to parse various file formats" in
  Term.info ~doc ~version:"0.1" "dolmen"

let () =
  match Cmdliner.Term.eval (opts, info) with
  | `Version | `Help -> exit 0
  | `Error (`Parse | `Term | `Exn) -> exit 1
  | `Ok file ->
    begin try
        begin match Misc.get_extension (Filename.basename file) with
          | ".cnf" ->
            let module Parse = Dimacs.Make(ParseLocation)(Term)(Cnf) in
            let _ = Parse.parse_file file in
            ()
          | ".smt2" ->
            let module Parse = Smtlib.Make(ParseLocation)(Term)(Statement) in
            let _ = Parse.parse_file file in
            ()
          | ".zf" ->
            let module Parse = Zf.Make(ParseLocation)(Term)(Statement) in
            let _ = Parse.parse_file file in
            ()
          | ".p" ->
            let module Parse = Tptp.Make(ParseLocation)(Term)(Statement) in
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
    end


