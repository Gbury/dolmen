
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

open Dolmen

module M = Logic.Make(ParseLocation)(Id)(Term)(Statement)

let help_msg =
"Small utility to parse various file formats.

Usage: `dolmen [options] file`
The file format is guessed based on the extension of the file name."

let parse_opts () =
  let input = ref (`Stdin M.Smtlib) in
  let print = ref false in
  let norm = ref false in
  let mode = ref `Regular in
  let set_file f = match !input with
    | `Stdin _ -> input := `File f
    | `File _ -> failwith "can only parse one file"
  in
  let opts =
    Arg.align [
      "--print", Arg.Set print, " print the parsed statements and format";
      "--norm", Arg.Set norm, " normalize the parsed terms";
      "--interactive", Arg.Unit (fun () -> mode := `Interactive),
        " parse the file in interactive mode";
    ]
  in
  Arg.parse opts set_file help_msg;
  !input, !print, !norm, !mode

let handle_exn = function
  | ParseLocation.Lexing_error (pos, lexeme) ->
    Format.printf "%a@\nLexing error, unrecognised lexeme: '%s'@."
      ParseLocation.fmt pos lexeme
  | ParseLocation.Syntax_error (pos, "") ->
    Format.printf "%a@\nSyntax error.@." ParseLocation.fmt pos
  | ParseLocation.Syntax_error (pos, msg) ->
    Format.printf "%a@\nSyntax error: %s@." ParseLocation.fmt pos msg
  | ParseLocation.Uncaught (pos, e) ->
    Format.printf "%a@\n%s@." ParseLocation.fmt pos (Printexc.to_string e)
  | e ->
    Format.printf "Unexpect exception (really bad!):@\n%s@."
      (Printexc.to_string e)

let rec fold gen f acc =
  match gen () with
  | None | Some { Statement.descr = Statement.Exit; _ } ->
    acc
  | exception e ->
    let () = handle_exn e in
    fold gen f false
  | Some s ->
    let () = f s in
    fold gen f acc

let parse f =
  let tmp = ref None in
  let rec aux () =
    match !tmp with
    | None ->
      tmp := Some [];
      let _, l = M.parse_file f in
      tmp := Some l;
      aux ()
    | Some [] -> None
    | Some (s :: r) ->
      tmp := Some r;
      Some s
  in
  aux

let map f g () =
  match g () with
  | None -> None
  | Some x -> Some (f x)

let () =
  let input, print, norm, mode = parse_opts () in
  let lang, stmts =
    match mode with
    | `Regular ->
      begin match input with
        | `Stdin _ ->
          Format.printf "Reading stdin requires interactive mode@.";
          exit 3
        | `File f ->
          let lang, _, _ = M.of_extension (Misc.get_extension f) in
          (lang, (parse f))
      end
    | `Interactive ->
      let l, gen, _ = M.parse_input input in
      l, gen (* Cleanup fuction is ignored because it will be automatically cleaned up
                upon exit (it's ok since we only deal with a single file) *)
  in
  let stmts = map (fun s ->
      if not norm then s
      else begin
        let f = match lang with
          | M.Tptp -> Term.map Normalize.Tptp.mapper
          | M.Smtlib -> Term.map Normalize.Smtlib.mapper
          | _ ->
            Format.eprintf "Normalization is only allowed for tptp and smtlib currently@.";
            exit 2
        in
        Statement.normalize f s
      end
    ) stmts in
  if print then
    begin match input with
      | `File f -> Format.printf "reading: %s@." f
      | `Stdin _ -> Format.printf "reading: stdin@."
    end;
  if print then
    Format.printf "guessed format : %s@." (M.string_of_language lang);
  let ok = fold stmts (fun s ->
      if print then
        Format.fprintf Format.std_formatter "%a@." Statement.print s
    ) true in
  if ok then
    Format.printf "ok@."
  else
    exit 2


