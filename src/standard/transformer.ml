
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module Make
    (Loc    : ParseLocation.S)
    (Ty     : sig
       type token
       type statement
       val env : string list
     end)
    (Lex    : Dolmen_intf.Lex.S with type token := Ty.token)
    (Parse  : Dolmen_intf.Parse.S with type token := Ty.token
                                   and type statement := Ty.statement) = struct

  include Ty

  module Lexer = Lex
  module Parser = Parse

  let rec find_env file = function
    | [] -> None
    | var :: r ->
      begin match Sys.getenv var with
        | dir ->
          let f = Filename.concat dir file in
          if Sys.file_exists f then Some f
          else find_env file r
        | exception Not_found ->
          find_env file r
      end

  let find ?(dir="") file =
    if Filename.is_relative file then begin
      let f = Filename.concat dir file in
      if Sys.file_exists f then
        Some f
      else
          find_env file Ty.env
    end else if Sys.file_exists file then
      Some file
    else
      None

  let parse_file file =
    let lexbuf, cleanup = ParseLocation.mk_lexbuf (`File file) in
    try
      let res = Parser.file Lexer.token lexbuf in
      cleanup ();
      res
    with
    | ((Loc.Syntax_error _) as e)
    | ((Loc.Lexing_error _) as e) ->
      let () = cleanup () in
      raise e
    | Parser.Error ->
      let pos = Loc.of_lexbuf lexbuf in
      let () = cleanup () in
      raise (Loc.Syntax_error (pos, ""))
    | Lexer.Error ->
      let pos = Loc.of_lexbuf lexbuf in
      let () = cleanup () in
      raise (Loc.Lexing_error (pos, Lexing.lexeme lexbuf))
    | _ as e ->
      let pos = Loc.of_lexbuf lexbuf in
      let () = cleanup () in
      raise (Loc.Uncaught (pos, e))

  let parse_input i =
    let lexbuf, cleanup = ParseLocation.mk_lexbuf i in
    let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier
        Lexer.token lexbuf in
    let loop = Parse.MenhirInterpreter.loop supplier in
    let aux () =
      begin match loop (Parse.Incremental.input Lexing.(lexbuf.lex_curr_p)) with
        | res -> res
        | exception ((Loc.Syntax_error _) as e) ->
          raise e
        | exception ((Loc.Lexing_error _) as e) ->
          raise e
        | exception Lexer.Error ->
          let pos = Loc.of_lexbuf lexbuf in
          let err = Lexing.lexeme lexbuf in
          Dolmen_line.consume lexbuf;
          raise (Loc.Lexing_error (pos, err))
        | exception Parser.Error ->
          let pos = Loc.of_lexbuf lexbuf in
          Dolmen_line.consume lexbuf;
          raise (Loc.Syntax_error (pos, ""))
        | exception e ->
          let pos = Loc.of_lexbuf lexbuf in
          Dolmen_line.consume lexbuf;
          raise (Loc.Uncaught (pos, e))
      end
    in
    aux, cleanup

end

