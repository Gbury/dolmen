
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module Make
    (Loc : ParseLocation.S)
    (Ty : sig type token type statement end)
    (Lex : Lex_intf.S with type token := Ty.token)
    (Parse : Parse_intf.S with type token := Ty.token and type statement := Ty.statement) = struct

  include Ty

  module Lexer = Lex
  module Parser = Parse

  let parse_file file =
    let lexbuf = ParseLocation.mk_lexbuf (`File file) in
    try
      Parser.file Lexer.token lexbuf
    with
    | Parser.Error ->
      let pos = Loc.of_lexbuf lexbuf in
      raise (Loc.Syntax_error (pos, ""))
    | Lexer.Error ->
      let pos = Loc.of_lexbuf lexbuf in
      raise (Loc.Lexing_error (pos, Lexing.lexeme lexbuf))
    | _ as e ->
      let pos = Loc.of_lexbuf lexbuf in
      raise (Loc.Uncaught (pos, e))

  let parse_input i =
    let lexbuf = ParseLocation.mk_lexbuf i in
    let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier
        Lexer.token lexbuf in
    let loop = Parse.MenhirInterpreter.loop supplier in
    let aux () =
      begin match loop (Parse.Incremental.input Lexing.(lexbuf.lex_curr_p)) with
        | res -> res
        | exception Parser.Error ->
          let pos = Loc.of_lexbuf lexbuf in
          Line.consume lexbuf;
          raise (Loc.Syntax_error (pos, ""))
        | exception Lexer.Error ->
          let pos = Loc.of_lexbuf lexbuf in
          Line.consume lexbuf;
          raise (Loc.Lexing_error (pos, Lexing.lexeme lexbuf))
        | exception e ->
          let pos = Loc.of_lexbuf lexbuf in
          Line.consume lexbuf;
          raise (Loc.Uncaught (pos, e))
      end
    in
    aux

end

