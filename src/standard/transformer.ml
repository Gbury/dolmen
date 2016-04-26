
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
    fun () ->
      try
        Parser.input Lexer.token lexbuf
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

end

