
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Generic type for parsers *)

module type S = sig

  type token

  type statement

  exception Error

  (* Standard parsing of the whole file *)

  val file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> statement list

  val input : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> statement option

  (* Needed for incremental parsing of input *)

  module MenhirInterpreter : sig

    include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
      with type token = token

  end

  module Incremental : sig

    val input : Lexing.position -> (statement option) MenhirInterpreter.checkpoint

  end

end

