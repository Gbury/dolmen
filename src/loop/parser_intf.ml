
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

module type S = sig

  type state
  (** The type of state for a whole pipeline *)

  type 'a key
  (** Keys into the state. *)

  val syntax_error_ref : bool key
  (** A key to determine whether to print a syntax error identifier/reference
      in Syntax error messages. Mainly useful when debugging syntax error messages. *)

  val interactive_prompt : (state -> string option) key
  (** The interactive prompt key. This is used to determine whether we are parsing
      in interactive mode, and if so, what prelude to print before parsing each
      toplevel phrase. *)

  val interactive_prompt_lang : state -> string option
  (** A standard implementation for the interactive prompt key/function. When the
      logic input is set to `Stdin, it will return a prompt string prelude that
      contains the currently set input language. *)

  val init :
    ?syntax_error_ref:bool ->
    ?interactive_prompt:(state -> string option) ->
    state -> state
  (** Init a state with all the relevant keys for this pipeline.
      @param syntax_error_ref : false by default.
      @param interactive_prompt : does nothing by default.
  *)

  val parse_logic :
    ?preludes:Logic.language State.file list -> Logic.language State.file ->
    (state -> state * Dolmen.Std.Statement.t option)
  (** Parsing function. Builds a statement generator from a file.

      The prelude files in [preludes] must not have a [`Stdin] source, and are
      parsed first. They may have a different language than the main file, and
      do not influence language detection for the main file.

      @before 0.9 The type was more complicated.
      @before 0.9 Prelude files were not supported, and the [preludes] argument
      did not exist. *)

  val parse_response :
    Dolmen.Std.Answer.t list -> state -> Response.language State.file ->
    state * (state -> state * Dolmen.Std.Answer.t option)
  (** Parsing function. Reads a list of prelude statements, and the state and
      returns a tuple of the new state (including the detected input language),
      together with a statement generator. *)

  val expand : state -> Dolmen.Std.Statement.t ->
    state * [ `Ok | `Gen of (state -> state -> state) *
                              (state -> state * Dolmen.Std.Statement.t option) ]
  (** Expand statements (such as includes). Returns the new state, and either:
      - [ `Ok ], which means the statement can be propagated as is
      - [ `Gen (flat, g) ], if the statement expands into a generator [g]. The bool [flat]
        indicates wether the statements in [g] should be treated as a single group of
        statements (with regards to timeouts, etc...), or as a list of independant statements
        (each with its own timeout...). *)

end
