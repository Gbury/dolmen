
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

include Dolmen.State

(* Exceptions *)
(* ************************************************************************* *)

exception File_not_found of
    Dolmen.ParseLocation.t option * string * string

exception Input_lang_changed of
    Dolmen_loop.Parser.language * Dolmen_loop.Parser.language


(* Module Instantiation *)
(* ************************************************************************* *)

module Aux = struct
  type solve_st = Diagnostic.t list
end

module Typer = Dolmen_loop.Typer.Make(Aux)

(* Type definition *)
(* ************************************************************************* *)

type lang = Dolmen_loop.Parser.language
type typer_st = Typer.type_st
type solver_st = Diagnostic.t list

type t = (lang, typer_st, solver_st) Dolmen.State.state

(* Warnings *)
(* ************************************************************************* *)

let add_diag d (st : t) =
  { st with solve_state = d :: st.solve_state; }

let warn t loc msg =
  let d = Diagnostic.warn ~loc msg in
  add_diag d t

let error t loc format =
  (* Flush the str formatter to clear any unflushed leftover *)
  let _ = Format.flush_str_formatter () in
  (* Set the str formatter out functions to not emit newline characters *)
  let str_out_functions =
    Format.pp_get_formatter_out_functions Format.str_formatter ()
  in
  let () =
    Format.pp_set_formatter_out_functions Format.str_formatter {
      str_out_functions with
      out_newline = (fun () -> str_out_functions.out_spaces 1);
      out_indent = (fun _ -> ());
    }
  in
  (* Print the error message *)
  Format.kfprintf (fun _ ->
      let msg = Format.flush_str_formatter () in
      let d = Diagnostic.error ~loc msg in
      add_diag d t
    ) Format.str_formatter ("@[<h>" ^^ format ^^ "@]")

(* Necessary functions *)
(* ************************************************************************* *)

let start _ = ()
let stop _ = ()

let file_not_found ?loc ~dir ~file =
  raise (File_not_found (loc, dir, file))

let set_lang_aux t l =
  let t = Dolmen.State.set_lang t l in
  match l with
  | Dolmen_loop.Parser.Alt_ergo ->
    Dolmen.State.set_mode t `Full
  | _ -> t

let set_lang t l =
  match t.input_lang with
  | None -> set_lang_aux t l
  | Some l' ->
    if l = l'
    then set_lang_aux t l
    else raise (Input_lang_changed (l', l))

let run_typecheck st = st.type_check

