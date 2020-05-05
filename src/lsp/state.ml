
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

include Dolmen.State

(* Exceptions *)
(* ************************************************************************* *)

exception Missing_smtlib_logic

exception File_not_found of
    Dolmen.ParseLocation.t option * string * string

exception Input_lang_changed of
    Dolmen_loop.Parser.language * Dolmen_loop.Parser.language


(* Module Instantiation *)
(* ************************************************************************* *)

module Aux = struct

  type solve_st = Diagnostic.t list

  let missing_smtlib_logic () = raise Missing_smtlib_logic

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
  Format.kasprintf (fun msg ->
      let d = Diagnostic.error ~loc msg in
      add_diag d t
    ) ("@[<h>" ^^ format ^^ "@]")

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

