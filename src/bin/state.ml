
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

include Dolmen.State

(* Exceptions *)
(* ************************************************************************* *)

exception File_not_found of Dolmen.ParseLocation.t option * string * string

exception Missing_smtlib_logic

exception Input_lang_changed
  of Dolmen_loop.Parse.language * Dolmen_loop.Parse.language

exception Shadowing of
    Dolmen.Id.t * Dolmen_type.Tff.reason * Dolmen_type.Tff.reason

(* Errors *)
(* ************************************************************************* *)

let error st format =
  Format.kfprintf (fun _ -> st) Format.err_formatter
    ("%a @[<hov>" ^^ format ^^ "@]@.")
    Fmt.(styled (`Fg (`Hi `Red)) string) "[ERROR]"

let warn st format =
  Format.kfprintf (fun _ -> st) Format.err_formatter
    ("%a @[<hov>" ^^ format ^^ "@]@.")
    Fmt.(styled (`Fg (`Hi `Magenta)) string) "[WARNING]"

(* Type definition *)
(* ************************************************************************* *)

type t = (Dolmen_loop.Parse.language, unit, unit) Dolmen.State.t

let start _ = ()
let stop _ = ()

let file_not_found ?loc ~dir ~file =
  raise (File_not_found (loc, dir, file))

let set_lang_aux t l =
  let t = Dolmen.State.set_lang t l in
  match l with
  | Dolmen_loop.Parse.Alt_ergo ->
    let old_mode = Dolmen.State.input_mode t in
    let t = Dolmen.State.set_mode t `Full in
    begin match old_mode with
      | Some `Incremental ->
        warn t
          "The Alt-ergo format does not support incremental mode, switching to full mode"
      | _ -> t
    end
  | _ -> t

let set_lang t l =
  match t.input_lang with
  | None -> set_lang_aux t l
  | Some l' ->
    if l = l'
    then set_lang_aux t l
    else raise (Input_lang_changed (l', l))

let run_typecheck st = st.type_check

