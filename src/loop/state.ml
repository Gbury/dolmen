
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Exceptions *)
(* ************************************************************************* *)

exception File_not_found of
    Dolmen.ParseLocation.t option * string * string

exception Input_lang_changed of
    Parser.language * Parser.language

(* Warning and error printers *)
(* ************************************************************************* *)

let pp_loc fmt o =
  match o with
  | None -> ()
  | Some loc ->
    Format.fprintf fmt "%a:@ " Dolmen.ParseLocation.fmt loc

let error ?loc _ format =
  Format.kfprintf (fun _ -> exit 1) Format.err_formatter
    ("@[<v>%a%a @[<hov>" ^^ format ^^ "@]@]@.")
    pp_loc loc
    Fmt.(styled `Bold @@ styled (`Fg (`Hi `Red)) string) "Error"

let warn ?loc st format =
  Format.kfprintf (fun _ -> st) Format.err_formatter
    ("@[<v>%a%a @[<hov>" ^^ format ^^ "@]@]@.")
    pp_loc loc
    Fmt.(styled `Bold @@ styled (`Fg (`Hi `Magenta)) string) "Warning"

(* Type information to close the state type in the typer functor *)
(* ************************************************************************* *)

module Aux = struct
  type solve_st = unit
  let warn = warn
end

(* Full state *)
(* ************************************************************************* *)

module Make(T : Typer_intf.T) = struct
  open Dolmen.State

  type t = (Parser.language, T.type_st, unit) Dolmen.State.state

  let warn = warn
  let error = error

  let start _ = ()
  let stop _ = ()

  let file_not_found ?loc ~dir ~file =
    raise (File_not_found (loc, dir, file))

  let set_lang_aux t l =
    let t = Dolmen.State.set_lang t l in
    match l with
    | Parser.Alt_ergo ->
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

end
