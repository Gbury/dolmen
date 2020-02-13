
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Exceptions *)
(* ************************************************************************* *)

exception Missing_smtlib_logic

exception Input_lang_changed of Parser.language * Parser.language

exception File_not_found of Dolmen.ParseLocation.t option * string * string

(* State for the typer *)
(* ************************************************************************* *)

module For_typer = struct

  type solve_st = unit

  let missing_smtlib_logic () = raise Missing_smtlib_logic

end

(* Full state *)
(* ************************************************************************* *)

module Make(T : Typer_intf.T) = struct
  open Dolmen.State

  let pp_loc fmt o =
    match o with
    | None -> ()
    | Some loc ->
      Format.fprintf fmt "%a:@ " Dolmen.ParseLocation.fmt loc

  let error ?loc _ format =
    Format.kfprintf (fun _ -> exit 1) Format.err_formatter
      ("%a @[<hov>%a" ^^ format ^^ "@]@.")
      Fmt.(styled (`Fg (`Hi `Red)) string) "Error" pp_loc loc

  let warn_aux st loc msg =
    Format.eprintf "@[<hov>%a%a %s@]"
      pp_loc loc
      Fmt.(styled (`Fg (`Hi `Magenta)) string) "Warning"
      msg;
    st

  let warn st loc msg = warn_aux st (Some loc) msg

  type solve_st = unit
  type type_st = T.ty_state
  type t = (Parser.language, type_st, solve_st) Dolmen.State.state

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
          warn_aux t None
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
