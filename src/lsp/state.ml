
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

include Dolmen_loop.State

(* Type definition *)
(* ************************************************************************* *)

type solve_state = Diagnostic.t list
type t = solve_state state

(* Warnings *)
(* ************************************************************************* *)

let add_diag d (st : t) =
  { st with solve_state = d :: st.solve_state; }

let full_loc t = function
  | Some full ->
    Dolmen.Std.Loc.full_loc full
  | None ->
    let file = input_file_loc t in
    Dolmen.Std.Loc.loc file Dolmen.Std.Loc.no_loc

let warn ?loc t format =
  let loc = full_loc t loc in
  Format.kasprintf (fun msg ->
      let d = Diagnostic.warn ~loc msg in
      add_diag d t) format

let error ?code:_ ?loc t format =
  let loc = full_loc t loc in
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

