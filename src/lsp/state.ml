
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

let warn ?(loc=Dolmen.Std.ParseLocation.mk "" 0 0 0 0) t format =
  Format.kasprintf (fun msg ->
      let d = Diagnostic.warn ~loc msg in
      add_diag d t) format


let error ?(loc=Dolmen.Std.ParseLocation.mk "" 0 0 0 0) t format =
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

