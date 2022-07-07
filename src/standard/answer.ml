
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Type definitions *)
type term = Term.t
type location = Loc.t
type defs = Statement.defs

type descr =
  | Unsat
  | Sat of defs list option
  | Error of string

type t = {
  id : Id.t option;
  descr : descr;
  attrs : term list;
  loc : location;
}

let pp_defs =
  Statement.print_group Statement.print_def

let print_model_opt fmt = function
  | None -> Format.fprintf fmt "@[<h>no model@]"
  | Some l ->
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    Format.fprintf fmt "%a"
      (Format.pp_print_list ~pp_sep pp_defs) l

let print_descr fmt = function
  | Unsat -> Format.fprintf fmt "unsat"
  | Sat model_opt ->
    Format.fprintf fmt "@[<hv 2>sat:@ %a@]" print_model_opt model_opt
  | Error message ->
    Format.fprintf fmt "@[<hv 2>error:@ @[<hov>%a@]@]"
      Format.pp_print_text message

let print_attrs fmt = function
  | [] -> ()
  | l ->
    Format.fprintf fmt "@[<hov>{ %a }@]@ "
      (Format.pp_print_list Term.print) l

let print fmt = function { descr; attrs; _ } ->
  Format.fprintf fmt "%a%a" print_attrs attrs print_descr descr


let mk ?id ?(loc= Loc.no_loc) ?(attrs=[]) descr =
  { id; descr; loc; attrs; }

let fun_def ?loc id vars params ret_ty body : defs =
  Statement.group ~recursive:false [
    Statement.def ?loc id ~vars ~params ret_ty body
  ]

let funs_def_rec ?loc l =
  let contents = List.map (fun (id, vars, params, ret_ty, body) ->
      Statement.def ?loc id ~vars ~params ret_ty body
    ) l in
  Statement.group ~recursive:true contents

let sat ?loc model =
  mk ?loc (Sat model)

let unsat ?loc () =
  mk ?loc Unsat

let error ?loc message =
  mk ?loc (Error message)

