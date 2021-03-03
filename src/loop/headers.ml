
(* Header check

   This module defines a some pipes to check the presence of headers in
   an input file. Headerts are there to provide meta-data about problems,
   such as version, source, classification, and even satisifability status
   in some cases.
   Note that this does *not* include the checking of constraints between
   successivestatements such as what smtlib specifies, see flow.ml


   The way this check is implemented/done:
   - a number of meta-data fields are defines in module [Field],
     together with ways to parse/print them.
   - then each language defines its list of headers that are:
     + required (causing an error if they are absent)
     + wanted (causing a warning if they are absent)
   - additionally, some checks are performed on the values of some
     headers, particularly lang_version and license, which are
     provided by the state (and usually controlled by the command
     line, as there are not reasonable defaults for them).
   - finally the pipe accumulates header (parsing them using the
     functions from the [Field] module, and a checking function is
     provided to check the presence of headers at the end of the
     file processing.

   Note that the current way means that headers can theoretically be
   present anywhere in the file, rather than at the beginning.
*)

(* Exit code for header errors *)
(* ************************************************************************ *)

let code = Code.create "on header errors"


(* Header fields *)
(* ************************************************************************ *)

module Field = struct

  type t =
    | Lang_version
    | Problem_version
    | Problem_source
    | Problem_license
    | Problem_category
    | Problem_status

  let equal = (=)
  let compare = compare
  let hash = Hashtbl.hash


  (* Correspondance between fields and their names in languages *)
  let name lang field =
    match lang, field with
    | Some Logic.Smtlib2 _, Lang_version -> ":smt-lib-version"
    | Some Logic.Smtlib2 _, Problem_source -> ":source"
    | Some Logic.Smtlib2 _, Problem_license -> ":license"
    | Some Logic.Smtlib2 _, Problem_category -> ":category"
    | Some Logic.Smtlib2 _, Problem_status -> ":status"
    | _, Lang_version -> "Language version"
    | _, Problem_version -> "Problem version"
    | _, Problem_source -> "Problem source"
    | _, Problem_license -> "Problem license"
    | _, Problem_category -> "Problem_category"
    | _, Problem_status -> "Problem status"

  let print ?lang fmt field =
    Format.fprintf fmt "%s" (name lang field)


  (* Parse an attribute into an (optional) field and value. *)

  module Id = Dolmen.Std.Id
  module Loc = Dolmen.Std.Loc
  module Ast = Dolmen.Std.Term

  type res =
    | Ok of t * string
    | Error of Loc.t * string
    | Not_a_header

  module Smtlib2 = struct

    let check_version_number version =
      if String.length version >= 3 &&
         String.sub version 0 2 = "2." then begin
        try
          let s = String.sub version 2 (String.length version - 2) in
          let _ = int_of_string s in
          true
        with Failure _ ->
          false
      end else
        false


    let rec parse = function
      | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, args); loc; _ } ->
        parse_aux loc s args
      | _ ->
        Not_a_header

    and parse_aux loc s args =
      match s with

      (* Language version *)
      | { Id.ns = Id.Attr; Id.name = ":smt-lib-version"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Id.Value Id.Real; Id.name = version }; _ } ] ->
            if check_version_number version then
                Ok (Lang_version, version)
            else
              Error (loc, ":smt-lib-version number must be in the form 2.X")
          | [] -> Error (loc, "empty value for :smt-lib-version")
          | { Ast.loc; _ } :: _ -> Error (loc, "Expected a version number")
        end

      (* Problem source *)
      | { Id.ns = Id.Attr; Id.name = ":source"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Id.Attr; Id.name = descr }; _ } ] ->
            Ok (Problem_source, descr)
          | [] -> Error (loc, "empty value for :source")
          | { Ast.loc; _ } :: _ -> Error (loc, "Expected a single symbol as description")
        end

      (* Problem license *)
      | { Id.ns = Id.Attr; Id.name = ":license"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Id.Value Id.String; Id.name = license }; _ } ] ->
            Ok (Problem_license, license)
          | [] -> Error (loc, "empty value for :license")
          | { Ast.loc; _ } :: _ -> Error (loc, "Expected a single string in quotes")
        end

      (* Problem category *)
      | { Id.ns = Id.Attr; Id.name = ":category"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Id.Value Id.String;
              Id.name = (("crafted"|"random"|"industrial") as category) }; _ } ] ->
            Ok (Problem_category, category)
          | [] -> Error (loc, "empty value for :category")
          | { Ast.loc; _ } :: _ ->
            Error (loc, {|Expected "crafted", "random", or "industrial" (in quotes)|})
        end


      (* Problem status *)
      | { Id.ns = Id.Attr; Id.name = ":status"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.name = (("sat"|"unsat"|"unknown") as status) ; _ }; _ } ] ->
            Ok (Problem_status, status)
          | _ -> Error (loc, "Expected sat|unsat|unknown")
        end

      (* catch-all *)
      | _ ->
        Not_a_header

  end

  let parse ?lang t =
    match lang with
    | Some Logic.Smtlib2 _ -> Smtlib2.parse t
    | _ -> Not_a_header


end

(* Headers *)
(* ************************************************************************ *)

module M = Map.Make(Field)

type t = {
  fields : string M.t;
}

let empty = {
  fields = M.empty;
}

let set h f v = {
  fields = M.add f v h.fields;
}

let remove h f = {
  fields = M.remove f h.fields;
}

let get h f =
  try Some (M.find f h.fields)
  with Not_found -> None

let mem h f =
  M.mem f h.fields

(* Required headers for languages *)
(* ************************************************************************ *)

let smtlib2_required : Field.t list = [
  Lang_version;
  Problem_source;
  Problem_category;
  (* Problem_status is checked for every check-sat *)
]

let smtlib2_wanted : Field.t list = [
  Problem_license;
]


(* Required headers for languages *)
(* ************************************************************************ *)

module Pipe(State : State_intf.Header_pipe
            with type header_state := t)
= struct

  (* Final check for headers *)

  let check_wanted st h =
    let lang = State.input_lang st in
    let wanted =
      match lang with
      | Some Logic.Smtlib2 _ -> smtlib2_wanted
      | _ -> []
    in
    match List.filter (fun f -> not (mem h f)) wanted with
    | [] -> st
    | missing ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      State.warn st "The following header fields are missing and thus \
                     default values will be assumed: %a"
        (Format.pp_print_list ~pp_sep (Field.print ?lang)) missing

  let check_required st h =
    let lang = State.input_lang st in
    let required =
      match lang with
      | Some Logic.Smtlib2 _ -> smtlib2_required
      | _ -> []
    in
    match List.filter (fun f -> not (mem h f)) required with
    | [] -> st
    | missing ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      State.error ~code st "The following header fields are missing: %a"
        (Format.pp_print_list ~pp_sep (Field.print ?lang)) missing

  let check st =
    if not (State.check_headers st) then st
    else begin
      let h = State.header_state st in
      let st = check_wanted st h in
      let st = check_required st h in
      st
    end


  (* Incremental checks and construction of the header set *)

  let error st loc fmt =
    let file = State.input_file_loc st in
    let loc : Dolmen.Std.Loc.full = { file; loc; } in
    State.error ~code ~loc st fmt

  let check_header st loc field value =
    match (field : Field.t) with
    | Lang_version ->
      begin match State.allowed_lang_version st with
        | None -> st
        | Some v ->
          if v = value then st
          else error st loc "This language version must be: %s" v
      end
    | Problem_license ->
      begin match State.allowed_licenses st with
        | [] -> st
        | allowed ->
          if List.mem value allowed then st
          else error st loc "This is not an allowed license"
      end
    | _ -> st

  let inspect st c =
    if not (State.check_headers st) then (st, c)
    else begin
      let lang = State.input_lang st in
      let h = State.header_state st in
      let st =
        match (c : Dolmen.Std.Statement.t) with
        | { descr = Set_info t; loc; _ } ->
          begin match Field.parse ?lang t with
            | Not_a_header -> st
            | Error (loc, msg) -> error st loc "%s" msg
            | Ok (field, value) ->
              let st = check_header st loc field value in
              let st = State.set_header_state st (set h field value) in
              st
          end
        | { descr = Prove _; loc; _ } ->
          if mem h Problem_status then
            State.set_header_state st (remove h Problem_status)
          else
            error st loc "This statement lacks a %s header"
              (Field.name lang Problem_status)
        | _ -> st
      in
      st, c
    end

end

