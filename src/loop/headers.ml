
(* Header check

   This module defines a pipe to check the presence of headers in
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
      | { Id.ns = Attr; Id.name = Simple ":smt-lib-version"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Value Real; Id.name = Simple version; }; _ } ] ->
            if check_version_number version then
                Ok (Lang_version, version)
            else
              Error (loc, ":smt-lib-version number must be in the form 2.X")
          | [] -> Error (loc, "empty value for :smt-lib-version")
          | { Ast.loc; _ } :: _ -> Error (loc, "Expected a version number")
        end

      (* Problem source *)
      | { Id.ns = Attr; Id.name = Simple ":source"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Attr; Id.name = Simple descr }; _ } ] ->
            Ok (Problem_source, descr)
          | [] -> Error (loc, "empty value for :source")
          | { Ast.loc; _ } :: _ -> Error (loc, "Expected a single symbol as description")
        end

      (* Problem license *)
      | { Id.ns = Attr; Id.name = Simple ":license"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Value String; Id.name = Simple license }; _ } ] ->
            Ok (Problem_license, license)
          | [] -> Error (loc, "empty value for :license")
          | { Ast.loc; _ } :: _ -> Error (loc, "Expected a single string in quotes")
        end

      (* Problem category *)
      | { Id.ns = Attr; Id.name = Simple ":category"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.ns = Value String;
              Id.name = Simple (("crafted"|"random"|"industrial") as category)
            }; _ }; ] ->
            Ok (Problem_category, category)
          | [] -> Error (loc, "empty value for :category")
          | { Ast.loc; _ } :: _ ->
            Error (loc, {|Expected "crafted", "random", or "industrial" (in quotes)|})
        end


      (* Problem status *)
      | { Id.ns = Attr; Id.name = Simple ":status"; } ->
        begin match args with
          | [ { Ast.term = Ast.Symbol {
              Id.name = Simple (("sat"|"unsat"|"unknown") as status) ; _ }; _ }; ] ->
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

(* Header errors & warnings *)
(* ************************************************************************ *)

let code =
  Code.create
    ~category:"Header"
    ~descr:"on header errors"

let missing_header_error =
  Report.Error.mk ~code ~mnemonic:"header-missing"
    ~message:(fun fmt (lang, missing) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "The following header fields are missing: %a"
          (Format.pp_print_list ~pp_sep (Field.print ?lang)) missing)
    ~name:"Missing header statement" ()

let invalid_header_value_error =
  Report.Error.mk ~code ~mnemonic:"header-invalid-value"
    ~message:(fun fmt (field, lang, msg) ->
        Format.fprintf fmt "Invalid value for header %a: %s"
          (Field.print ?lang) field msg)
    ~name:"Invalid header value" ()

let bad_header_payload =
  Report.Error.mk ~code ~mnemonic:"header-bad-payload"
    ~message:(fun fmt msg ->
        Format.fprintf fmt "Could not parse the header: %s" msg)
    ~name:"Incorrect header payload" ()

let empty_header_field =
  Report.Warning.mk ~code ~mnemonic:"empty-header-field"
    ~message:(fun fmt (lang, l) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt
          "The following header fields are missing and thus \
           default values will be assumed: %a"
          (Format.pp_print_list ~pp_sep (Field.print ?lang)) l)
    ~name:"Header field with a missing value" ()


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

module type S = Headers_intf.S

module Make(State : State.S) = struct

  let pipe = "Headers"

  let header_check : bool State.key =
    State.create_key ~pipe "header_check"

  let header_state : t State.key =
    State.create_key ~pipe "header_state"

  let header_licenses : string list State.key =
    State.create_key ~pipe "header_licenses"

  let header_lang_version : string option State.key =
    State.create_key ~pipe "header_lang_version"

  let init
      ~header_check:header_check_value
      ?header_state:(header_state_value=empty)
      ~header_licenses:header_licenses_value
      ~header_lang_version:header_lang_version_value
      st =
    st
    |> State.set header_check header_check_value
    |> State.set header_state header_state_value
    |> State.set header_licenses header_licenses_value
    |> State.set header_lang_version header_lang_version_value

  (* Final check for headers *)

  let check_wanted st h =
    let lang = (State.get State.logic_file st).lang in
    let wanted =
      match lang with
      | Some Logic.Smtlib2 _ -> smtlib2_wanted
      | _ -> []
    in
    match List.filter (fun f -> not (mem h f)) wanted with
    | [] -> st
    | missing ->
      State.warn st empty_header_field (lang, missing)

  let check_required st h =
    let lang = (State.get State.logic_file st).lang in
    let required =
      match lang with
      | Some Logic.Smtlib2 _ -> smtlib2_required
      | _ -> []
    in
    match List.filter (fun f -> not (mem h f)) required with
    | [] -> st
    | missing -> State.error st missing_header_error (lang, missing)

  let check st =
    if not (State.get header_check st) then st
    else begin
      let h = State.get header_state st in
      let st = check_wanted st h in
      let st = check_required st h in
      st
    end


  (* Incremental checks and construction of the header set *)

  let error st loc err param =
    let file = (State.get State.logic_file st).loc in
    let loc : Dolmen.Std.Loc.full = { file; loc; } in
    State.error ~loc st err param

  let invalid_header_value st loc field msg =
    let lang = (State.get State.logic_file st).lang in
    error st loc invalid_header_value_error (field, lang, msg)

  let check_header st loc field value =
    match (field : Field.t) with
    | Lang_version ->
      begin match State.get header_lang_version st with
        | None -> st
        | Some v ->
          if v = value then st
          else invalid_header_value st loc Lang_version
              (Format.sprintf "language version must be: %s" v)
      end
    | Problem_license ->
      begin match State.get header_licenses st with
        | [] -> st
        | allowed ->
          if List.mem value allowed then st
          else invalid_header_value st loc Problem_license
              "this license is not in the list of allowed licenses"
      end
    | _ -> st

  let inspect st c =
    if not (State.get header_check st) then (st, c)
    else begin
      let lang = (State.get State.logic_file st).lang in
      let h = State.get header_state st in
      let st =
        match (c : Dolmen.Std.Statement.t) with
        | { descr = Set_info t; loc; _ } ->
          begin match Field.parse ?lang t with
            | Not_a_header -> st
            | Error (loc, msg) ->
              error st loc bad_header_payload msg
            | Ok (field, value) ->
              let st = check_header st loc field value in
              let st = State.set header_state (set h field value) st in
              st
          end
        | { descr = Prove _; loc; _ } ->
          if mem h Problem_status then
            State.set header_state (remove h Problem_status) st
          else
            error st loc missing_header_error (lang, [Problem_status])
        | _ -> st
      in
      st, c
    end

end

