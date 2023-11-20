
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Types *)
(* ************************************************************************* *)

type _ kind =
  | Error : { id : int; } -> [> `Error ] kind
  | Warning : { id : int; } -> [> `Warning ] kind

type ('kind, 'param) aux = {

    (* report identification *)
    kind : 'kind kind;
    code : Code.t;

    (* cli interaction *)
    mnemonic : string;

    (* short message printing *)
    message : Format.formatter -> 'param -> unit;
    mutable hints : ('param -> (Format.formatter -> unit) option) list;

    (* long documentation *)
    name : string;
    doc : Format.formatter -> unit;

  }

type any = Any : (_, _) aux -> any
type 'a error = ([ `Error ], 'a) aux
type 'a warning = ([ `Warning ], 'a) aux
type any_error = Any_err : _ error -> any_error
type any_warning = Any_warn : _ warning -> any_warning


(* Common functions *)
(* ************************************************************************* *)

let code report = report.code
let name report = report.name
let mnemonic report = report.mnemonic

let print_doc fmt report =
  Format.fprintf fmt "%t" report.doc

let print fmt (report, param) =
  report.message fmt param

let print_hints fmt (report, param) =
  List.iter (fun hint ->
      match hint param with
      | None -> ()
      | Some pp ->
        Format.fprintf fmt "@\n@[<hov 2>Hint: %t@]" pp
    ) report.hints



(* Creation and fetching *)
(* ************************************************************************* *)

let locked = ref false
let error_count = ref 0
let warning_count = ref 0
let mnemonics_table = Hashtbl.create 113

let no_doc fmt =
  Format.fprintf fmt "No documentation yet"

let mk_aux
    ~kind ~code
    ~mnemonic
    ~message ?(hints=[])
    ~name ?(doc=no_doc) () =
  assert (not !locked && not (Hashtbl.mem mnemonics_table mnemonic));
  let res = { code; kind; mnemonic; message; hints; name; doc; } in
  Hashtbl.add mnemonics_table mnemonic (Any res);
  res

let mk_error
    ?(code=Code.bug) ~mnemonic ~message ?hints ~name ?doc () : _ error =
  let id = incr error_count; !error_count in
  mk_aux ~kind:(Error { id; }) ~code ~mnemonic ~message ?hints ~name ?doc ()

let mk_warning
    ?(code=Code.bug) ~mnemonic ~message ?hints ~name ?doc () : _ warning =
  let id = !warning_count in
  incr warning_count;
  mk_aux ~kind:(Warning { id; }) ~code ~mnemonic ~message ?hints ~name ?doc ()

let add_hint t hint =
  let new_hint _ = Some (hint) in
  t.hints <- new_hint :: t.hints


(* Warnings interface *)
(* ************************************************************************* *)

module Warning = struct

  type 'a t = 'a warning

  let mk = mk_warning
  let code = code
  let name = name
  let mnemonic = mnemonic
  let print = print
  let print_doc = print_doc
  let print_hints = print_hints

  module Status = struct

    type t =
      | Disabled
      | Enabled
      | Fatal

    let print fmt = function
      | Disabled -> Format.fprintf fmt "disabled"
      | Enabled -> Format.fprintf fmt "enabled"
      | Fatal -> Format.fprintf fmt "fatal"

    let to_string t =
      Format.asprintf "%a" print t

    let merge s s' =
      match s, s' with
      | Disabled, Disabled -> Disabled
      | _, Fatal | Fatal, _ -> Fatal
      | _, _ -> Enabled

  end

end

(* Error interface *)
(* ************************************************************************* *)

module Error = struct

  type 'a t = 'a error

  let mk = mk_error
  let code = code
  let name = name
  let mnemonic = mnemonic
  let print = print
  let print_doc = print_doc
  let print_hints = print_hints

  let user_interrupt : _ t =
    mk ~code:Code.limit ~mnemonic:"user-interrupt"
      ~message:(fun fmt () ->
          Format.fprintf fmt "User Interrupt")
      ~name:"User Interrupt" ()

  let timeout : _ t =
    mk ~code:Code.limit ~mnemonic:"timeout"
      ~message:(fun fmt () ->
          Format.fprintf fmt "Time limit reached")
      ~name:"Timeout" ()

  let spaceout : _ t =
    mk ~code:Code.limit ~mnemonic:"spaceout"
      ~message:(fun fmt () ->
          Format.fprintf fmt "Memory limit reached")
      ~name:"Out of space" ()

  let internal_error =
    mk ~code:Code.bug ~mnemonic:"internal-error"
      ~message:(fun fmt t ->
          Format.fprintf fmt "@[<v>%t@ Please report upstream, ^^@]" t)
      ~name:"Internal Error" ()

  let uncaught_exn =
    mk ~code:Code.bug ~mnemonic:"uncaught-exn"
      ~message:(fun fmt (exn, bt) ->
          Format.fprintf fmt
            "Uncaught exception:@\n%s%a%s"
            (Printexc.to_string exn)
            (if Printexc.backtrace_status ()
             then Format.pp_print_newline
             else (fun _ _ -> ())) ()
            (if Printexc.backtrace_status ()
             then Printexc.raw_backtrace_to_string bt
             else ""))
      ~name:"Uncaught exception" ()

end

(* Reports *)
(* ************************************************************************* *)

module T = struct

  type all = [ `All ]
  type err = [ `Error of any_error ]
  type warn = [ `Warning of any_warning ]

  type t = [ all | err | warn ]

  let name = function
    | `All -> "All warnings"
    | `Error Any_err e -> name e
    | `Warning Any_warn w -> name w

  let mnemonic = function
    | `All -> "all"
    | `Error Any_err e -> mnemonic e
    | `Warning Any_warn w -> mnemonic w

  let kind = function
    | `All -> "group"
    | `Error _ -> "error"
    | `Warning _ -> "warning"

  let category = function
    | `All -> "General"
    | `Error Any_err e -> Code.category (code e)
    | `Warning Any_warn w -> Code.category (code w)

  let doc = function
    | `Error Any_err e -> e.doc
    | `Warning Any_warn w -> w.doc
    | `All ->
      Format.dprintf "%a" Format.pp_print_text
        "The group of all warnings. Its main use is when specifying \
         a set of warnings to enable/disable/make fatal when using the \
         '-w' option of Dolmen."

  let find_mnemonic = function
    | "all" -> Some `All
    | mnemonic ->
      begin match Hashtbl.find_opt mnemonics_table mnemonic with
        | Some (Any ({ kind = Warning _; _ } as w)) ->
          Some (`Warning (Any_warn w))
        | Some (Any ({ kind = Error _; _ } as e)) ->
          Some (`Error (Any_err e))
        | None -> None
      end

  let list () =
    Hashtbl.fold (fun _ any acc ->
        let elt =
          match any with
          | Any ({ kind = Warning _; _ } as w) ->
            `Warning (Any_warn w)
          | Any ({ kind = Error _; _ } as e) ->
            `Error (Any_err e)
        in
        elt :: acc
      ) mnemonics_table [`All]

end

(* Configuration *)
(* ************************************************************************* *)

module Conf = struct

  type t = {
    warnings : Warning.Status.t array;
  }

  let mk ~default =
    locked := true;
    { warnings = Array.make !warning_count default; }

  let _copy t =
    { warnings = Array.copy t.warnings; }

  let _get conf i =
    conf.warnings.(i)

  let _set_inplace conf i status =
    conf.warnings.(i) <- status

  let _update_inplace conf i status =
    _set_inplace conf i (Warning.Status.merge status (_get conf i))

  let status conf (warning : _ warning) =
    let (Warning { id; }) = warning.kind in
    _get conf id

  let update f param conf = function
    | `All ->
      for i = 0 to Array.length conf.warnings - 1 do
        f conf i param
      done;
      conf
    | `Warning (Any_warn { kind = Warning { id; }; _}) ->
      let conf = _copy conf in
      f conf id param;
      conf

  let fatal conf w = update _update_inplace Fatal conf w
  let enable conf w = update _update_inplace Enabled conf w
  let disable conf w = update _set_inplace Disabled conf w
  let set_enabled conf w = update _set_inplace Enabled conf w

end
