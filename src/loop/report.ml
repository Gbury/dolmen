
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
    hints : ('param -> (Format.formatter -> unit) option) list;

    (* long documentation *)
    name : string;
    doc : Format.formatter -> unit;

  }

type any = Any : (_, _) aux -> any
type 'a error = ([ `Error ], 'a) aux
type 'a warning = ([ `Warning ], 'a) aux


(* Common functions *)
(* ************************************************************************* *)

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
  let id = incr warning_count; !warning_count in
  mk_aux ~kind:(Warning { id; }) ~code ~mnemonic ~message ?hints ~name ?doc ()


(* Warnings interface *)
(* ************************************************************************* *)

module Warning = struct

  type 'a t = 'a warning

  let print = print
  let print_hints = print_hints
  let mk = mk_warning
  let code t = t.code

end

(* Error interface *)
(* ************************************************************************* *)

module Error = struct

  type 'a t = 'a error

  let print = print
  let print_hints = print_hints
  let mk = mk_error
  let code t = t.code

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
          Format.fprintf fmt "%t" t)
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

(* Configuration *)
(* ************************************************************************* *)

module Conf = struct

  type status =
    | Disabled
    | Enabled
    | Fatal

  type t = {
    warnings : status array;
  }

  let mk ~default =
    locked := true;
    { warnings = Array.make !warning_count default; }

  let merge s s' =
    match s, s' with
    | Disabled, Disabled -> Disabled
    | _, Fatal | Fatal, _ -> Fatal
    | _, _ -> Enabled

  let _copy t =
    { warnings = Array.copy t.warnings; }

  let _get conf i =
    conf.warnings.(i)

  let _set_inplace conf i status =
    conf.warnings.(i) <- status

  let _update_inplace conf i status =
    _set_inplace conf i (merge status (_get conf i))

  let status conf (warning : _ warning) =
    let (Warning { id; }) = warning.kind in
    _get conf id

  let update f param conf (warning : _ warning) =
    let conf = _copy conf in
    let (Warning { id; }) = warning.kind in
    f conf id param;
    conf

  let fatal conf w = update _update_inplace Fatal conf w
  let enable conf w = update _update_inplace Enabled conf w
  let disable conf w = update _set_inplace Disabled conf w
  let set_enabled conf w = update _set_inplace Enabled conf w

  let update_mnemonic f status conf = function
    | "all" ->
      for i = 0 to Array.length conf.warnings do
        f conf i status
      done;
      Ok conf
    | mnemonic ->
      begin match Hashtbl.find_opt mnemonics_table mnemonic with
        | Some (Any { kind = Warning { id; }; _ }) ->
          f conf id status;
          Ok conf
        | Some (Any { kind = Error _; _ }) ->
          Error `Error_mnemonic
        | None ->
          Error `Unknown_mnemonic
      end

  let fatal_mnemonic conf =
    update_mnemonic _update_inplace Fatal (_copy conf)
  let enable_mnemonic conf =
    update_mnemonic _update_inplace Enabled (_copy conf)
  let disable_mnemonic conf =
    update_mnemonic _set_inplace Disabled (_copy conf)
  let set_enabled_mnemonic conf =
    update_mnemonic _set_inplace Enabled (_copy conf)

end
