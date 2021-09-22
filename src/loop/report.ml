
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Types *)
(* ************************************************************************* *)

type _ kind =
  | Error : {
      id : int;
      code : Code.t;
    } -> [> `Error ] kind
  | Warning : {
      id : int;
      code : Code.t; (* for when the warning is fatal *)
      mutable enabled : bool;
      mutable fatal : bool;
    } -> [> `Warning ] kind

type ('kind, 'param) aux = {

    (* report identification *)
    kind : 'kind kind;

    (* cli interaction *)
    mnemonic : string;

    (* short message printing *)
    message : Format.formatter -> 'param -> unit;
    hints : ('param -> (Format.formatter -> unit) option) list;

    (* long documentation *)
    name : string;
    doc : Format.formatter -> unit;

  }


type 'a error = ([ `Error ], 'a) aux
type any_error = Any_error : _ error -> any_error

type 'a warning = ([ `Warning ], 'a) aux
type any_warning = Any_warning : _ warning -> any_warning

type any = Any : (_, _) aux -> any


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

let error_table = Hashtbl.create 113
let warning_table = Hashtbl.create 113
let mnemonics_table = Hashtbl.create 113

let no_doc fmt =
  Format.fprintf fmt "No documentation yet"

let mk_aux
    ~kind ~mnemonic
    ~message ?(hints=[])
    ~name ?(doc=no_doc) () =
  assert (not @@ Hashtbl.mem mnemonics_table mnemonic);
  let res = { kind; mnemonic; message; hints; name; doc; } in
  Hashtbl.add mnemonics_table mnemonic (Any res);
  res

let error_count = ref 0

let mk_error ?(code=Code.bug) ~mnemonic ~message ?hints ~name ?doc () : _ error =
  let id = incr error_count; !error_count in
  let kind = Error { id; code } in
  let res = mk_aux ~kind ~mnemonic ~message ?hints ~name ?doc () in
  Hashtbl.add error_table id (Any_error res);
  res

let warning_count = ref 0

let mk_warning
    ?(code=Code.bug) ?(enabled = true) ?(fatal = false)
    ~mnemonic ~message ?hints ~name ?doc () : _ warning =
  let id = incr warning_count; !warning_count in
  let kind = Warning { id; code; enabled; fatal; } in
  let res = mk_aux ~kind ~mnemonic ~message ?hints ~name ?doc () in
  Hashtbl.add warning_table id (Any_warning res);
  res


(* Error interface *)
(* ************************************************************************* *)

module Error = struct

  type 'a t = 'a error

  let print = print
  let print_hints = print_hints
  let mk = mk_error
  let code ({ kind = Error { code; _ }; _ }  : _ t) = code

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

(* Warnings interface *)
(* ************************************************************************* *)

module Warning = struct

  type 'a t = 'a warning

  let print = print
  let print_hints = print_hints
  let mk = mk_warning
  let fatal ({ kind = Warning { fatal; _ }; _} : _ t) = fatal
  let enabled ({ kind = Warning { enabled; _ }; _} : _ t) = enabled
  let code ({ kind = Warning { code; _ }; _ } : _ t) = code

end

