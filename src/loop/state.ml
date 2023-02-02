
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

module M = Dolmen.Std.Hmap.Make(struct
    type t = int
    let compare (a: int) (b: int) = compare a b
  end)

type t = M.t

type 'a key = {
  id : int;
  name : string;
  pipe : string;
  inj : 'a Dolmen.Std.Hmap.injection;
}

type report_style =
  | Minimal
  | Regular
  | Contextual

type source = [
  | `Stdin
  | `File of string
  | `Raw of string * string
]

type mode = [
  | `Full
  | `Incremental
]

type 'lang file = {
  lang    : 'lang option;
  mode    : mode option;
  loc     : Dolmen.Std.Loc.file;
  dir     : string;
  source  : source;
}

exception Error of t
exception Key_not_found of t * string * string

let () =
  Printexc.register_printer (function
      | Key_not_found (_st, key_name, key_pipe) ->
        let msg = Format.asprintf
            "Key %s not bound in state. \
             Have you called the init function for the %s pipe/module ?"
            key_name key_pipe
        in
        Some msg
      | _ -> None
    )

(* Helper functions *)
(* ************************************************************************* *)

let split_input = function
  | `Stdin ->
    Sys.getcwd (), `Stdin
  | `File f ->
    Filename.dirname f, `File (Filename.basename f)

let mk_file ?lang ?mode ?(loc = Dolmen.Std.Loc.mk_file "") dir source =
  { lang; mode; dir; source ; loc; }

(* Signatures *)
(* ************************************************************************* *)

module type S = sig

  type t
  (** The type of state *)

  type 'a key
  (** The type of keys into the state. *)

  exception Error of t
  (** Convenient exception. *)

  exception Key_not_found of t * string * string
  (** Exception raised by `get` when the key is not bound. *)

  val create_key : pipe:string -> string -> _ key
  (** create a new key *)

  val get : 'a key -> t -> 'a
  (** get the value associated to a key. *)

  val get_or : default:'a -> 'a key -> t -> 'a
  (** get the value associated to a key,
      or the default if the key is not bound. *)

  val set : 'a key -> 'a -> t -> t
  (** Set the value associated to a key. *)

  val warn :
    ?file:_ file ->
    ?loc:Dolmen.Std.Loc.full ->
    t -> 'a Report.Warning.t -> 'a -> t
  (** Emit a warning *)

  val error :
    ?file:_ file ->
    ?loc:Dolmen.Std.Loc.full ->
    t -> 'a Report.Error.t -> 'a -> t
  (** Emit an error. *)

  val debug : bool key
  val reports : Report.Conf.t key
  val report_style : report_style key
  val max_warn : int key
  val cur_warn : int key
  val time_limit : float key
  val size_limit : float key
  val logic_file : Logic.language file key
  val response_file : Response.language file key
  (* common keys *)

end

(* Key functions *)
(* ************************************************************************* *)

let empty : t = M.empty

let key_counter = ref 0
let create_key ~pipe name =
  incr key_counter;
  { id = !key_counter; pipe; name;
    inj = Dolmen.Std.Hmap.create_inj ();}

let get k t =
  match M.get ~inj:k.inj k.id t with
  | Some v -> v
  | None -> raise (Key_not_found (t, k.name, k.pipe))

let get_or ~default k t =
  match M.get ~inj:k.inj k.id t with
  | Some v -> v
  | None -> default

let set k v t =
  M.add ~inj:k.inj k.id v t

let update k f t =
  set k (f (get k t)) t


(* Some common keys *)
(* ************************************************************************* *)

let pipe = "state"
let bt : bool key = create_key ~pipe "bt"
let debug : bool key = create_key ~pipe "debug"
let reports : Report.Conf.t key = create_key ~pipe "reports"
let report_style : report_style key = create_key ~pipe "report_style"
let max_warn : int key = create_key ~pipe "max_warn"
let cur_warn : int key = create_key ~pipe "cur_warn"

let time_limit : float key = create_key ~pipe "time_limit"
let size_limit : float key = create_key ~pipe "size_limit"

let logic_file : Logic.language file key = create_key ~pipe "logic_file"
let response_file : Response.language file key = create_key ~pipe "response_file"

let init
    ?bt:(bt_value=(Printexc.backtrace_status ()))
    ~debug:debug_value
    ~report_style:report_style_value
    ~reports:reports_value
    ~max_warn:max_warn_value
    ?cur_warn:(cur_warn_value=0)
    ~time_limit:time_limit_value
    ~size_limit:size_limit_value
    ~logic_file:logic_file_value
    ~response_file:response_file_value
    st =
  st
  |> set bt bt_value
  |> set debug debug_value
  |> set report_style report_style_value
  |> set reports reports_value
  |> set max_warn max_warn_value
  |> set cur_warn cur_warn_value
  |> set time_limit time_limit_value
  |> set size_limit size_limit_value
  |> set logic_file logic_file_value
  |> set response_file response_file_value

