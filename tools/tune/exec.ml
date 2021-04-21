
open Res

type cmd = string
type env = string array


module type S = sig

  type t
  type res
  type stats

  val run : ?env:env -> cmd -> (t, string) Result.t
  (** Run a command *)

  val res : t -> res
  (** The underlying result. *)

  val stats : t -> stats
  (** The stats of the result. *)

  val print : Format.formatter -> t -> unit
  (** Print function *)

end

module Core = struct

  type res = {
    errcode : int;
    stdout : string;
    stderr : string;
  }

  type stats = unit
  type t = res

  let res t = t
  let stats _ = ()

  let print_output fmt s =
    let max_l = 50 in
    if String.length s <= max_l then
      Format.fprintf fmt "%s" s
    else begin
      let s = String.sub s 0 (max_l - 3) in
      Format.fprintf fmt "%s..." s
    end

  let print fmt { errcode; stdout; stderr; } =
    Format.fprintf fmt
      "(@[<hv>\
       (@[<hov>errcode@ %d@])@ \
       (@[<hov>stdout@ '%a'@])@ \
       (@[<hov>stderr@ '%a'@])\
       @])"
      errcode
      print_output stdout
      print_output stderr

  let int_of_process_status = function
    | Unix.WEXITED i
    | Unix.WSIGNALED i
    | Unix.WSTOPPED i -> i

  let run ?(env=Unix.environment()) cmd =
    try
      let oc, ic, errc = Unix.open_process_full cmd env in
      close_out ic;
      (* read out and err *)
      let err = ref "" in
      let t_err = Thread.create (fun e -> err := CCIO.read_all e) errc in
      let out = CCIO.read_all oc in
      Thread.join t_err;
      let status = Unix.close_process_full (oc, ic, errc) in
      Ok {
        stdout = out;
        stderr = !err;
        errcode = int_of_process_status status;
      }
    with _ ->
      Error "process died"

end

module Time(Base : S) = struct

  type stats = {
    (* time stats *)
    real_time : float;
    user_time : float;
    kernel_time : float;
    (* Memory stats *)
    peak_mem : int;
  }

  type res = Base.t

  type t = {
    res : res;
    stats : stats;
  }

  let res t = t.res
  let stats t = t.stats

  let print_stats fmt { real_time; user_time; kernel_time; peak_mem; } =
    Format.fprintf fmt
      "(@[<hv>\
       (@[<hov>real_time@ %f@])@ \
       (@[<hov>user_time@ %f@])@ \
       (@[<hov>kernel_time@ %f@])@ \
       (@[<hov>peak_mem@ %d@])\
       @])"
      real_time
      user_time
      kernel_time
      peak_mem

  let print fmt { res; stats; } =
    Format.fprintf fmt
      "(@[<hv>\
       (@[<hov>stats@ %a@])@ \
       (@[<hov>res@ %a@])\
       @])"
      print_stats stats
      Base.print res

  let read_stats file =
    match CCIO.(with_in file read_lines_l) with
    | [ real_time; kernel_time; user_time; peak_mem ] ->
      let real_time = Float.of_string real_time in
      let kernel_time = Float.of_string kernel_time in
      let user_time = Float.of_string user_time in
      let peak_mem = int_of_string peak_mem in
      Ok { user_time; real_time; kernel_time; peak_mem; }
    | l ->
      Error (Format.asprintf
               "failed to read stats from time:\n@[<v>%a@]"
               (Format.pp_print_list Format.pp_print_string) l)

  let run ?env cmd =
    let tmp_file = Filename.temp_file "dolmen-tune." ".time" in
    let cmd =
      Format.asprintf
        "/usr/bin/time -f '%s' -o %s %s"
        "%e\n%S\n%U\n%M" tmp_file cmd
    in
    try
      let+ res = Base.run ?env cmd in
      let+ stats = read_stats tmp_file in
      Sys.remove tmp_file;
      Ok { res; stats; }
    with e ->
      Sys.remove tmp_file;
      raise e

end

module Full = Time(Core)


