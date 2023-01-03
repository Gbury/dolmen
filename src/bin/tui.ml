
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Global state and terminal interaction *)
(* ************************************************************************* *)

let display : (unit, unit) Progress.Display.t option ref = ref None

let init_display () =
  match !display with
  | Some _ ->
    assert false
  | None ->
    let config = Progress.Config.v () in
    let multi = Progress.Multi.blank in
    let d = Progress.Display.start ~config multi in
    display := Some d

let finalise_display () =
  match !display with
  | None -> ()
  | Some d ->
    let () = Progress.Display.finalise d in
    display := None

(* Regular printing *)
(* ************************************************************************* *)

let kfprintf aux fmt format =
  match !display with
  | None -> Format.kfprintf aux fmt format
  | Some d ->
    Progress.Display.pause d;
    Format.kfprintf
      (fun fmt -> Progress.Display.resume d; aux fmt)
      fmt format

let fprintf fmt format = kfprintf (fun _ -> ()) fmt format
let printf format = fprintf Format.std_formatter format
let eprintf format = fprintf Format.err_formatter format


(* Progress bars *)
(* ************************************************************************* *)

module Bar = struct

  module Data = struct

    type t = {
      total_bytes : int;          (* total bytes to process *)
      processed_bytes : int;      (* processed bytes *)
      elapsed_time : Mtime.span;  (* time spent processing *)
      max_mem : int;              (* memory limit *)
      cur_mem : int;              (* memory currently used *)
      last_mem : Mtime.span;      (* last span that cur memory was computed *)
    }

    let zero max_mem = {
      total_bytes = 0;
      processed_bytes = 0;
      elapsed_time = Mtime.Span.zero;
      max_mem;
      cur_mem = 0;
      last_mem = Mtime.Span.zero;
    }

    let input max_mem total_bytes = { (zero max_mem) with total_bytes }

    let total_bytes { total_bytes; _ } = total_bytes
    let elapsed_time { elapsed_time; _ } = elapsed_time
    let processed_bytes { processed_bytes; _ } = processed_bytes
    let percentage { processed_bytes; total_bytes; _ } =
      int_of_float (100. *. (float processed_bytes) /. (float total_bytes))
    let perthousand { processed_bytes; total_bytes; _ } =
      int_of_float (1000. *. (float processed_bytes) /. (float total_bytes))
    let cur_mem { cur_mem; _ } = cur_mem
    let ratio_mem { cur_mem; max_mem; _ } =
      int_of_float (1000. *. (float cur_mem) /. (float max_mem))

  end

  type config = {
    mem_bar : bool;
  }

  type t = {
    mutable data : Data.t;
    config : config;
    reporter : Data.t Progress.Reporter.t;
  }

  let percentage_printer =
    Progress.Printer.create ()
      ~string_len:4
      ~to_string:(fun p -> Format.asprintf "%3d%%" p)

  let line ~config ~name =
      let process_width, mem_width =
        match Terminal.Size.get_columns () with
        | None -> `Expand, `Fixed 20
        | Some w -> `Expand, `Fixed ((w - 100) / 2)
      in
      let open Progress.Line in
      list [
        using (fun _ -> name) (of_printer ~init:name (Progress.Printer.string ~width:20));
        using Data.elapsed_time (of_printer ~init:Mtime.Span.zero Progress.Units.Duration.mm_ss);
        using Data.processed_bytes (of_printer ~init:0 Progress.Units.Bytes.of_int);
      const "/";
      using Data.total_bytes (of_printer ~init:0 Progress.Units.Bytes.of_int);
      (* TODO: add bytes/seconds *)
      if config.mem_bar then
        const "|"
      else
        noop ();
      if config.mem_bar then
        using Data.cur_mem (of_printer ~init:0 Progress.Units.Bytes.of_int)
      else
        noop ();
      if config.mem_bar then
        using Data.ratio_mem (bar ~width:mem_width ~data:`Latest 1000)
      else
        noop ();
      using Data.perthousand (bar ~width:process_width ~data:`Latest 1000);
      using Data.percentage (of_printer ~init:0 percentage_printer);
      ]

  let create ?above ~config ~name ~max_mem ~total_bytes () =
    let data = Data.input max_mem total_bytes in
    let line = line ~config ~name in
    match !display with
    | None -> failwith "Display not intialised"
    | Some d ->
      let reporter = Progress.Display.add_line ?above d line in
      { data; config; reporter; }

  let obj_size obj =
    let in_words = Obj.reachable_words (Obj.repr obj) in
    let in_bytes = in_words * (Sys.word_size / 8) in
    in_bytes

  let[@inline] add_to_process ({ data; reporter; _ } as t) ~loc =
    let total_bytes = data.total_bytes + (Dolmen.Std.Loc.length_in_bytes loc) in
    let new_data = { data with total_bytes; } in
    let () = Progress.Reporter.report reporter new_data in
    t.data <- new_data

  let[@inline] add_processed ({ data; config; reporter; } as t) ~span ~processed ~mem =
    let elapsed_time = Mtime.Span.add data.elapsed_time span in
    let cur_mem, last_mem =
      if not config.mem_bar then
        data.cur_mem, data.last_mem
      else begin
        match mem with
        | `None -> data.cur_mem, data.last_mem
        | `Add obj -> obj_size obj + data.cur_mem, data.last_mem
        | `Set obj ->
          if Mtime.Span.equal Mtime.Span.zero data.elapsed_time
          || (Mtime.Span.to_s (Mtime.Span.abs_diff elapsed_time data.last_mem) >= 0.3)
          then obj_size obj, elapsed_time
          else data.cur_mem, data.last_mem
      end
    in
    let processed_bytes =
      match processed with
      | `Last loc -> max data.processed_bytes (Dolmen.Std.Loc.last_byte loc)
      | `Sum loc -> data.processed_bytes + (Dolmen.Std.Loc.length_in_bytes loc)
    in
    let new_data = { data with elapsed_time; cur_mem; last_mem; processed_bytes; } in
    let () = Progress.Reporter.report reporter new_data in
    t.data <- new_data

end

