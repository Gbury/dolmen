
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Some helpers *)
(* ************************************************************************* *)

let file_size file_path =
  let st = Unix.stat file_path in
  st.st_size

(* Custom progress bar *)
(* ************************************************************************* *)

let percentage_printer =
  Progress.Printer.create ()
    ~string_len:4
    ~to_string:(fun p -> Format.asprintf "%3d%%" p)

module Bytes_stats = struct

  type t = {
    total_bytes : int;
    processed_bytes : int;
    elapsed_time : Mtime.span;
  }

  let zero = {
    total_bytes = 0;
    processed_bytes = 0;
    elapsed_time = Mtime.Span.zero;
  }

  let input total_bytes = { zero with total_bytes }

  let total_bytes { total_bytes; _ } = total_bytes
  let elapsed_time { elapsed_time; _ } = elapsed_time
  let processed_bytes { processed_bytes; _ } = processed_bytes
  let percentage { processed_bytes; total_bytes; _ } =
    int_of_float (100. *. (float processed_bytes) /. (float total_bytes))
  let perthousand { processed_bytes; total_bytes; _ } =
    int_of_float (1000. *. (float processed_bytes) /. (float total_bytes))

  let line phase : t Progress.Line.t =
    let open Progress.Line in
    list [
      using (fun _ -> phase) (of_printer ~init:phase (Progress.Printer.string ~width:50));
      using elapsed_time (of_printer ~init:Mtime.Span.zero Progress.Units.Duration.mm_ss);
      using processed_bytes (of_printer ~init:0 Progress.Units.Bytes.of_int);
      using total_bytes (of_printer ~init:0 Progress.Units.Bytes.of_int);
      (* TODO: add bytes/seconds *)
      using perthousand (bar ~data:`Latest 1000);
      using percentage (of_printer ~init:0 percentage_printer);
    ]

end

(* Stats & progress bars *)
(* ************************************************************************* *)

module type S = Stats_intf.S

module Make(State : State.S) = struct

  (* type *)

  type config = {
    enabled : bool;
    typing : bool;
    model : bool;
  }

  type input = int
  type counter = Mtime_clock.counter
  type parsing = {
    stats : Bytes_stats.t array;
    reporters : Bytes_stats.t Progress.Reporter.t array;
  }

  type typing = {
    stats : Bytes_stats.t;
    reporter : Bytes_stats.t Progress.Reporter.t;
  }

  type model = {
    stats : Bytes_stats.t;
    reporter : Bytes_stats.t Progress.Reporter.t;
  }


  (* state keys *)

  let pipe = "stats"

  let config_key : config State.key =
    State.create_key ~pipe "stats_config"

  let parsing_key : parsing State.key =
    State.create_key ~pipe "stats_parsing"

  let typing_key : typing State.key =
    State.create_key ~pipe "stats_typing"

  let model_key : model State.key =
    State.create_key ~pipe "stats_model"


  (* state init *)

  let init ~enabled ~typing ~model st =
    let config = { enabled; typing; model; } in
    let st = State.set config_key config st in
    if not enabled then st
    else begin
      (* create progress display *)
      let config = Progress.Config.v () in
      let multi = Progress.Multi.blank in
      let display = Progress.Display.start ~config multi in
      let st = State.set State.progress_display display st in
      (* create bars *)
      let st =
        let parsing : parsing =
          let stats = [| |] in
          let reporters = [| |] in
          { stats; reporters; }
        in
        State.set parsing_key parsing st
      in
      let st =
        if not typing then st
        else begin
          let typing : typing =
            let stats = Bytes_stats.zero in
            let line = Bytes_stats.line "typing" in
            let reporter = Progress.Display.add_line display line in
            { stats; reporter; }
          in
          State.set typing_key typing st
        end
      in
      let st =
        if not model then st
        else begin
          let model : model =
            let stats = Bytes_stats.zero in
            let line = Bytes_stats.line "model" in
            let reporter = Progress.Display.add_line display line in
            { stats; reporter; }
          in
          State.set model_key model st
        end
      in
      st
    end

  (* common *)

  let config st = State.get config_key st

  let start_counter st =
    if (config st).enabled then Some (Mtime_clock.counter ()) else None

  (* parsing and inputs *)

  let new_input st input_name input_size =
    let config = config st in
    if config.enabled then begin
      let parsing = State.get parsing_key st in
      let display = State.get State.progress_display st in
      let n = Array.length parsing.stats in
      assert (n = Array.length parsing.reporters);
      let z = Bytes_stats.input input_size in
      let line_name = Format.asprintf "parsing (%s)" input_name in
      let line = Bytes_stats.line line_name in
      let above =
        (if config.model then 1 else 0 (* model *)) +
        (if config.typing then 1 else 0 (* typing *)) +
        n (* already existing parsing lines *)
      in
      let reporter = Progress.Display.add_line ~above display line in
      let stats = Array.append parsing.stats [| z |] in
      let reporters = Array.append parsing.reporters [| reporter |] in
      let parsing : parsing = { stats; reporters; } in
      let st' = State.set parsing_key parsing st in
      n, st'
    end else
      0, st

  let record_parsed st input counter loc =
    match input with
    | None -> st
    | Some input ->
      let span =
        match counter with
        | None -> Mtime.Span.zero
        | Some counter -> Mtime_clock.count counter
      in
      let config = config st in
      if config.enabled then begin
        (* record the loc as parsed *)
        let st =
          let parsing = State.get parsing_key st in
          let input_stats = parsing.stats.(input) in
          let input_reporter = parsing.reporters.(input) in
          let elapsed_time = Mtime.Span.add span input_stats.elapsed_time in
          let processed_bytes = max input_stats.processed_bytes (Dolmen.Std.Loc.last_byte loc) in
          let input_stats = { input_stats with elapsed_time; processed_bytes; } in
          let () = Progress.Reporter.report input_reporter input_stats in
          let stats = Array.copy parsing.stats in
          let () = Array.set stats input input_stats in
          let parsing = { parsing with stats } in
          State.set parsing_key parsing st
        in
        (* add the loc to be typed *)
        let st =
          if not config.typing then st
          else begin
            let bytes_to_type = Dolmen.Std.Loc.length_in_bytes loc in
            let typing = State.get typing_key st in
            let total_bytes = typing.stats.total_bytes + bytes_to_type in
            let stats = { typing.stats with total_bytes } in
            let typing = { typing with stats } in
            let () = Progress.Reporter.report typing.reporter stats in
            State.set typing_key typing st
          end
        in
        st
      end else
        st

  let record_typed st counter loc =
    match counter with
    | None -> st
    | Some counter ->
      let span = Mtime_clock.count counter in
      let config = config st in
      (* record the loc as typed *)
      let st =
        if not config.typing then st
        else begin
          let processed = Dolmen.Std.Loc.length_in_bytes loc in
          let typing = State.get typing_key st in
          let elapsed_time = Mtime.Span.add span typing.stats.elapsed_time in
          let processed_bytes = typing.stats.processed_bytes + processed in
          let stats = { typing.stats with elapsed_time; processed_bytes; } in
          let typing = { typing with stats } in
          let () = Progress.Reporter.report typing.reporter stats in
          State.set typing_key typing st
        end
      in
      (* add the loc to be checked for model *)
      let st =
        if not config.model then st
        else begin
          let bytes_to_type = Dolmen.Std.Loc.length_in_bytes loc in
          let model = State.get model_key st in
          let total_bytes = model.stats.total_bytes + bytes_to_type in
          let stats = { model.stats with total_bytes } in
          let model = { model with stats } in
          let () = Progress.Reporter.report model.reporter stats in
          State.set model_key model st
        end
      in
      st

  (* finalisation *)

  let finalise st =
    let config = config st in
    if config.typing then begin
      let typing = State.get typing_key st in
      let () = Unix.sleepf (1. /. 60.) in
      Progress.Reporter.report typing.reporter typing.stats
    end;
    if config.model then begin
      let model = State.get model_key st in
      let () = Unix.sleepf (1. /. 60.) in
      Progress.Reporter.report model.reporter model.stats
    end;
    if config.enabled then begin
      let parsing = State.get parsing_key st in
      Array.iter2 (fun stats reporter ->
          let () = Unix.sleepf (1. /. 60.) in
          Progress.Reporter.report reporter stats
        ) parsing.stats parsing.reporters;
      let display = State.get State.progress_display st in
      let () = Unix.sleepf (1. /. 60.) in
      Progress.Display.finalise display
    end;
    ()

end


