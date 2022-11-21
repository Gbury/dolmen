
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Some helpers *)
(* ************************************************************************* *)

let file_size file_path =
  let st = Unix.stat file_path in
  st.st_size

(* Stats & progress bars *)
(* ************************************************************************* *)

module type S = Stats_intf.S

module Make(State : State.S) = struct

  (* type *)

  type config = {
    mem : bool;
    max_mem : int;
    enabled : bool;
    typing : bool;
    model : bool;
  }

  type input = Tui.Bar.t State.key
  type counter = Mtime_clock.counter

  (* state keys *)

  let pipe = "stats"

  let config_key : config State.key =
    State.create_key ~pipe "stats_config"

  let parsing_lines : int State.key =
    State.create_key ~pipe "parsing_lines"

  let typing_key : Tui.Bar.t State.key =
    State.create_key ~pipe "stats_typing"

  let model_key : Tui.Bar.t State.key =
    State.create_key ~pipe "stats_model"

  (* state init *)

  let init ~mem ~max_mem ~enabled ~typing ~model st =
    let config = { mem; max_mem; enabled; typing; model; } in
    let st = State.set config_key config st in
    if not enabled then st
    else begin
      (* create progress display *)
      let () = Tui.init_display () in
      (* init the number of parsing lines *)
      let st = State.set parsing_lines 0 st in
      (* create typing bar *)
      let st =
        if not typing then st
        else begin
          let typing =
            Tui.Bar.create ()
              ~config:{ mem_bar = config.mem }
              ~name:"typing" ~max_mem ~total_bytes:0
          in
          State.set typing_key typing st
        end
      in
      (* create parsing bar *)
      let st =
        if not model then st
        else begin
          let model =
            Tui.Bar.create ()
              ~config:{ mem_bar = config.mem }
              ~name:"model" ~max_mem ~total_bytes:0
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
    let key =
      State.create_key ~pipe
        (Format.asprintf "parsing:%s" input_name)
    in
    let st =
      if config.enabled then begin
        let n = State.get parsing_lines st in
        let above =
          (if config.model then 1 else 0 (* model *)) +
          (if config.typing then 1 else 0 (* typing *)) +
          n (* already existing parsing lines *)
        in
        let name = Format.asprintf "%s" input_name in
        let parsing =
          Tui.Bar.create ~config:{ mem_bar = false } ()
            ~name ~above ~max_mem:config.max_mem ~total_bytes:input_size
        in
        State.set key parsing st
      end else
        st
    in
    key, st

  let record_parsed st input counter loc =
    match input with
    | None -> st
    | Some input ->
      begin match counter with
        | None -> st
        | Some counter ->
          let span = Mtime_clock.count counter in
          let config = config st in
          assert config.enabled;
          (* record the loc as parsed *)
          let parsing = State.get input st in
          Tui.Bar.add_processed parsing ~span
            ~processed:(`Last loc) ~mem:`None;
          (* add the loc to be typed *)
          if config.typing then begin
            let typing = State.get typing_key st in
            Tui.Bar.add_to_process typing ~loc
          end;
          st
      end

  let record_typed st counter loc persistent_data =
    match counter with
    | None -> st
    | Some counter ->
      let span = Mtime_clock.count counter in
      let config = config st in
      assert (config.enabled);
      (* record the loc as typed *)
      if config.typing then begin
        let typing = State.get typing_key st in
        Tui.Bar.add_processed typing ~span
          ~processed:(`Sum loc) ~mem:(`Set persistent_data);
      end;
      (* add the loc to be checked for model *)
      if config.model then begin
        let model = State.get model_key st in
        Tui.Bar.add_to_process model ~loc
      end;
      st

  let record_checked st counter loc persistent_data =
    match counter with
    | None -> st
    | Some counter ->
      let span = Mtime_clock.count counter in
      let config = config st in
      assert (config.enabled);
      (* record the loc as typed *)
      if config.model then begin
        let model = State.get model_key st in
        Tui.Bar.add_processed model ~span ~processed:(`Sum loc) ~mem:persistent_data;
      end;
      st

end


