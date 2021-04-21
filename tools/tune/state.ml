
type t = {
  config : Config.t;
  results : (Params.t * Exec.Full.t) list;
}

(* Creation and manipulaiton *)

let empty config =
  { config; results = []; }

let set_results results t =
  { t with results; }

let add_results l t =
  let results =
    match t.results with
    | [] -> l
    | l' -> List.rev_append l l'
  in
  { t with results; }


(* Save/store on disk *)

let save ~file (state : t) =
  let ch = open_out_bin file in
  let () = Marshal.to_channel ch state [] in
  ()

let load ~file : t =
  let ch = open_in_bin file in
  let res : t = Marshal.from_channel ch in
  let () = close_in ch in
  res

let with_in ~input_state ~f =
  let state = load ~file:input_state in
  f state

let with_out ~output_state ~f =
  let state = f () in
  save ~file:output_state state

let with_ ~input_state ~output_state ~f =
  with_in ~input_state ~f:(fun state ->
      with_out ~output_state ~f:(fun () -> f state)
    )

