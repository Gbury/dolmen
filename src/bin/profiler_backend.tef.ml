
module P = Dolmen_std.Profile

let active = lazy (
  match Sys.getenv "TEF" with
  | "1"|"true" -> true | _ -> false
  | exception Not_found -> false
)

let program_start = Mtime_clock.now()

module Make()
  : P.BACKEND
= struct
  let first_ = ref true
  let closed_ = ref false

  let teardown_ oc =
    if not !closed_ then (
      closed_ := true;
      output_char oc ']'; (* close array *)
      flush oc;
      close_out_noerr oc
    )

  (* connection to subprocess writing into the file *)
  let oc =
    let oc = Unix.open_process_out "gzip - --stdout > trace.json.gz" in
    output_char oc '[';
    at_exit (fun () -> teardown_ oc);
    oc

  let get_ts () : float =
    let now = Mtime_clock.now() in
    Mtime.Span.to_us (Mtime.span program_start now)

  let emit_sep_ () =
    if !first_ then (
      first_ := false;
    ) else (
      output_string oc ",\n";
    )

  let pid = Unix.getpid()

  let emit_duration_event ~name ~start ~end_ () : unit =
    let dur = end_ -. start in
    let ts = start in
    let tid = Thread.id (Thread.self()) in
    emit_sep_();
    Printf.fprintf oc
      {json|{"pid": %d,"tid": %d,"dur": %.2f,"ts": %.2f,"name":"%s","ph":"X"}|json}
      pid tid dur ts name;
    ()

  let emit_instant_event ~name ~ts () : unit =
    let pid = Unix.getpid() in
    let tid = Thread.id (Thread.self()) in
    emit_sep_();
    Printf.fprintf oc
      {json|{"pid": %d,"tid": %d,"ts": %.2f,"name":"%s","ph":"I"}|json}
      pid tid ts name;
    ()

  let teardown () = teardown_ oc
end

let setup_ = lazy (
  let lazy active = active in
  let b = if active then Some (module Make() : P.BACKEND) else None in
  P.Control.setup b
)

let setup () = Lazy.force setup_
let teardown = P.Control.teardown
