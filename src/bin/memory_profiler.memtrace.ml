
let available = true

let start filename sampling_rate =
  let _s =
    Memtrace.start_tracing
      ~filename ~sampling_rate
      ~context:(Some "dolmen")
  in
  ()

