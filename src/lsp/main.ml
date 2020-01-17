

let main () =
  Lsp.Logger.log ~section:"" ~title:"app" "start lsp";
  Lsp.Rpc.start State.empty Handler.handler stdin stdout;
  Lsp.Logger.log ~section:"" ~title:"app" "stop lsp";
  ()

let () =
  Lsp.Logger.with_log_file (Some "/tmp/lsp.log") main

