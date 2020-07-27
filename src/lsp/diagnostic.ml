
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type t = Lsp.Protocol.PublishDiagnostics.diagnostic

let lsp_pos line character =
  Lsp.Protocol.Position.{ line; character; }

let lsp_range start_ end_ =
  Lsp.Protocol.Range.{ start_; end_; }

let start_pos = lsp_pos 1 1
let start_range = lsp_range start_pos start_pos

let range_of_loc = function
  | None -> start_range
  | Some (l : Dolmen.Std.Loc.loc) ->
    lsp_range
      (lsp_pos (l.start_line - 1) l.start_column)
      (lsp_pos (l.stop_line - 1) l.stop_column)

let warn ?loc msg =
  Lsp.Protocol.PublishDiagnostics.{
    range = range_of_loc loc;
    severity = Some Warning;
    code = NoCode;
    source = Some "dolmenls";
    message = msg;
    relatedInformation = [];
    tags = [];
  }

let error ?loc msg =
  Lsp.Protocol.PublishDiagnostics.{
    range = range_of_loc loc;
    severity = Some Error;
    code = NoCode;
    source = Some "dolmenls";
    message = msg;
    relatedInformation = [];
    tags = [];
  }


