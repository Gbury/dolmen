
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type t = Lsp.Types.Diagnostic.t

let lsp_pos line character =
  Lsp.Types.Position.create ~line ~character

let lsp_range start end_ =
  Lsp.Types.Range.create ~start ~end_

let start_pos = lsp_pos 1 1
let start_range = lsp_range start_pos start_pos

let range_of_loc = function
  | None -> start_range
  | Some l ->
    if Dolmen.Std.Loc.is_dummy l
    then start_range
    else
      lsp_range
        (lsp_pos (l.start_line - 1) l.start_column)
        (lsp_pos (l.stop_line - 1) l.stop_column)

let warn ?loc message =
  Lsp.Types.Diagnostic.create ()
    ~range:(range_of_loc loc)
    ~severity:Warning
    ~source:"dolmenls"
    ~message

let error ?loc message =
  Lsp.Types.Diagnostic.create ()
    ~range:(range_of_loc loc)
    ~severity:Error
    ~source:"dolmenls"
    ~message


