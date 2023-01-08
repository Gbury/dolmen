
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
  | Some (l : Dolmen.Std.Loc.loc) ->
    let start_pos = if l.start_line = 0 then 0 else l.start_line - 1 in
    let end_pos = if l.stop_line = 0 then 0 else l.stop_line - 1 in
    lsp_range
      (lsp_pos start_pos l.start_column)
      (lsp_pos end_pos l.stop_column)

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


