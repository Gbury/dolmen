
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

let get_extension s =
  let rec aux s i j =
    if j >= String.length s then
      if i < 0 then ""
      else String.sub s i (String.length s - i)
    else begin
      match s.[j] with
      | '.' -> aux s j (j + 1)
      | _ -> aux s i (j + 1)
    end
  in
  aux s (-1) 0

