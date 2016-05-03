
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

let replicate n x =
  let rec aux x acc i =
    if i <= 0 then acc
    else aux x (x :: acc) (i - 1)
  in
  aux x [] n

(* Option printing *)

let pp_opt ?(none="<none>") pp b = function
  | Some t -> pp b t
  | None -> Printf.bprintf b "%s" none

let print_opt ?(none="<none>") print fmt = function
  | Some t -> print fmt t
  | None -> Format.fprintf fmt "%s" none


(* List printing functions *)

let rec pp_list ~pp_sep ~sep ~pp b = function
  | [] -> ()
  | [h] -> pp b h
  | h :: r ->
    Printf.bprintf b "%a%a%a"
      pp h pp_sep sep
      (pp_list ~pp_sep ~sep ~pp) r

let rec print_list ~print_sep ~sep ~print fmt = function
  | [] -> ()
  | [h] -> print fmt h
  | h :: r ->
    Format.fprintf fmt "%a%a%a"
      print h print_sep sep
      (print_list ~print_sep ~sep ~print) r

