
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

let get_extension s =
  let rec aux s i =
    if i <= 0 then ""
    else
      match s.[i] with
      | '.' -> String.sub s i (String.length s - i)
      | _ -> aux s (i - 1)
  in
  aux s (String.length s - 1)

let replicate n x =
  let rec aux x acc i =
    if i <= 0 then acc
    else aux x (x :: acc) (i - 1)
  in
  aux x [] n

let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

let opt_map o f =
  match o with
  | None -> None
  | Some x -> Some (f x)

let opt_bind o f =
  match o with
  | None -> None
  | Some x -> f x


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


(* Operations on Lexing.lexbuf *)
let set_file buf filename =
  let open Lexing in
  buf.lex_curr_p <- {buf.lex_curr_p with pos_fname=filename;};
  ()

let filename_of_input = function
  | `Contents (file, _) -> file
  | `File file -> file
  | `Stdin -> "<stdin>"

let filename_of_input_source = function
  | `Raw (file, _) -> file
  | `File file -> file
  | `Stdin -> "<stdin>"

let mk_lexbuf i =
  let filename = filename_of_input i in
  match i with
  | `Contents (_, s) ->
    let buf = Lexing.from_string s in
    set_file buf filename;
    buf, (fun () -> ())
  | `Stdin ->
    let ch, cl = stdin, (fun () -> ()) in
    let buf = Lexing.from_channel ch in
    set_file buf filename;
    buf, cl
  | `File s ->
    let ch = open_in s in
    let cl = (fun () -> close_in ch) in
    let buf = Lexing.from_channel ch in
    set_file buf filename;
    buf, cl

