
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Strings & unicode *)
(* ************************************************************************* *)

let string_for_all ?(start=0) pred s =
  let rec aux start p s i =
    if i < start then true
    else if p (String.unsafe_get s i) then aux start p s (i - 1)
    else false
  in
  aux start pred s (String.length s - 1)

let encode e c =
  match Uutf.encode e c with
  | `Ok -> ()
  | `Partial ->
    (* should only happen with manual sources according to the doc,
       so it is safe to assume it doesn't happen *)
    assert false

let encode_char e c = encode e (`Uchar c)

let string_unicode_map ?(encoding_from=`UTF_8) ?(encoding_to=`UTF_8) f s =
  let b = Buffer.create (String.length s) in
  let d = Uutf.decoder ~encoding:encoding_from (`String s) in
  let e = Uutf.encoder encoding_to (`Buffer b) in
  let rec aux () =
    match Uutf.decode d with
    | `End ->
      let () = encode e `End in
      let ret = Buffer.contents b in
      if String.equal s ret then s else ret
    | `Await ->
      (* should only happen with manual sources according to the doc,
         so it is safe to assume it doesn't happen *)
      assert false
    | `Uchar c ->
      let pos = Uutf.decoder_count d in
      let () = List.iter (encode_char e) (f pos (Some c)) in
      aux ()
    | `Malformed _ ->
      let pos = Uutf.decoder_count d in
      let () = List.iter (encode_char e) (f pos None) in
      aux ()
  in
  aux ()

(* File extensions *)
(* ************************************************************************* *)

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


(* Hashs *)
(* ************************************************************************* *)
(* Taken from containres's CCHash, cf
   https://github.com/c-cube/ocaml-containers/blob/master/src/core/CCHash.ml *)

let fnv_offset_basis = 0xcbf29ce484222325L
let fnv_prime = 0x100000001b3L

(* hash an integer *)
let hash_int n =
  let h = ref fnv_offset_basis in
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((n lsr (k * 8)) land 0xff)));
  done;
  (Int64.to_int !h) land max_int (* truncate back to int and remove sign *)

let hash_string (x:string) =
  let h = ref fnv_offset_basis in
  for i = 0 to String.length x - 1 do
    h := Int64.(mul !h fnv_prime);
    let byte = Char.code (String.unsafe_get x i) in
    h := Int64.(logxor !h (of_int byte));
  done;
  Int64.to_int !h land max_int

let hash2 a b =
  let h = ref fnv_offset_basis in
  (* we only do one loop, where we mix bytes of [a] and [b], so as
     to simplify control flow *)
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((a lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((b lsr (k * 8)) land 0xff)));
  done;
  Int64.to_int !h land max_int

let hash3 a b c =
  let h = ref fnv_offset_basis in
  (* we only do one loop, where we mix bytes of [a] [b] and [c], so as
     to simplify control flow *)
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((a lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((b lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((c lsr (k * 8)) land 0xff)));
  done;
  Int64.to_int !h land max_int

let hash4 a b c d =
  let h = ref fnv_offset_basis in
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((a lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((b lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((c lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((d lsr (k * 8)) land 0xff)));
  done;
  Int64.to_int !h land max_int

let[@inline] hash_combine f s x = hash2 s (f x)

let hash_list f l = List.fold_left (hash_combine f) 0x42 l


(* Comparisons *)
(* ************************************************************************* *)

(* Useful shorthand for chaining comparisons *)
let (<?>) i (cmp, x, y) =
  match i with
  | 0 -> cmp x y
  | _ -> i

(* lexicographic comparison *)
let lexicographic cmp l l' =
  let rec aux l l' =
    match l, l' with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | x :: r, x' :: r' ->
      begin match cmp x x' with
        | 0 -> aux r r'
        | res -> res
      end
  in
  aux l l'


(* Options *)
(* ************************************************************************* *)

let opt_map o f =
  match o with
  | None -> None
  | Some x -> Some (f x)

let opt_bind o f =
  match o with
  | None -> None
  | Some x -> f x

let pp_opt ?(none="<none>") pp b = function
  | Some t -> pp b t
  | None -> Printf.bprintf b "%s" none

let print_opt ?(none="<none>") print fmt = function
  | Some t -> print fmt t
  | None -> Format.fprintf fmt "%s" none


(* Lists *)
(* ************************************************************************* *)

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

let list_concat_map f l =
  let rec aux f acc = function
    | [] -> List.rev acc
    | x :: l ->
       let xs = f x in
       aux f (List.rev_append xs acc) l
  in aux f [] l

let list_map_sharing f l =
  let l' = List.map f l in
  if List.for_all2 (==) l l' then l else l'

let list_partition_map f l =
  let rec aux f l1 l2 = function
    | [] -> List.rev l1, List.rev l2
    | x :: r ->
      begin match f x with
        | `Left y -> aux f (y :: l1) l2 r
        | `Right z -> aux f l1 (z :: l2) r
      end
  in
  aux f [] [] l


(* Iteration *)
(* ************************************************************************* *)

let rec foldn n f acc =
  if n <= 0 then acc
  else foldn (n - 1) f (f acc)

(* Lexbufs *)
(* ************************************************************************* *)

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
    let ch = open_in_bin s in
    let cl = (fun () -> close_in ch) in
    let buf = Lexing.from_channel ch in
    set_file buf filename;
    buf, cl

let lex_string rule s =
  rule (Lexing.from_string s)

