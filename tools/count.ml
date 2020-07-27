
(* Log base 2 *)
let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

(* Count number of lines and maximum of columns *)
let count_lines_and_columns file =
  let ch = open_in file in
  let n_lines = ref 0 in
  let max_col = ref 0 in
  try while true do
      let s = input_line ch in
      n_lines := !n_lines + 1;
      max_col := max !max_col (String.length s);
      ()
    done;
    assert false
  with End_of_file ->
    close_in ch;
    !n_lines, !max_col

(* Scan a folder *)
let rec iter_folder_rec path f =
  let handle = Unix.opendir path in
  let rec aux f h =
    match Unix.readdir h with
    | exception End_of_file ->
      Unix.closedir h
    | "." | ".." ->
      aux f h
    | s ->
      let s = Filename.concat path s in
      let stat = Unix.stat s in
      begin match stat.st_kind with
        | Unix.S_REG -> f s
        | Unix.S_DIR -> iter_folder_rec s f
        | _ -> aux f h
      end;
      aux f h
  in
  aux f handle

(* Count and acc for one file *)
let count_one_file (line, col, sum) file =
  (* Format.eprintf "counting %s@." file; *)
  let lines, cols = count_lines_and_columns file in
  let log_l = log2 lines in
  let log_c = log2 cols in
  max line log_l, max col log_c, max sum (log_l + log_c)

(* Count and acc for one dir *)
let count_one_dir acc dir =
  let r = ref acc in
  iter_folder_rec dir
    (fun file -> r := count_one_file !r file);
  !r


(* Main function *)
let main () =
  let l = ref [] in
  Arg.parse [] (fun s -> l := s :: !l) "count [file | folder]*";
  let (lines, chars, sum) = List.fold_left (fun acc s ->
      match (Unix.stat s).st_kind with
      | Unix.S_REG -> count_one_file acc s
      | Unix.S_DIR -> count_one_dir acc s
      | _ -> acc
    ) (0, 0, 0) !l
  in
  Format.printf "%d bits required for lines at max@\n%d bits required for cols at max\n%d bits required for both at max@."
    lines chars sum;
  ()

let () = main ()
