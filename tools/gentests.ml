
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)


(* Helper functions *)
(* ************************************************************************* *)

let incr_output_of_problem file =
  Filename.chop_extension file ^ ".incremental"

let full_output_of_problem file =
  Filename.chop_extension file ^ ".full"

let expected_of_problem file =
  Filename.chop_extension file ^ ".expected"

let response_of_problem file =
  match Filename.extension file with
  | ".smt2" -> Some (Filename.chop_extension file ^ ".rsmt2")
  | _ -> None

let supports_incremental file =
  match Filename.extension file with
  | ".ae" -> false
  | _ -> true

let is_a_pb file =
  match Filename.extension file with
  | ".ae"
  | ".cnf"
  | ".icnf"
  | ".smt2"
  | ".psmt2"
  | ".p"
  | ".zf"
    -> true
  | _
    -> false


(* Unix helper functions *)
(* ************************************************************************* *)

(* touch the file *)
let touch file contents =
  if Sys.file_exists file then
    true
  else
    let ch = open_out file in
    output_string ch contents;
    let () = close_out ch in
    false

(* read a file and print it out *)
let cat fmt file =
  let ch = open_in file in
  try while true do
      let s = input_line ch in
      Format.fprintf fmt "%s@\n" s
    done
  with End_of_file ->
    Format.fprintf fmt "@."

(* does the file exists ? *)
let exists file =
  match open_in file with
  | _ -> true
  | exception Sys_error _ -> false

(* is the file empty ? *)
let is_empty file =
  let ch = open_in file in
  try
    let _ = input_char ch in
    close_in ch;
    false
  with End_of_file ->
    close_in ch;
    true

(* Read all the contents of a file *)
let read_all ch =
  let b = Buffer.create 113 in
  try
    while true do
      Buffer.add_channel b ch 30
    done;
    assert false
  with End_of_file ->
    Buffer.contents b

(* grep a string in a file *)
let contains pattern file =
  let cmd = Format.asprintf {|grep -q "%s" %s|} pattern file in
  let ch = Unix.open_process_in cmd in
  let _ = read_all ch in
  let res = Unix.close_process_in ch in
  match res with
  | Unix.WEXITED 0 -> true
  | _ -> false

(* Scan a folder *)
let scan_folder path =
  let handle = Unix.opendir path in
  let rec aux files folders h =
    match Unix.readdir h with
    | exception End_of_file ->
      Unix.closedir h;
      List.sort String.compare files,
      List.sort String.compare folders
    | "." | ".." ->
      aux files folders h
    | s when Filename.extension s <> ".t" ->
      let f = Filename.concat path s in
      let stat = Unix.stat f in
      begin match stat.st_kind with
        | Unix.S_REG -> aux (s :: files) folders h
        | Unix.S_DIR -> aux files (s :: folders) h
        | _ -> aux files folders h
      end
    | _ -> aux files folders h
  in
  aux [] [] handle


(* Exit codes *)
(* ************************************************************************* *)

type exit_code =
  | Any     (* Any exit code *)
  | Error   (* Any non-zero exit code *)
  | Success (* Zero exit code *)

let pp_exit_codes fmt = function
  | Success -> Format.fprintf fmt "0"
  | Error -> Format.fprintf fmt "(not 0)"
  | Any -> Format.fprintf fmt "(or 0 (not 0))"


(* Base stanza *)
(* ************************************************************************* *)

let pp_deps fmt (pb_file, response_file, additional) =
  let l = (Format.asprintf "@[<h>(:input %s)@]" pb_file) :: additional in
  let l =
    match response_file with
    | None -> l
    | Some f ->
      (Format.asprintf "@[<h>(:response %s)@]" f) :: l
  in
  Format.pp_print_list Format.pp_print_string fmt l
    ~pp_sep:Format.pp_print_space

let test_stanza_aux ?(deps=[]) mode fmt
    (res_file, pb_file, response_file, exit_codes, expected_file) =
  Format.fprintf fmt "
@[<v 2>(rule@ \
  (target  %s)@ \
  (deps    @[<hov>%a@])@ \
  (package dolmen_bin)@ \
  (action @[<hov 1>(chdir %%{workspace_root}@ \
           @[<hov 1>(with-outputs-to %%{target}@ \
            @[<hov 1>(with-accepted-exit-codes %a@ \
             @[<hov 1>(run dolmen %s--mode=%s --color=never %%{input} %%{read-lines:flags.dune})@]\
             )@]\
             )@]\
             )@]\
             ))@]@\n\
@[<v 2>(rule@ \
  (alias runtest)@ \
  (package dolmen_bin)@ \
  (action (diff %s %s))@])@\n"
    res_file
    pp_deps (pb_file, response_file, deps)
    pp_exit_codes exit_codes
    (match response_file with
     | None -> ""
     | Some _ -> Format.asprintf "--check-model=true -r %%{response} "
    )
    mode expected_file res_file

let test_stanza_incr ?deps fmt ((_, pb_file, _, _, _) as data) =
  if not (supports_incremental pb_file) then ()
  else
    Format.fprintf fmt "; Incremental test@\n%a@\n"
      (test_stanza_aux ?deps "incremental") data

let test_stanza_full ?deps fmt data =
  Format.fprintf fmt "; Full mode test@\n%a@\n"
    (test_stanza_aux ?deps "full") data

let test_stanza ?deps fmt (exit_codes, pb_file, response_file) =
  let incr_file = incr_output_of_problem pb_file in
  let full_file = full_output_of_problem pb_file in
  let expected_file = expected_of_problem pb_file in
  Format.fprintf fmt "; Test for %s@\n%a%a@\n" pb_file
    (test_stanza_incr ?deps) (incr_file, pb_file, response_file, exit_codes, expected_file)
    (test_stanza_full ?deps) (full_file, pb_file, response_file, exit_codes, expected_file)



(* Generating a test case *)
(* ************************************************************************* *)

let check_expect_file path =
  let default_expect_contents = "run 'make promote' to update this file" in
  if touch path default_expect_contents then
    if is_empty path then Success
    else if contains "Error" path then Error
    else Any
  else
    Success

let test_deps path pb =
  match Filename.extension pb with
  | ".p" ->
    if Sys.file_exists (Filename.concat path "Axioms") then
      ["(glob_files Axioms/*.ax)"]
    else
      []
  | _ -> []

let gen_test fmt path pb =
  (* check exit codes and the expected output *)
  let expected_file = Filename.concat path (expected_of_problem pb) in
  let exit_codes = ignore (check_expect_file expected_file); Any in
  (* see whether a response file exists *)
  let response_file =
    match response_of_problem pb with
    | None -> None
    | Some f ->
      let full_path = Filename.concat path f in
      if exists full_path then Some f else None
  in
  (* check for deps *)
  let deps = test_deps path pb in
  test_stanza ~deps fmt (exit_codes, pb, response_file)


(* Generating tests for a folder and its files *)
(* ************************************************************************* *)

let is_not_empty_or_delete file =
  if Sys.file_exists file then
    let ch = open_in file in
    try
      let _ = input_char ch in
      true
    with End_of_file ->
      let () = Sys.remove file in
      false
  else
    false

let gen_tests path files =
  match List.filter is_a_pb files with
  | [] -> ()
  | pbs ->
    let _ = touch (Filename.concat path "flags.dune") "" in
    let ch = open_out (Filename.concat path "dune") in
    let fmt = Format.formatter_of_out_channel ch in
    let () = Format.fprintf fmt "; File auto-generated by gentests.ml@\n@\n" in
    let () =
      let templ = Filename.concat path "dune.templ" in
      if is_not_empty_or_delete templ then begin
        Format.fprintf fmt
          "; Template begin@\n%a@\n; Template end@\n@\n"
          cat templ
      end
    in
    let () = Format.fprintf fmt "; Auto-generated part begin@\n" in
    let () = List.iter (gen_test fmt path) pbs in
    let () = Format.fprintf fmt "; Auto-generated part end@." in
    close_out ch


(* Generating tests recursively for a folder and its children *)
(* ************************************************************************* *)

let rec gen_tests_rec path =
  let files, folders = scan_folder path in
  let () = gen_tests path files in
  List.iter gen_tests_rec (List.map (Filename.concat path) folders)


(* Main point of entry *)
(* ************************************************************************* *)

let () =
  let root = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "." in
  gen_tests_rec root

