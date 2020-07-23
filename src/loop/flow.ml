
(* Types *)
(* ************************************************************************ *)

type header_mode =
  | Lang_Version
  | Problem_Logic
  | Problem_Source
  | Problem_License
  | Problem_Category
  | Problem_Status

type solve_mode =
  | Assert
  | Sat_or_unsat

type mode =
  | Start of { expect : header_mode; }
  | Solve of { current : solve_mode; stack_depth : int; }
  | Exited


