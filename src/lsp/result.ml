
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Result

    Extension of the Result module to also include monadinc let operators
*)

include Stdlib.Result

(* Let operators *)
(* ************************************************************************ *)

let ( let+ ) o f =
  match o with
  | Ok res -> f res
  | Error _ as ret -> ret
