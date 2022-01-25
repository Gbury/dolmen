
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Type definition *)

type t = string

module H = Weak.Make(struct

    type t = string

    let hash s = Hashtbl.hash s
    let equal s s' = String.equal s s'

  end)

let table = H.create 113

let sizes = Array.make 1000 0

let () = at_exit (fun () ->
    Format.eprintf "sizes:@.";
    Array.iteri (fun i n ->
        if n <> 0 then Format.eprintf "%d: %d@." i n
      ) sizes
  )

let mk s =
  let i = String.length s in
  sizes.(i) <- sizes.(i) + 1;
  (* H.merge table s *)
  s

