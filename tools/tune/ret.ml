
type 'a t = [
  | `Ok of 'a
  | `Error of bool * string
  | `Help of Cmdliner.Manpage.format * string option
]

let (let*) o f =
  match o with
  | `Ok res -> f res
  | `Error _ | `Help _ as ret -> ret


