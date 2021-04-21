
type ('ok, 'err) t = ('ok, 'err) Result.t

let ( let+ ) o f =
  match o with
  | Ok res -> f res
  | Error _ as ret -> ret

