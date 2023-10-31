
module M = Map.Make(String)
module T = Dolmen_std.Maps.String

let add_find =
  QCheck2.Test.make ~count:1_000 ~long_factor:100
    ~name:"Maps.add_find"
    QCheck2.Gen.(list (pair string int))
    (fun l ->
       let m =
         List.fold_left (fun m (k, v) ->
             M.add k v m
           ) M.empty l
       in
       let t =
         List.fold_left (fun t (k, v) ->
             T.add k v t
           ) T.empty l
       in
       List.for_all (fun (k, _) ->
           M.find k m = T.find_exn k t
         ) l
    )


