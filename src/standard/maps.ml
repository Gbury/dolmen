
module type S = Dolmen_intf.Map.S

module Make(Ord : Map.OrderedType) = struct

  include Map.Make(Ord)

  let find_exn = find

  let find_opt k t =
    match find k t with
    | res -> Some res
    | exception Not_found -> None

  let find_add k f t =
    update k (fun o -> Some (f o)) t

end

module Int = Make(Int)

module String = Maps_string

