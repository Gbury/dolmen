

module Int = struct

  type t = int Seq.t

  let of_list l = List.to_seq l

  let range ~min ~step ~max =
    Seq.unfold (fun acc ->
        if acc > max then None
        else begin
          let acc = acc + step in
          Some (acc, acc)
        end
      ) min

end


