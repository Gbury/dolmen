
module Int = struct

  type t = {
    min : int;
    step : int;
    max : int;
  }

  let mk name ~min_limit ~max_limit (min, step, max) =
    if min < min_limit then
      `Error (false,
          Format.asprintf
            "The minimum value of a range for %s is %d, \
             which is strictly smaller than the provided %d"
            name min_limit min
        )
    else if max > max_limit then
      `Error (false,
          Format.asprintf
            "The maximum value of a range for %s is %d, \
             which is strictly smaller than the provided %d"
            name max_limit max
        )
    else
      `Ok { min; step; max; }

  let to_seq { min; step; max; } =
    Seq.unfold (fun acc ->
        if acc > max then None
        else begin
          let acc' = acc + step in
          Some (acc, acc')
        end
      ) min

end

