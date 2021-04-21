
let mk ~file results ~x ~y =
  let points = List.map (fun result ->
      let x = x result in
      let y = y result in
      x, y
    ) results in
  let series = Gnuplot.Series.points_xy points in
  let output = Gnuplot.Output.create (`Png file) in
  let t = Gnuplot.create () in
  let () = Gnuplot.plot ~output ~use_grid:true t series in
  let () = Gnuplot.close t in
  ()

