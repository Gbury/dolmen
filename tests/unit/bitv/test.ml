
module B = Dolmen_type.Misc.Bitv

let print s = Format.printf "%s@." s

let () =
  Printexc.record_backtrace true;

  (* Testing binary parsing (not much chance of it being wrong) *)
  print @@ B.parse_binary "#b011010";
  print @@ B.parse_binary "#b111001";
  begin try
      ignore (B.parse_binary "#b10a0101");
      print "fail !"
    with B.Invalid_char 'a' ->
      print "ok"
  end;

  (* Testing hexadecimal parsing *)
  print @@ B.parse_hexa "#x10";
  print @@ B.parse_hexa "#x6B2D";
  print @@ B.parse_hexa "#x123456789abcdef";

  (* Testing decimal parsing (much more likely to have bugs, given
     the complexity of the code) *)
  print @@ B.parse_decimal "bv10" 2;
  print @@ B.parse_decimal "bv10" 5;
  print @@ B.parse_decimal "bv999" 5;
  print @@ B.parse_decimal "bv999" 10;
  print @@ B.parse_decimal
    "bv9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
    334;

  (* end *)
  ()
