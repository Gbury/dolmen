
(* Bitvector manipulation *)
(* ************************************************************************ *)

module Bitv = struct

  exception Invalid_char of char


  (* Bitv in binary forms *)

  let check_bin = function
    | '0' | '1' -> ()
    | c -> raise (Invalid_char c)

  let parse_binary s =
    assert (String.length s > 2 && s.[0] = '#' && s.[1] = 'b');
    let s' = String.sub s 2 (String.length s - 2) in
    String.iter check_bin s';
    s'


  (* Bitv in hexadecimal form *)

  let hex_to_bin = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'a' | 'A' -> "1010"
    | 'b' | 'B' -> "1011"
    | 'c' | 'C' -> "1100"
    | 'd' | 'D' -> "1101"
    | 'e' | 'E' -> "1110"
    | 'f' | 'F' -> "1111"
    | c -> raise (Invalid_char c)

  let parse_hexa s =
    assert (String.length s > 2 && s.[0] = '#' && s.[1] = 'x');
    let s' = String.sub s 2 (String.length s - 2) in
    let b = Bytes.create ((String.length s - 2) * 4) in
    String.iteri (fun i c ->
        Bytes.blit_string (hex_to_bin c) 0 b (i * 4) 4
      ) s';
    let s'' = Bytes.to_string b in
    s''


  (* bitv in decimal form *)

  let int_of_char = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    |  c  -> raise (Invalid_char c)

  let char_of_int = function
    | 0 -> '0'
    | 1 -> '1'
    | 2 -> '2'
    | 3 -> '3'
    | 4 -> '4'
    | 5 -> '5'
    | 6 -> '6'
    | 7 -> '7'
    | 8 -> '8'
    | 9 -> '9'
    | _ -> assert false

  (* Implementation of division by 2 on strings, and
     of parsing a string-encoded decimal into a binary bitv,
     taken from
     https://stackoverflow.com/questions/11006844/convert-a-very-large-number-from-decimal-string-to-binary-representation/11007021#11007021 *)

  let divide_string_by_2 b start =
    let b' = Bytes.create (Bytes.length b - start) in
    let next_additive = ref 0 in
    for i = start to Bytes.length b - 1 do
      let c = int_of_char (Bytes.get b i) in
      let additive = !next_additive in
      next_additive := if c mod 2 = 1 then 5 else 0;
      let c' = c / 2 + additive in
      Bytes.set b' (i - start) (char_of_int c')
    done;
    b'

  let rec first_non_zero b start =
    if start >= Bytes.length b then
      None
    else if Bytes.get b start = '0' then
      first_non_zero b (start + 1)
    else
      Some start

  let rec parse_decimal_aux res idx b start =
    if idx < 0 then
      res
    else begin
      (* if b is odd, set the bit in res to 1 *)
      let c = int_of_char (Bytes.get b (Bytes.length b - 1)) in
      if c mod 2 = 1 then Bytes.set res idx '1';
      (* divide b by 2 *)
      let b' = divide_string_by_2 b start in
      match first_non_zero b' 0 with
      | Some start' -> parse_decimal_aux res (idx - 1) b' start'
      | None -> res
    end

  let parse_decimal s n =
    assert (String.length s > 2 && s.[0] = 'b' && s.[1] = 'v');
    let b = Bytes.of_string (String.sub s 2 (String.length s - 2)) in
    let b' = parse_decimal_aux (Bytes.make n '0') (n - 1) b 0 in
    Bytes.to_string b'

end
