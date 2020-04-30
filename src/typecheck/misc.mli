
(* Bitvector helpers *)
module Bitv : sig

  exception Invalid_char of char
  (** Excpetion raised by functions in this module when a non-valid
      character is encountered in the parsing functions. *)

  val parse_binary : string -> string
  (** Parse a string of the form "#bXXXXXXX" (with X a binary
      character, i.e. either '1' or '0'), into a binary
      representation of the bitvector.
      In this case, it means taking the substring and checking
      that all characters are valid.
      @raise Invalid_char if the input string contains an non-binary
        character *)

  val parse_hexa : string -> string
  (** Parse a string of the form "#xXXXXXXX" (with X a hexadecimal
      character, i.e. in the '0'-'f' range), into a binary
      representation of the bitvector.
      @raise Invalid_char if the input string contains a non-hexa
        character *)

  val parse_decimal : string -> int -> string
  (** Parse a string of the form "bvXXXXXXX" (with X a decimal
      character, i.e. in the '0'-'9' range), into a binary bitvector
      of the given size (second argument), which is the representation
      of the decimal integer (or a truncated version if the size given
      is smaller thant the required size for the given integer).
      @raise Invalid_char if the input string contains a non-decimal
        character *)

end
