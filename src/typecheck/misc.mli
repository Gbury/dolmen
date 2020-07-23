
(** Misc

    Collection of various helper functions, some rather specific to
    dolmen (such as the bitv manipulation module), and others more
    generic (e.g. the {List} and {Option} modules. For these generic
    functions, they are here either because they do not exist in the
    stdlib and they did not (yet) warrant depending on an external
    alternative stdlib, or because they were added in recent versions
    of ocaml and thus would unnecessarily restrict the availability
    of dolmen on older versions of ocaml.
*)

(** Option helpers *)
module Options : sig

  val map : ('a -> 'b) -> 'a option -> 'b option
  (** Map over options. *)

end

(** List helpers *)
module Lists : sig

  val init : int -> (int -> 'a) -> 'a list
  (** Create a list of the given length by calling th e givne function
      for each element (starting from [0] up until the length minus one. *)

  val replicate : int -> 'a -> 'a list
  (** Create a list containing n-times the given element. *)

  val take_drop : int -> 'a list -> 'a list * 'a list
  (** [take_drop n l] tries and split the list into a first list
      containing the first [n] elements of [l], and in a second list
      the rest.
      @raise Invalid_argument if [l] has less than [n] elements. *)

end

(** String helper *)
module Strings : sig

  val to_list : string -> char list
  (** Explode the string into a list of characters. *)

  val is_suffix : suffix:string -> string -> bool
  (** Check whether a string is a suffix of another. *)

end

(** Fuzzy search maps *)
module Fuzzy_Map : sig

  type 'a t
  (** The type of Fuzzy maps from Dolmen identifiers
      to values of type ['a] *)

  val empty : _ t
  (** The empty fuzzy search map. *)

  val mem : 'a t -> Dolmen.Std.Id.t -> bool
  (** Test whether the given id is bound in the map. *)

  val find : 'a t -> Dolmen.Std.Id.t -> 'a
  (** Find the value bound to an id.
      @raise Not_found if the id is not bound. *)

  val add : 'a t -> Dolmen.Std.Id.t -> 'a -> 'a t
  (** Add a new binding to the map. *)

  val suggest : 'a t -> limit:int -> Dolmen.Std.Id.t -> Dolmen.Std.Id.t list
  (** Return a list of bound identifiers in the map that are close
      (up to [limit] in terms of edition distance) to the given id. *)

end


(** Fuzzy search Hashtables *)
module Fuzzy_Hashtbl : sig

  type 'a t
  (** The type of fuzzy hashtables from Dolmen identifiers
      to values of type ['a]. *)

  val create : unit -> _ t
  (** Create a new fuzzy hashtable. *)

  val find : 'a t -> Dolmen.Std.Id.t -> 'a
  (** Find the value bound to the given Identifier
      @raise Not_found if the id is not bound. *)

  val add : 'a t -> Dolmen.Std.Id.t -> 'a -> unit
  (** Add a new binding to the hashtable. *)

  val suggest : 'a t -> limit:int -> Dolmen.Std.Id.t -> Dolmen.Std.Id.t list
  (** Return a list of bound identifiers in the hashtbl that are close
      (up to [limit] in terms of edition distance) to the given id. *)

end

(** Bitvector helpers *)
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
