
module type S = Dolmen_intf.Map.S

module Int : S with type key := int

module String : S with type key := string

module Make(Ord : Map.OrderedType) : S with type key := Ord.t



