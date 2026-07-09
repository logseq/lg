module Int_set = Set.Make (Int)
module String_set = Set.Make (String)

module Bool_order = struct
  type t = bool

  let compare = Bool.compare
end

module Bool_set = Set.Make (Bool_order)
