module Int_set = Set.Make (Int)
module Float_set = Set.Make (Float)
module Char_set = Set.Make (Char)
module String_set = Set.Make (String)

module Bool_order = struct
  type t = bool

  let compare left right = Bool.compare right left
end

module Bool_set = Set.Make (Bool_order)

module Int_list_set = Set.Make (struct
  type t = int list

  let compare = Stdlib.compare
end)

module Float_list_set = Set.Make (struct
  type t = float list

  let compare = Stdlib.compare
end)

module Char_list_set = Set.Make (struct
  type t = char list

  let compare = Stdlib.compare
end)

module String_list_set = Set.Make (struct
  type t = string list

  let compare = Stdlib.compare
end)

module Bool_list_set = Set.Make (struct
  type t = bool list

  let compare = Stdlib.compare
end)

module Int_vector_set = Set.Make (struct
  type t = int Rrbvec.t

  let compare = Stdlib.compare
end)

module Float_vector_set = Set.Make (struct
  type t = float Rrbvec.t

  let compare = Stdlib.compare
end)

module Char_vector_set = Set.Make (struct
  type t = char Rrbvec.t

  let compare = Stdlib.compare
end)

module String_vector_set = Set.Make (struct
  type t = string Rrbvec.t

  let compare = Stdlib.compare
end)

module Bool_vector_set = Set.Make (struct
  type t = bool Rrbvec.t

  let compare = Stdlib.compare
end)

module Int_vector_vector_set = Set.Make (struct
  type t = int Rrbvec.t Rrbvec.t

  let compare = Stdlib.compare
end)
