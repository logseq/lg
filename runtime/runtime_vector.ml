let contains_index vector index =
  index >= 0 && index < Rrbvec.length vector

let assoc vector index value = Rrbvec.set vector index value
let rseq vector = Rrbvec.rev vector
let nth vector index = Rrbvec.nth vector index

let nth_default vector index not_found =
  if contains_index vector index then Rrbvec.nth vector index else not_found
