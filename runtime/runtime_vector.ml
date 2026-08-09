let contains_index vector index =
  index >= 0 && index < Rrbvec.length vector

let assoc vector index value = Rrbvec.set vector index value
let rseq vector = Rrbvec.rev vector
