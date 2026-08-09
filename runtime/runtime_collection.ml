let conj_list collection value = value :: collection
let conj_seq collection value = Seq.cons value collection
let conj_vector collection value = Rrbvec.push_back collection value
let conj_set collection value = Runtime_poly_set.add value collection
let disjoin_poly_set collection value = Runtime_poly_set.remove value collection
