let conj_list collection value = value :: collection
let conj_seq collection value = Seq.cons value collection
let conj_vector collection value = Rrbvec.push_back collection value
let conj_set collection value = Runtime_poly_set.add value collection
let disjoin_poly_set collection value = Runtime_poly_set.remove value collection
let disjoin_nil collection _value = collection
let empty_poly_set _ = Runtime_poly_set.empty
let empty_list _ = []
let empty_vector _ = Rrbvec.empty
let empty_string _ = ""
let peek_vector values = Option.get (Rrbvec.peek_back values)
let pop_vector values = snd (Option.get (Rrbvec.pop_back values))
let peek_nil _ = None
let pop_nil _ = None

let count_list = List.length
let count_vector = Rrbvec.length
let count_set = Runtime_poly_set.cardinal
let count_array = Array.length
let count_string = String.length
let count_host_list = List.length
let count_host_array = Array.length

let clone_list values = List.map Fun.id values
let clone_vector = Rrbvec.copy
