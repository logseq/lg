let length = Array.length
let copy = Array.copy
let of_seq = Array.of_seq
let of_list = Array.of_list
let of_vector values = Array.of_seq (Rrbvec.to_seq values)

let of_seq_padded size initial values =
  let result = Array.make size initial in
  let rec fill index remaining =
    if index < size then
      match remaining () with
      | Seq.Nil -> result
      | Seq.Cons (value, rest) ->
          result.(index) <- value;
          fill (index + 1) rest
    else result
  in
  fill 0 values

let of_list_padded size initial values =
  of_seq_padded size initial (List.to_seq values)

let of_vector_padded size initial values =
  of_seq_padded size initial (Rrbvec.to_seq values)

let of_array_padded size initial values =
  of_seq_padded size initial (Array.to_seq values)

let copy_range source source_start source_end target target_start =
  Array.blit source source_start target target_start (source_end - source_start)

let slice source from to_ = Array.sub source from (to_ - from)

let append = Array.append

let sort values compare = Array.fast_sort compare values
