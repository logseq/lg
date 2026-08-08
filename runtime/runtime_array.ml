let length = Array.length

let copy_range source source_start source_end target target_start =
  Array.blit source source_start target target_start (source_end - source_start)

let slice source from to_ = Array.sub source from (to_ - from)

let append = Array.append

let sort values compare = Array.fast_sort compare values
