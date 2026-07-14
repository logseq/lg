let hash_combine seed hash_value =
  let open Int32 in
  let seed = of_int seed in
  let hash = of_int hash_value in
  let mixed =
    add hash
      (add (-1640531527l)
         (add (shift_left seed 6) (shift_right seed 2)))
  in
  to_int (logxor seed mixed)
