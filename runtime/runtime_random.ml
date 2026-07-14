let initialized = lazy (Random.self_init ())

let rand_int bound =
  if bound <= 0 then invalid_arg "rand-int expects a positive bound";
  Lazy.force initialized;
  Random.int bound
