let initialized = lazy (Random.self_init ())

let seed value =
  Lazy.force initialized;
  Random.init value

let rand_int bound =
  if bound <= 0 then invalid_arg "rand-int expects a positive bound";
  Lazy.force initialized;
  Random.int bound

let rand bound =
  if bound < 0. then invalid_arg "rand expects a non-negative bound";
  Lazy.force initialized;
  Random.float bound

let rand_nth values =
  match values with
  | [] -> invalid_arg "rand-nth expects a non-empty collection"
  | _ -> List.nth values (rand_int (List.length values))

let shuffle values =
  Lazy.force initialized;
  let values = Array.of_list values in
  for index = Array.length values - 1 downto 1 do
    let swap_index = Random.int (index + 1) in
    let value = values.(index) in
    values.(index) <- values.(swap_index);
    values.(swap_index) <- value
  done;
  Array.to_list values
