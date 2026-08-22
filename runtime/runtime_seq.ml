type 'a t = 'a Seq.t

let memoize sequence = Seq.memoize sequence
let[@warning "-32"] empty _sequence = Seq.empty
let[@warning "-32"] realized _sequence = false

let defer thunk =
  let sequence = lazy (thunk ()) in
  fun () -> Lazy.force sequence ()

let transformer_sequence transform source =
  let front = ref [] in
  let back = ref [] in
  let enqueue value = back := value :: !back in
  let dequeue () =
    match !front with
    | value :: rest ->
        front := rest;
        Some value
    | [] -> (
        match List.rev !back with
        | [] -> None
        | value :: rest ->
            back := [];
            front := rest;
            Some value)
  in
  let initial = () in
  let downstream =
    ( (fun () -> initial),
      ( (fun result -> result),
        ( (fun result output ->
            enqueue output;
            Runtime_reduced.continue result),
          () ) ) )
  in
  let transformed = transform downstream in
  let complete = fst (snd transformed) in
  let step = fst (snd (snd transformed)) in
  let accumulator = ref initial in
  let remaining = ref source in
  let completed = ref false in
  let finish () =
    if not !completed then (
      accumulator := complete !accumulator;
      completed := true)
  in
  let rec next () =
    match dequeue () with
    | Some value -> Seq.Cons (value, memoize next)
    | None when !completed -> Seq.Nil
    | None -> (
        match (!remaining) () with
        | Seq.Nil ->
            finish ();
            next ()
        | Seq.Cons (input, rest) ->
            remaining := rest;
            let result = step !accumulator input in
            accumulator := Runtime_reduced.unreduced result;
            if Runtime_reduced.is_reduced result then finish ();
            next ())
  in
  memoize next

let rec unfold_memoized step state =
  let node =
    lazy
      (match step state with
      | None -> Seq.Nil
      | Some (value, next_state) ->
          Seq.Cons (value, unfold_memoized step next_state))
  in
  fun () -> Lazy.force node

let rec unfold_chunks step state =
  let chunk =
    lazy
      (match step state with
      | None -> None
      | Some (values, from, until, next_state) ->
          let rest =
            let sequence = lazy (unfold_chunks step (next_state ())) in
            fun () -> Lazy.force sequence ()
          in
          Some (values, from, until, rest))
  in
  let rec emit values index until rest () =
    if index >= until then rest ()
    else
      Seq.Cons
        (Array.unsafe_get values index, emit values (index + 1) until rest)
  in
  fun () ->
    match Lazy.force chunk with
    | None -> Seq.Nil
    | Some (values, from, until, rest) ->
        emit values from until rest ()

let of_list values = values |> List.to_seq |> memoize
let of_vector values = values |> Rrbvec.to_list |> of_list
let of_array values = values |> Array.to_seq |> memoize

let of_array_rev values =
  let rec next index () =
    if index < 0 then Seq.Nil
    else Seq.Cons (Array.get values index, next (index - 1))
  in
  next (Array.length values - 1) |> memoize

let of_string value = value |> String.to_seq |> memoize
let to_list sequence = List.of_seq sequence
let fold_left fn init sequence = Seq.fold_left fn init sequence

let rec for_all predicate sequence =
  match sequence () with
  | Seq.Nil -> true
  | Seq.Cons (value, rest) -> predicate value && for_all predicate rest

let map fn sequence = sequence |> Seq.map fn |> memoize
let mapi fn sequence = sequence |> Seq.mapi fn |> memoize
let filter_map fn sequence = sequence |> Seq.filter_map fn |> memoize
let capture_adapter adapter value _ = adapter value
let map_adapter mapper adapter value = map mapper (adapter value)

let rec map2 fn left right =
  memoize (fun () ->
      match (left (), right ()) with
      | Seq.Cons (left_value, left_rest), Seq.Cons (right_value, right_rest) ->
          Seq.Cons (fn left_value right_value, map2 fn left_rest right_rest)
      | Seq.Nil, _ | _, Seq.Nil -> Seq.Nil)

let flat_map fn sequence = sequence |> Seq.flat_map fn |> memoize
let filter predicate sequence = sequence |> Seq.filter predicate |> memoize

let rec take_while predicate sequence =
  memoize (fun () ->
      match sequence () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (value, rest) ->
          if predicate value then Seq.Cons (value, take_while predicate rest)
          else Seq.Nil)

let rec drop_while predicate sequence =
  memoize (fun () ->
      match sequence () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (value, rest) ->
          if predicate value then (drop_while predicate rest) ()
          else Seq.Cons (value, rest))

let distinct equal sequence =
  let rec loop seen sequence = memoize (fun () -> advance seen sequence)
  and advance seen sequence =
    match sequence () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (value, rest) ->
        if List.exists (equal value) seen then advance seen rest
        else Seq.Cons (value, loop (value :: seen) rest)
  in
  loop [] sequence

let all_distinct equal values =
  let rec loop seen = function
    | [] -> true
    | value :: rest ->
        if List.exists (equal value) seen then false
        else loop (value :: seen) rest
  in
  loop [] values

let rec concat = function
  | [] -> Seq.empty
  | [ sequence ] -> sequence
  | sequence :: rest -> Seq.append sequence (concat rest)

let interleave sequences =
  let rec split heads tails = function
    | [] -> Some (List.rev heads, List.rev tails)
    | sequence :: rest -> (
        match sequence () with
        | Seq.Nil -> None
        | Seq.Cons (head, tail) -> split (head :: heads) (tail :: tails) rest)
  in
  let rec next pending sequences =
    memoize (fun () ->
        match pending with
        | head :: rest -> Seq.Cons (head, next rest sequences)
        | [] -> (
            match split [] [] sequences with
            | None -> Seq.Nil
            | Some (heads, tails) -> (next heads tails) ()))
  in
  next [] sequences

let take count sequence =
  if count <= 0 then Seq.empty else sequence |> Seq.take count |> memoize

let drop count sequence =
  if count <= 0 then sequence
  else if count = 1 then
    match sequence () with Seq.Nil -> Seq.empty | Seq.Cons (_, rest) -> rest
  else sequence |> Seq.drop count |> memoize

let rest sequence = drop 1 sequence
let next sequence = drop 1 sequence
let drop_from_sequence sequence count = drop count sequence
let drop_from_vector vector count = drop count (of_vector vector)

let non_empty sequence =
  match sequence () with Seq.Nil -> None | Seq.Cons _ -> Some sequence

let repeat value = Seq.repeat value |> memoize

let cycle values =
  let values = memoize values in
  match values () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons _ ->
      let rec loop () = Seq.append values loop () in
      memoize loop

let range start step =
  let rec next value () = Seq.Cons (value, next (value + step)) in
  next start |> memoize

let range_until start stop step =
  if step = 0 then
    if start = stop then Seq.empty else range start 0
  else
    let rec next value () =
      if (step > 0 && value >= stop) || (step < 0 && value <= stop) then
        Seq.Nil
      else Seq.Cons (value, next (value + step))
    in
    next start |> memoize

let first sequence =
  match sequence () with
  | Seq.Nil -> invalid_arg "first of empty sequence"
  | Seq.Cons (value, _) -> value

let first_opt sequence =
  match sequence () with Seq.Nil -> None | Seq.Cons (value, _) -> Some value

let uncons sequence =
  match sequence () with
  | Seq.Nil -> None
  | Seq.Cons (value, rest) -> Some (value, rest)

let second sequence =
  match sequence () with
  | Seq.Nil -> invalid_arg "second of empty sequence"
  | Seq.Cons (_, rest) -> first rest

let nth index sequence =
  if index < 0 then invalid_arg "negative sequence index"
  else first (Seq.drop index sequence)

let nth_opt index sequence =
  if index < 0 then None
  else
    match Seq.drop index sequence () with
    | Seq.Nil -> None
    | Seq.Cons (value, _) -> Some value

let last sequence =
  match sequence () with
  | Seq.Nil -> invalid_arg "last of empty sequence"
  | Seq.Cons (value, rest) -> Seq.fold_left (fun _ value -> value) value rest

let last_opt sequence =
  match sequence () with
  | Seq.Nil -> None
  | Seq.Cons (value, rest) ->
      Some (Seq.fold_left (fun _ value -> value) value rest)

let is_empty sequence =
  match sequence () with Seq.Nil -> true | Seq.Cons _ -> false

let zip_vectors collections =
  let length =
    match collections with
    | [] -> 0
    | first :: rest ->
        List.fold_left
          (fun length collection -> min length (Rrbvec.length collection))
          (Rrbvec.length first) rest
  in
  let rows = ref Rrbvec.empty in
  for index = 0 to length - 1 do
    let row =
      collections
      |> List.map (fun collection -> Rrbvec.nth collection index)
      |> Rrbvec.of_list
    in
    rows := Rrbvec.push_back !rows row
  done;
  !rows
