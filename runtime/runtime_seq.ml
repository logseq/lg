type 'a t = 'a Seq.t

let memoize sequence = Seq.memoize sequence
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
  let rec loop seen sequence =
    memoize (fun () ->
        match sequence () with
        | Seq.Nil -> Seq.Nil
        | Seq.Cons (value, rest) ->
            if List.exists (equal value) seen then (loop seen rest) ()
            else Seq.Cons (value, loop (value :: seen) rest))
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

let take count sequence = sequence |> Seq.take count |> memoize

let drop count sequence =
  if count = 1 then
    match sequence () with Seq.Nil -> Seq.empty | Seq.Cons (_, rest) -> rest
  else sequence |> Seq.drop count |> memoize

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
  let rec next value () = Seq.Cons (value, next (Int64.add value step)) in
  next start |> memoize

let range_until start stop step =
  if step = 0L then invalid_arg "range step cannot be 0"
  else
    let rec next value () =
      if (step > 0L && value >= stop) || (step < 0L && value <= stop) then
        Seq.Nil
      else Seq.Cons (value, next (Int64.add value step))
    in
    next start |> memoize

let first sequence =
  match sequence () with
  | Seq.Nil -> invalid_arg "first of empty sequence"
  | Seq.Cons (value, _) -> value

let first_opt sequence =
  match sequence () with Seq.Nil -> None | Seq.Cons (value, _) -> Some value

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
