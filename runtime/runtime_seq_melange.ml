external call1 : ('a -> 'b) -> int -> 'a -> 'b = "call" [@@mel.send]
external call0 : (unit -> 'a) -> int -> unit -> 'a = "call" [@@mel.send]
external call2 : ('a -> 'b -> 'c) -> int -> 'a -> 'b -> 'c = "call"
  [@@mel.send]

let defer thunk =
  let sequence = lazy (call0 thunk 0 ()) in
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
  let transformed = call1 transform 0 downstream in
  let complete = fst (snd transformed) in
  let step = fst (snd (snd transformed)) in
  let accumulator = ref initial in
  let remaining = ref source in
  let completed = ref false in
  let finish () =
    if not !completed then (
      accumulator := call1 complete 0 !accumulator;
      completed := true)
  in
  let rec next () =
    match dequeue () with
    | Some value -> Seq.Cons (value, Seq.memoize next)
    | None when !completed -> Seq.Nil
    | None -> (
        match (!remaining) () with
        | Seq.Nil ->
            finish ();
            next ()
        | Seq.Cons (input, rest) ->
            remaining := rest;
            let result = call2 step 0 !accumulator input in
            accumulator := Runtime_reduced.unreduced result;
            if Runtime_reduced.is_reduced result then finish ();
            next ())
  in
  Seq.memoize next

let unfold step initial_state =
  let state = ref initial_state in
  let rec next () =
    match call1 step 0 !state with
    | None -> Seq.Nil
    | Some (value, next_state) ->
        state := next_state;
        Seq.Cons (value, next)
  in
  next

let rec unfold_memoized step state =
  let node =
    lazy
      (match call1 step 0 state with
      | None -> Seq.Nil
      | Some (value, next_state) ->
          Seq.Cons (value, unfold_memoized step next_state))
  in
  fun () -> Lazy.force node

let rec unfold_chunks step state =
  let chunk =
    lazy
      (match call1 step 0 state with
      | None -> None
      | Some (values, from, until, next_state) ->
          let rest =
            let sequence =
              lazy (unfold_chunks step (call0 next_state 0 ()))
            in
            fun () -> Lazy.force sequence ()
          in
          Some (values, from, until, rest))
  in
  let rec emit values index until rest () =
    if index >= until then rest ()
    else
      Seq.Cons
        (Array.get values index, emit values (index + 1) until rest)
  in
  fun () ->
    match Lazy.force chunk with
    | None -> Seq.Nil
    | Some (values, from, until, rest) ->
        emit values from until rest ()
