external call1 : ('a -> 'b) -> int -> 'a -> 'b = "call" [@@mel.send]
external call0 : (unit -> 'a) -> int -> unit -> 'a = "call" [@@mel.send]

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
