external call1 : ('a -> 'b) -> int -> 'a -> 'b = "call" [@@mel.send]

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
