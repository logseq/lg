type tap = {
  identity : string option;
  run : Lg_edn_backend.t -> unit;
}

let taps : tap list ref = ref []

let same_tap left right =
  match (left.identity, right.identity) with
  | Some left, Some right -> String.equal left right
  | None, None -> left.run == right.run
  | (Some _ | None), (Some _ | None) -> false

let add identity run =
  let tap = { identity; run } in
  if not (List.exists (same_tap tap) !taps) then taps := tap :: !taps

let remove identity run =
  let tap = { identity; run } in
  taps := List.filter (fun existing -> not (same_tap existing tap)) !taps

let exec thunk =
  thunk ();
  true

let tap value =
  exec (fun () ->
      !taps
      |> List.rev
      |> List.iter (fun tap ->
             try tap.run value with _ -> ()))
