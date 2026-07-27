module Sequence = Lg_runtime.Runtime_seq

let () =
  let count = 20_000 in
  let values =
    Sequence.of_array (Array.init count Fun.id)
    |> Sequence.map (fun value -> value + 1)
    |> Sequence.to_list
  in
  assert (List.length values = count);
  assert (List.hd values = 1);
  assert (List.hd (List.rev values) = count);
  let repeated =
    Sequence.of_array (Array.make count 7)
    |> Sequence.distinct Int.equal |> Sequence.to_list
  in
  assert (repeated = [ 7 ])
