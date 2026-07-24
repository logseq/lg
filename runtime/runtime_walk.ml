let preserve_properties original rebuilt =
  {
    rebuilt with
    Runtime_dynamic.metadata = original.Runtime_dynamic.metadata;
    type_name = original.Runtime_dynamic.type_name;
  }

let map_entry inner key value =
  let entry =
    Runtime_dynamic.vector (Rrbvec.of_list [ key; value ]) |> inner
  in
  match List.of_seq (Runtime_dynamic.to_seq entry) with
  | [ key; value ] -> (key, value)
  | _ -> invalid_arg "clojure.walk map entry must contain two values"

let walk inner outer form =
  let walked =
    match form.Runtime_dynamic.payload with
    | Runtime_dynamic.List ->
        form
        |> Runtime_dynamic.to_seq
        |> Seq.map inner
        |> List.of_seq
        |> Runtime_dynamic.list
        |> preserve_properties form
    | Runtime_dynamic.Vector _ ->
        form
        |> Runtime_dynamic.to_seq
        |> Seq.map inner
        |> List.of_seq
        |> Rrbvec.of_list
        |> Runtime_dynamic.vector
        |> preserve_properties form
    | Runtime_dynamic.Seq ->
        form
        |> Runtime_dynamic.to_seq
        |> Seq.map inner
        |> List.of_seq
        |> List.to_seq
        |> Runtime_dynamic.seq
        |> preserve_properties form
    | Runtime_dynamic.Set set ->
        set.values
        |> List.to_seq
        |> Seq.map inner
        |> Runtime_dynamic.set
        |> preserve_properties form
    | Runtime_dynamic.Map map ->
        Runtime_dynamic.map_entries map
        |> List.map (fun (key, value) -> map_entry inner key value)
        |> Runtime_dynamic.map
        |> preserve_properties form
    | _ -> form
  in
  outer walked

let rec postwalk transform form = walk (postwalk transform) transform form

let rec prewalk transform form =
  walk (prewalk transform) Fun.id (transform form)
