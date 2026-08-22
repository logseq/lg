type value = Lg_edn_backend.t

let map_entry inner (key, value) =
  match inner (Lg_edn_backend.Vector [| key; value |]) with
  | Lg_edn_backend.Vector [| key; value |]
  | Lg_edn_backend.List [| key; value |] ->
      (key, value)
  | _ -> invalid_arg "clojure.walk map entry must contain two values"

let rebuild_map entries =
  let values = Hashtbl.create (Array.length entries) in
  let reversed_keys =
    Array.fold_left
      (fun keys (key, value) ->
        let keys = if Hashtbl.mem values key then keys else key :: keys in
        Hashtbl.replace values key value;
        keys)
      [] entries
  in
  reversed_keys |> List.rev
  |> List.map (fun key -> (key, Hashtbl.find values key))
  |> Array.of_list

let rebuild_set values =
  let seen = Hashtbl.create (Array.length values) in
  values
  |> Array.to_list
  |> List.filter (fun value ->
         if Hashtbl.mem seen value then false
         else (
           Hashtbl.add seen value ();
           true))
  |> Array.of_list

let rec walk inner form =
  let open Lg_edn_backend in
  match form with
  | List values -> List (Array.map inner values)
  | Seq values -> Seq (Stdlib.Seq.map inner values)
  | Vector values -> Vector (Array.map inner values)
  | Int4_vector (first, second, third, fourth) ->
      Vector
        (Array.map inner
           [| Small_int first; Small_int second; third; Small_int fourth |])
  | Int4_array (entities, attributes, values, txs) ->
      Vector
        (Array.mapi
           (fun index value ->
             inner
               (Int4_vector
                  (entities.(index), attributes.(index), value, txs.(index))))
           values)
  | Int_vector values ->
      Vector (Array.map (fun value -> inner (Small_int value)) values)
  | Map entries ->
      Map (entries |> Array.map (map_entry inner) |> rebuild_map)
  | Set values -> Set (values |> Array.map inner |> rebuild_set)
  | Json_source source -> walk inner (Lg_edn_backend.of_json_string source)
  | ( Nil | Bool _ | String _ | Char _ | Symbol _ | Keyword _ | Small_int _
    | Int _ | Bigint _ | Float _ | Decimal _ | Ratio _ | Regex _ | Tagged _ )
    as value ->
      value

let keywordize_map_keys = function
  | Lg_edn_backend.Map entries ->
      Lg_edn_backend.Map
        (Array.map
           (function
             | Lg_edn_backend.String key, value ->
                 (Lg_edn_backend.Keyword key, value)
             | entry -> entry)
           entries
        |> rebuild_map)
  | value -> value

let stringify_map_keys = function
  | Lg_edn_backend.Map entries ->
      Lg_edn_backend.Map
        (Array.map
           (function
             | Lg_edn_backend.Keyword key, value ->
                 (Lg_edn_backend.String key, value)
             | entry -> entry)
           entries
        |> rebuild_map)
  | value -> value

let replace replacements value =
  match replacements with
  | Lg_edn_backend.Map entries ->
      entries
      |> Array.find_map (fun (candidate, replacement) ->
             if Runtime_edn.equal candidate value then Some replacement
             else None)
      |> Option.value ~default:value
  | _ -> invalid_arg "clojure.walk replacement map must be an EDN map"
