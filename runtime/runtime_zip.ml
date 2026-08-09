let is_sequential = function
  | Lg_edn_backend.List _ | Lg_edn_backend.Vector _
  | Lg_edn_backend.Int4_vector _ | Lg_edn_backend.Int4_array _
  | Lg_edn_backend.Int_vector _ ->
      true
  | _ -> false

let is_vector = function
  | Lg_edn_backend.Vector _ | Lg_edn_backend.Int4_vector _
  | Lg_edn_backend.Int4_array _ | Lg_edn_backend.Int_vector _ ->
      true
  | _ -> false

let sequence_children value =
  match Runtime_edn.sequence_values value with
  | Some values -> Array.to_seq values
  | None -> Seq.empty

let make_sequence_node original children =
  match original with
  | Lg_edn_backend.List _ ->
      Lg_edn_backend.List (Rrbvec.to_array children)
  | _ -> Lg_edn_backend.Vector (Rrbvec.to_array children)

let make_vector_node _original children =
  Lg_edn_backend.Vector (Rrbvec.to_array children)

let is_xml_branch = function Lg_edn_backend.String _ -> false | _ -> true

let xml_children value =
  match value with
  | Lg_edn_backend.Map entries ->
      entries
      |> Array.find_map (function
           | Lg_edn_backend.Keyword "content", content -> Some content
           | _ -> None)
      |> fun content -> Option.bind content Runtime_edn.sequence_values
      |> Option.map Array.to_seq |> Option.value ~default:Seq.empty
  | _ -> Seq.empty

let make_xml_node original children =
  match original with
  | Lg_edn_backend.Map entries ->
      let content = Lg_edn_backend.Vector (Rrbvec.to_array children) in
      let found = ref false in
      let entries =
        Array.map
          (function
            | Lg_edn_backend.Keyword "content", _ ->
                found := true;
                (Lg_edn_backend.Keyword "content", content)
            | entry -> entry)
          entries
      in
      if !found then Lg_edn_backend.Map entries
      else
        Lg_edn_backend.Map
          (Array.append entries [| (Lg_edn_backend.Keyword "content", content) |])
  | _ -> original
