type t = string

let compare_identifier left right =
  let parts value =
    let start =
      if String.length value > 0 && value.[0] = ':' then 1 else 0
    in
    match
      if String.length value = 0 then None
      else String.rindex_from_opt value (String.length value - 1) '/'
    with
    | Some separator when separator >= start ->
        (Some (start, separator), (separator + 1, String.length value))
    | Some _ | None -> (None, (start, String.length value))
  in
  let compare_range left (left_start, left_end) right (right_start, right_end) =
    let rec loop left_index right_index =
      if left_index = left_end then Int.compare right_index right_end
      else if right_index = right_end then 1
      else
        let result = Char.compare left.[left_index] right.[right_index] in
        if result = 0 then loop (left_index + 1) (right_index + 1)
        else result
    in
    loop left_start right_start
  in
  let left_namespace, left_name = parts left in
  let right_namespace, right_name = parts right in
  let namespace_result =
    match (left_namespace, right_namespace) with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some left_range, Some right_range ->
        compare_range left left_range right right_range
  in
  if namespace_result = 0 then compare_range left left_name right right_name
  else namespace_result
