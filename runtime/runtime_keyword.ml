type t = string

let of_string value = value

let identifier_start value length =
  if length > 0 && String.unsafe_get value 0 = ':' then 1 else 0

let rec last_separator value index start =
  if index < start then -1
  else if String.unsafe_get value index = '/' then index
  else last_separator value (index - 1) start

let compare_range left left_start left_end right right_start right_end =
  let rec loop left_index right_index =
    if left_index = left_end then right_index - right_end
    else if right_index = right_end then 1
    else
      let result =
        Char.code (String.unsafe_get left left_index)
        - Char.code (String.unsafe_get right right_index)
      in
      if result = 0 then loop (left_index + 1) (right_index + 1)
      else result
  in
  loop left_start right_start

let compare_identifier left right =
  if String.equal left right then 0
  else
    let left_length = String.length left in
    let right_length = String.length right in
    let left_start = identifier_start left left_length in
    let right_start = identifier_start right right_length in
    let left_separator =
      last_separator left (left_length - 1) left_start
    in
    let right_separator =
      last_separator right (right_length - 1) right_start
    in
    let left_has_namespace = left_separator >= left_start in
    let right_has_namespace = right_separator >= right_start in
    let namespace_result =
      match (left_has_namespace, right_has_namespace) with
      | false, false -> 0
      | false, true -> -1
      | true, false -> 1
      | true, true ->
          compare_range left left_start left_separator right right_start
            right_separator
    in
    if namespace_result <> 0 then namespace_result
    else
      let left_name_start =
        if left_has_namespace then left_separator + 1 else left_start
      in
      let right_name_start =
        if right_has_namespace then right_separator + 1 else right_start
      in
      compare_range left left_name_start left_length right right_name_start
        right_length
