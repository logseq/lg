open Semantic_type

let valid_tag name =
  let letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  String.length name > 0
  && letter name.[0]
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       name

let required row =
  match row.bound with
  | Upper_row -> []
  | Bounded_row tags -> tags
  | Exact_row | Lower_row -> List.map fst row.tags

let admits row tag = row.bound = Lower_row || List.mem_assoc tag row.tags

let compatible left right =
  List.for_all (admits right) (required left)
  && List.for_all (admits left) (required right)

let merge merge_payload left right =
  let rec add tags = function
    | [] -> Some { tags = List.sort compare tags; bound = Lower_row }
    | (tag, payload) :: rest -> (
        match List.assoc_opt tag tags with
        | None -> add ((tag, payload) :: tags) rest
        | Some previous ->
            let merged =
              match (previous, payload) with
              | None, None -> Some None
              | Some left, Some right ->
                  Option.map Option.some (merge_payload left right)
              | _ -> None
            in
            Option.bind merged (fun payload ->
                add ((tag, payload) :: List.remove_assoc tag tags) rest))
  in
  add left.tags right.tags

let compatible_payloads compatible_payload left right =
  compatible left right
  && List.for_all
       (fun (tag, payload) ->
         match (List.assoc_opt tag right.tags, payload) with
         | None, _ | Some None, None -> true
         | Some (Some right), Some left -> compatible_payload left right
         | _ -> false)
       left.tags
