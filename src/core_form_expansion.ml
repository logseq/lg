open Ast

let get_in target keys default =
  let rec expand target = function
    | [] -> target
    | key :: rest ->
        let get =
          match (rest, default) with
          | [], Some default ->
              FList [ FCoreSymbol Core_get; target; key; default ]
          | _ -> FList [ FCoreSymbol Core_get; target; key ]
        in
        expand get rest
  in
  expand target keys

let assoc_in target keys value =
  let rec expand depth target keys =
    let target_name = "__lg_assoc_in_target_" ^ string_of_int depth in
    let key_name = "__lg_assoc_in_key_" ^ string_of_int depth in
    let key, rest =
      match keys with
      | [] -> (FSymbol "nil", [])
      | key :: rest -> (key, rest)
    in
    let nested_value =
      match rest with
      | [] -> value
      | _ ->
          expand (depth + 1)
            (FList [ FCoreSymbol Core_get; FSymbol target_name; FSymbol key_name ])
            rest
    in
    FList
      [
        FSymbol "let";
        FVector
          [ FSymbol target_name; target; FSymbol key_name; key ];
        FList
          [
            FCoreSymbol Core_assoc;
            FSymbol target_name;
            FSymbol key_name;
            nested_value;
          ];
      ]
  in
  expand 0 target keys

let update_in target keys function_form argument_forms =
  let rec update_arguments = function
    | [] -> function_form :: argument_forms
    | [ key ] -> key :: function_form :: argument_forms
    | key :: rest ->
        key :: FCoreSymbol Core_update :: update_arguments rest
  in
  FList
    (FCoreSymbol Core_update :: target :: update_arguments keys)

let rec apply_transducer collection = function
  | FList [ FSymbol "map"; function_form ] ->
      Ok (FList [ FCoreSymbol Core_map; function_form; collection ])
  | FList [ FSymbol "filter"; predicate_form ] ->
      Ok (FList [ FCoreSymbol Core_filter; predicate_form; collection ])
  | FSymbol "cat" ->
      Ok
        (FList
           [
             FCoreSymbol Core_mapcat;
             FList
               [ FSymbol "fn"; FVector [ FSymbol "value" ]; FSymbol "value" ];
             collection;
           ])
  | FList (FSymbol "comp" :: transducers) ->
      List.fold_left
        (fun result transducer ->
          Result.bind result (fun collection ->
              apply_transducer collection transducer))
        (Ok collection) transducers
  | _ -> Error.error "transducers support map, filter, and cat"
