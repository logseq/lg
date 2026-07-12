let compile_expr = Expression_elaborator.compile_expr
let compile_top_level = Top_level_elaborator.compile

type state = Compiler_state.t

let empty_state = Compiler_state.empty

let compile_forms_incremental (state : Compiler_state.t) forms =
  let rec loop env next_type items = function
    | [] -> Ok (env, next_type, List.rev items)
    | form :: rest -> (
        match compile_top_level "" env next_type form with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) ->
            loop env next_type (item :: items) rest)
  in
  match loop state.env state.next_type [] forms with
  | Error _ as err -> err
  | Ok (env, next_type, new_items) ->
      let next_state =
        {
          Compiler_state.env;
          next_type;
          items = state.items @ new_items;
        }
      in
      Ok (next_state, new_items)

let compile_forms forms =
  match compile_forms_incremental empty_state forms with
  | Error _ as err -> err
  | Ok (_state, items) -> Ok items
