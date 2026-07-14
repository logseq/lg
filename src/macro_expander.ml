open Ast

module Env = Compiler_environment

type value =
  | Form of form
  | Closure of closure
  | Macro_function of Macro_definition.t
  | Builtin of string
  | Volatile of value ref

and closure = {
  params : form list;
  body : form list;
  locals : locals;
  namespace : string;
}

and locals = (string * value) list

type context = {
  compiler_env : Env.t;
  namespace : string;
  locals : locals;
}

let gensym_counter = ref 0

let nil = Form (FSymbol "nil")
let form_of_value = function
  | Form form -> Ok form
  | _ -> Error.error "expected macro form"

let sequence_forms = function
  | Form (FList forms | FVector forms) -> Ok forms
  | Form (FSymbol "nil") -> Ok []
  | Form (FSymbol symbol) ->
      Error.error ("expected sequential macro value, got symbol " ^ symbol)
  | Form (FKeyword keyword) ->
      Error.error ("expected sequential macro value, got keyword " ^ keyword)
  | Form _ -> Error.error "expected sequential macro value, got scalar form"
  | Closure _ -> Error.error "expected sequential macro value, got function"
  | Macro_function _ ->
      Error.error "expected sequential macro value, got function"
  | Builtin _ -> Error.error "expected sequential macro value, got function"
  | Volatile _ -> Error.error "expected sequential macro value, got volatile"

let truthy = function Form (FSymbol "nil" | FBool false) -> false | _ -> true

let host_class_symbol name =
  String.contains name '$'
  ||
  match String.rindex_opt name '.' with
  | Some separator when separator + 1 < String.length name ->
      let initial = name.[separator + 1] in
      initial >= 'A' && initial <= 'Z'
  | _ -> false

let rec split_params fixed = function
  | [] -> Ok (List.rev fixed, None)
  | FSymbol "&" :: [ rest ] -> Ok (List.rev fixed, Some rest)
  | FSymbol "&" :: _ -> Error.error "macro rest parameter must be last"
  | param :: rest -> split_params (param :: fixed) rest

let rec bind_pattern locals pattern value =
  match pattern with
  | FSymbol "_" -> Ok locals
  | FSymbol name -> Ok ((name, value) :: locals)
  | FVector patterns -> (
      match sequence_forms value with
      | Error error ->
          Error
            { error with
              message = error.message ^ " while binding a vector pattern";
            }
      | Ok values -> bind_vector_pattern locals patterns values)
  | _ -> Error.error "unsupported macro binding pattern"

and bind_vector_pattern locals patterns values =
  let rec loop locals patterns values =
    match patterns with
    | [] -> Ok locals
    | FSymbol "&" :: [ rest_pattern ] ->
        let rest_value =
          match values with [] -> nil | values -> Form (FList values)
        in
        bind_pattern locals rest_pattern rest_value
    | FSymbol ":as" :: [ alias ] ->
        bind_pattern locals alias (Form (FVector values))
    | pattern :: patterns ->
        let value, values =
          match values with
          | [] -> (nil, [])
          | value :: values -> (Form value, values)
        in
        (match bind_pattern locals pattern value with
        | Error _ as err -> err
        | Ok locals -> loop locals patterns values)
  in
  loop locals patterns values

let bind_params locals params args =
  match split_params [] params with
  | Error _ as err -> err
  | Ok (fixed, rest_pattern) ->
      let fixed_count = List.length fixed in
      if
        List.length args < fixed_count
        || (Option.is_none rest_pattern && List.length args <> fixed_count)
      then Error.error "macro called with unsupported arity"
      else
        let rec bind_fixed locals patterns values =
          match patterns with
          | [] -> Ok (locals, values)
          | pattern :: patterns -> (
              match values with
              | [] -> assert false
              | value :: values -> (
                  match bind_pattern locals pattern (Form value) with
                  | Error _ as err -> err
                  | Ok locals -> bind_fixed locals patterns values))
        in
        (match bind_fixed locals fixed args with
        | Error _ as err -> err
        | Ok (locals, remaining) -> (
            match rest_pattern with
            | None -> Ok locals
            | Some pattern ->
                let value =
                  match remaining with [] -> nil | forms -> Form (FList forms)
                in
                bind_pattern locals pattern value))

let bind_value_params locals params args =
  match split_params [] params with
  | Error _ as error -> error
  | Ok (fixed, rest_pattern) ->
      let fixed_count = List.length fixed in
      if
        List.length args < fixed_count
        || (Option.is_none rest_pattern && List.length args <> fixed_count)
      then Error.error "macro helper called with unsupported arity"
      else
        let rec bind_fixed locals patterns values =
          match (patterns, values) with
          | [], remaining -> Ok (locals, remaining)
          | pattern :: patterns, value :: values -> (
              match bind_pattern locals pattern value with
              | Error _ as error -> error
              | Ok locals -> bind_fixed locals patterns values)
          | _ -> assert false
        in
        (match bind_fixed locals fixed args with
        | Error _ as error -> error
        | Ok (locals, remaining) -> (
            match rest_pattern with
            | None -> Ok locals
            | Some pattern ->
                let rec collect_forms collected = function
                  | [] -> Ok (List.rev collected)
                  | value :: rest -> (
                      match form_of_value value with
                      | Error _ as error -> error
                      | Ok form -> collect_forms (form :: collected) rest)
                in
                Result.bind (collect_forms [] remaining) (fun forms ->
                    bind_pattern locals pattern
                      (match forms with
                      | [] -> nil
                      | forms -> Form (FList forms)))))

let lookup_local name locals = List.assoc_opt name locals

let rec eval context = function
  | FSymbol "nil" as form -> Ok (Form form)
  | FSymbol name -> (
      match lookup_local name context.locals with
      | Some value -> Ok value
      | None -> (
          match Env.find_macro_value ~scope:context.namespace name context.compiler_env with
          | Some initial_value -> eval context initial_value
          | None -> (
              match
                Env.find_macro_function ~scope:context.namespace name
                  context.compiler_env
              with
              | Some definition -> Ok (Macro_function definition)
              | None when List.mem name [ "conj"; "identity" ] ->
                  Ok (Builtin name)
              | None when host_class_symbol name -> Ok (Form (FSymbol name))
              | None -> Error.error ("unknown macro symbol " ^ name))))
  | (FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _ | FKeyword _)
    as form ->
      Ok (Form form)
  | FVector forms ->
      Result.bind (eval_forms context forms) (fun values ->
             let rec collect acc = function
               | [] -> Ok (Form (FVector (List.rev acc)))
               | value :: rest -> (
                   match form_of_value value with
                   | Error _ as err -> err
                   | Ok form -> collect (form :: acc) rest)
             in
             collect [] values)
  | FMap entries ->
      let rec loop acc = function
        | [] -> Ok (Form (FMap (List.rev acc)))
        | (key, value) :: rest -> (
            match (eval context key, eval context value) with
            | Ok key, Ok value -> (
                match (form_of_value key, form_of_value value) with
                | Ok key, Ok value -> loop ((key, value) :: acc) rest
                | (Error _ as err), _ | _, (Error _ as err) -> err)
            | (Error _ as err), _ | _, (Error _ as err) -> err)
      in
      loop [] entries
  | FList [ FSymbol "quote"; form ] -> Ok (Form form)
  | FList [ FSymbol "syntax-quote"; form ] -> syntax_quote context form
  | FList [ FSymbol "deref"; reference ] -> (
      match eval context reference with
      | Ok (Volatile value) -> Ok !value
      | Ok _ -> Error.error "deref expects a volatile macro value"
      | Error _ as err -> err)
  | FList (FSymbol "if" :: condition :: then_form :: else_forms) -> (
      match eval context condition with
      | Error _ as err -> err
      | Ok condition ->
          if truthy condition then eval context then_form
          else
            eval context
              (match else_forms with form :: _ -> form | [] -> FSymbol "nil"))
  | FList (FSymbol "when" :: condition :: body) -> (
      match eval context condition with
      | Error _ as err -> err
      | Ok condition -> if truthy condition then eval_body context body else Ok nil)
  | FList [ FSymbol "when-some"; FVector [ pattern; expression ]; body ] -> (
      match eval context expression with
      | Error _ as err -> err
      | Ok (Form (FSymbol "nil")) -> Ok nil
      | Ok value -> (
          match bind_pattern context.locals pattern value with
          | Error _ as err -> err
          | Ok locals -> eval { context with locals } body))
  | FList (FSymbol "let" :: FVector bindings :: body) ->
      eval_let context bindings body
  | FList (FSymbol "binding" :: FVector bindings :: body) ->
      eval_let context bindings body
  | FList (FSymbol "fn" :: FVector params :: body) ->
      Ok
        (Closure
           {
             params;
             body;
             locals = context.locals;
             namespace = context.namespace;
           })
  | FList (FSymbol "do" :: body) -> eval_body context body
  | FList (FSymbol ("and" | "clojure.core/and") :: forms) ->
      eval_and context forms
  | FList (FSymbol "or" :: forms) -> eval_or context forms
  | FList (FSymbol ("cond" | "clojure.core/cond") :: clauses) ->
      eval_cond context clauses
  | FList (FSymbol ("condp" | "clojure.core/condp") :: predicate :: target :: clauses) ->
      eval_condp context predicate target clauses
  | FList (FSymbol "case" :: target :: clauses) ->
      eval_case context target clauses
  | FList [ FSymbol "for"; FVector [ pattern; collection ]; body ] ->
      eval_for context pattern collection body
  | FList (FSymbol name :: args) -> eval_call context name args
  | FList [] -> Ok (Form (FList []))
  | FList _ -> Error.error "macro call head must be a symbol"

and eval_forms context forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match eval context form with
        | Error _ as err -> err
        | Ok value -> loop (value :: acc) rest)
  in
  loop [] forms

and eval_body context = function
  | [] -> Ok nil
  | [ form ] -> eval context form
  | form :: rest -> (
      match eval context form with
      | Error _ as err -> err
      | Ok _ -> eval_body context rest)

and eval_let context bindings body =
  let rec bind locals = function
    | [] -> eval_body { context with locals } body
    | pattern :: expression :: rest -> (
        match eval { context with locals } expression with
        | Error _ as err -> err
        | Ok value -> (
            match bind_pattern locals pattern value with
            | Error _ as err -> err
            | Ok locals -> bind locals rest))
    | _ -> Error.error "macro let requires binding pairs"
  in
  bind context.locals bindings

and eval_and context = function
  | [] -> Ok (Form (FBool true))
  | [ form ] -> eval context form
  | form :: rest -> (
      match eval context form with
      | Error _ as err -> err
      | Ok value when truthy value -> eval_and context rest
      | Ok value -> Ok value)

and eval_or context = function
  | [] -> Ok nil
  | form :: rest -> (
      match eval context form with
      | Error _ as err -> err
      | Ok value when truthy value -> Ok value
      | Ok _ -> eval_or context rest)

and eval_cond context = function
  | [] -> Ok nil
  | test :: expression :: rest -> (
      match eval context test with
      | Error _ as err -> err
      | Ok value ->
          if truthy value then eval context expression else eval_cond context rest)
  | _ -> Error.error "macro cond requires test/expression pairs"

and eval_condp context predicate target clauses =
  match (predicate, eval context target) with
  | _, (Error _ as error) -> error
  | FSymbol predicate_name, Ok target ->
      let target_name = "\000lg-condp-target" in
      let context =
        { context with locals = (target_name, target) :: context.locals }
      in
      let rec select = function
        | [] -> Error.error "macro condp requires a default expression"
        | [ default ] -> eval context default
        | test :: expression :: rest -> (
            match
              eval_call context predicate_name [ test; FSymbol target_name ]
            with
            | Error _ as error -> error
            | Ok matched ->
                if truthy matched then eval context expression else select rest)
      in
      select clauses
  | _ -> Error.error "macro condp predicate must be a symbol"

and eval_case context target clauses =
  match eval context target with
  | Error _ as err -> err
  | Ok target ->
      let rec select = function
        | [] -> Ok nil
        | [ default ] -> eval context default
        | candidate :: expression :: rest -> (
            match eval context candidate with
            | Error _ as err -> err
            | Ok candidate ->
                if candidate = target then eval context expression else select rest)
      in
      select clauses

and eval_for context pattern collection body =
  match eval context collection with
  | Error _ as err -> err
  | Ok collection -> (
      match sequence_forms collection with
      | Error _ as err -> err
      | Ok forms ->
          let rec loop acc = function
            | [] -> Ok (Form (FList (List.rev acc)))
            | form :: rest -> (
                match bind_pattern context.locals pattern (Form form) with
                | Error _ as err -> err
                | Ok locals -> (
                    match eval { context with locals } body with
                    | Error _ as err -> err
                    | Ok value -> (
                        match form_of_value value with
                        | Error _ as err -> err
                        | Ok form -> loop (form :: acc) rest)))
          in
          loop [] forms)

and eval_call context name arg_forms =
  match lookup_local name context.locals with
  | Some callable -> (
      match eval_forms context arg_forms with
      | Error _ as err -> err
      | Ok args -> apply_value context callable args)
  | None -> (
      match Env.find_macro_function ~scope:context.namespace name context.compiler_env with
      | Some definition -> invoke_function_definition context definition arg_forms
      | None -> eval_builtin context name arg_forms)

and apply_value context callable args =
  match callable with
  | Closure closure ->
      let arg_forms =
        let rec collect acc = function
          | [] -> Ok (List.rev acc)
          | value :: rest -> (
              match form_of_value value with
              | Error _ as err -> err
              | Ok form -> collect (form :: acc) rest)
        in
        collect [] args
      in
      (match arg_forms with
      | Error _ as err -> err
      | Ok arg_forms -> (
          match bind_params closure.locals closure.params arg_forms with
          | Error _ as err -> err
          | Ok locals ->
              eval_body
                { context with namespace = closure.namespace; locals }
                closure.body))
  | Macro_function definition ->
      let placeholders = List.map (fun _ -> FSymbol "nil") args in
      (match select_arity definition placeholders with
      | Error _ as error -> error
      | Ok arity -> (
          match bind_value_params context.locals arity.params args with
          | Error _ as error -> error
          | Ok locals ->
              eval_body
                { context with
                  namespace = definition.namespace;
                  locals;
                }
                arity.body))
  | Builtin "identity" -> (
      match args with
      | [ value ] -> Ok value
      | _ -> Error.error "identity expects one macro argument")
  | Builtin "conj" -> (
      match args with
      | [ collection; value ] -> (
          match (collection, form_of_value value) with
          | Form (FVector forms), Ok value ->
              Ok (Form (FVector (forms @ [ value ])))
          | Form (FList forms), Ok value ->
              Ok (Form (FList (value :: forms)))
          | _, (Error _ as error) -> error
          | _ -> Error.error "conj expects a macro vector or list")
      | _ -> Error.error "conj expects two macro arguments")
  | Builtin name -> Error.error ("unsupported macro function value " ^ name)
  | _ -> Error.error "macro value is not callable"

and invoke_definition context (definition : Macro_definition.t) arg_forms =
  match select_arity definition arg_forms with
  | Error _ as err -> err
  | Ok arity -> (
      match bind_params [] arity.params arg_forms with
      | Error _ as err -> err
      | Ok locals ->
          eval_body
            { context with namespace = definition.namespace; locals }
            arity.body)

and invoke_function_definition context (definition : Macro_definition.t)
    arg_forms =
  match select_arity definition arg_forms with
  | Error _ as error -> error
  | Ok arity -> (
      match eval_forms context arg_forms with
      | Error _ as error -> error
      | Ok values -> (
          match bind_value_params context.locals arity.params values with
          | Error _ as error -> error
          | Ok locals ->
              eval_body
                { context with namespace = definition.namespace; locals }
                arity.body))

and eval_builtin context name arg_forms =
  let eval_args () = eval_forms context arg_forms in
  let unary fn =
    match eval_args () with
    | Ok [ value ] -> fn value
    | Ok _ -> Error.error (name ^ " expects one macro argument")
    | Error _ as err -> err
  in
  match name with
  | "System/getProperty" -> Ok nil
  | "identity" -> unary (fun value -> Ok value)
  | "string?" ->
      unary (fun value -> Ok (Form (FBool (match value with Form (FString _) -> true | _ -> false))))
  | "seq?" ->
      unary (fun value -> Ok (Form (FBool (match value with Form (FList _) -> true | _ -> false))))
  | "empty?" ->
      unary (fun value ->
          sequence_forms value
          |> Result.map (fun forms -> Form (FBool (forms = []))))
  | "nil?" ->
      unary (fun value -> Ok (Form (FBool (value = nil))))
  | "first" | "second" | "last" | "next" | "nnext" | "butlast" ->
      unary (sequence_operation name)
  | "=" -> (
      match eval_args () with
      | Ok values ->
          let equal =
            match values with
            | [] | [ _ ] -> true
            | first :: rest -> List.for_all (( = ) first) rest
          in
          Ok (Form (FBool equal))
      | Error _ as err -> err)
  | "list" ->
      Result.bind (eval_args ()) (fun values ->
             let rec collect acc = function
               | [] -> Ok (Form (FList (List.rev acc)))
               | value :: rest -> (
                   match form_of_value value with
                   | Error _ as err -> err
                   | Ok form -> collect (form :: acc) rest)
             in
             collect [] values)
  | "cons" -> (
      match eval_args () with
      | Ok [ value; collection ] -> (
          match (form_of_value value, sequence_forms collection) with
          | Ok value, Ok forms -> Ok (Form (FList (value :: forms)))
          | (Error _ as err), _ | _, (Error _ as err) -> err)
      | Ok _ -> Error.error "cons expects two macro arguments"
      | Error _ as err -> err)
  | "conj" -> (
      match eval_args () with
      | Ok [ collection; value ] -> (
          match (collection, form_of_value value) with
          | Form (FVector forms), Ok value -> Ok (Form (FVector (forms @ [ value ])))
          | Form (FList forms), Ok value -> Ok (Form (FList (value :: forms)))
          | _, (Error _ as err) -> err
          | _ -> Error.error "conj expects a macro vector or list")
      | Ok _ -> Error.error "conj expects two macro arguments"
      | Error _ as err -> err)
  | "vec" ->
      unary (fun value ->
          sequence_forms value |> Result.map (fun forms -> Form (FVector forms)))
  | "map" | "mapcat" -> eval_map context name arg_forms
  | "reduce" -> eval_reduce context arg_forms
  | "volatile!" -> unary (fun value -> Ok (Volatile (ref value)))
  | "deref" ->
      unary (function
        | Volatile value -> Ok !value
        | _ -> Error.error "deref expects a volatile macro value")
  | "vswap!" -> eval_vswap context arg_forms
  | "gensym" ->
      incr gensym_counter;
      Ok (Form (FSymbol ("G__" ^ string_of_int !gensym_counter)))
  | _ -> Error.error ("unsupported macro function " ^ name)

and sequence_operation name value =
  match sequence_forms value with
  | Error _ as err -> err
  | Ok forms ->
      let result =
        match (name, forms) with
        | "first", form :: _ -> Form form
        | "second", _ :: form :: _ -> Form form
        | "last", _ -> (
            match List.rev forms with form :: _ -> Form form | [] -> nil)
        | "next", _ :: (_ :: _ as rest) -> Form (FList rest)
        | "nnext", _ :: _ :: (_ :: _ as rest) -> Form (FList rest)
        | "butlast", _ -> (
            match List.rev forms with _ :: rest -> Form (FList (List.rev rest)) | [] -> nil)
        | _ -> nil
      in
      Ok result

and eval_map context name = function
  | [ fn_form; collection_form ] -> (
      match (eval context fn_form, eval context collection_form) with
      | Ok fn, Ok collection -> (
          match sequence_forms collection with
          | Error _ as err -> err
          | Ok forms ->
              let rec loop acc = function
                | [] -> Ok (Form (FList (List.rev acc |> List.concat)))
                | form :: rest -> (
                    match apply_value context fn [ Form form ] with
                    | Error _ as err -> err
                    | Ok value ->
                        let emitted =
                          if name = "mapcat" then sequence_forms value
                          else form_of_value value |> Result.map (fun form -> [ form ])
                        in
                        (match emitted with
                        | Error _ as err -> err
                        | Ok emitted -> loop (emitted :: acc) rest))
              in
              loop [] forms)
      | (Error _ as err), _ | _, (Error _ as err) -> err)
  | _ -> Error.error (name ^ " expects a function and collection")

and eval_reduce context = function
  | [ fn_form; initial_form; collection_form ] -> (
      match
        ( eval context fn_form,
          eval context initial_form,
          eval context collection_form )
      with
      | (Error _ as error), _, _ -> error
      | _, (Error _ as error), _ -> error
      | _, _, (Error _ as error) -> error
      | Ok fn, Ok initial, Ok collection -> (
          match sequence_forms collection with
          | Error _ as error -> error
          | Ok forms ->
              let rec loop result = function
                | [] -> Ok result
                | form :: rest -> (
                    match apply_value context fn [ result; Form form ] with
                    | Error _ as error -> error
                    | Ok result -> loop result rest)
              in
              loop initial forms))
  | _ -> Error.error "reduce expects a function, initial value, and collection"

and eval_vswap context = function
  | reference_form :: fn_form :: extra_forms -> (
      match eval context reference_form with
      | Error _ as err -> err
      | Ok (Volatile reference) -> (
          match eval context fn_form with
          | Error _ as err -> err
          | Ok fn -> (
              match eval_forms context extra_forms with
              | Error _ as err -> err
              | Ok extras -> (
                  match apply_value context fn (!reference :: extras) with
                  | Error _ as err -> err
                  | Ok value ->
                      reference := value;
                      Ok value)))
      | Ok _ -> Error.error "vswap! expects a volatile macro value")
  | _ -> Error.error "vswap! expects a reference and function"

and syntax_quote context form =
  let generated = ref [] in
  let rec quote = function
    | FList [ FSymbol "unquote"; expression ] -> eval context expression
    | FList forms -> quote_sequence (fun forms -> FList forms) forms
    | FVector forms -> quote_sequence (fun forms -> FVector forms) forms
    | FMap entries ->
        let rec loop acc = function
          | [] -> Ok (Form (FMap (List.rev acc)))
          | (key, value) :: rest -> (
              match (quote key, quote value) with
              | Ok key, Ok value -> (
                  match (form_of_value key, form_of_value value) with
                  | Ok key, Ok value -> loop ((key, value) :: acc) rest
                  | (Error _ as err), _ | _, (Error _ as err) -> err)
              | (Error _ as err), _ | _, (Error _ as err) -> err)
        in
        loop [] entries
    | FSymbol name when String.ends_with ~suffix:"#" name ->
        let symbol =
          match List.assoc_opt name !generated with
          | Some symbol -> symbol
          | None ->
              incr gensym_counter;
              let symbol = "G__" ^ string_of_int !gensym_counter in
              generated := (name, symbol) :: !generated;
              symbol
        in
        Ok (Form (FSymbol symbol))
    | FSymbol name ->
        let name =
          if String.contains name '/' then name
          else
            match Env.find_macro ~scope:context.namespace name context.compiler_env with
            | Some _ -> context.namespace ^ "/" ^ name
            | None -> name
        in
        Ok (Form (FSymbol name))
    | form -> Ok (Form form)
  and quote_sequence construct forms =
    let rec loop acc = function
      | [] -> Ok (Form (construct (List.rev acc |> List.concat)))
      | FList [ FSymbol "unquote-splicing"; expression ] :: rest -> (
          match eval context expression with
          | Error _ as err -> err
          | Ok value -> (
              match sequence_forms value with
              | Error _ as err -> err
              | Ok forms -> loop (forms :: acc) rest))
      | form :: rest -> (
          match quote form with
          | Error _ as err -> err
          | Ok value -> (
              match form_of_value value with
              | Error _ as err -> err
              | Ok form -> loop ([ form ] :: acc) rest))
    in
    loop [] forms
  in
  quote form

and select_arity (definition : Macro_definition.t) args :
    (Macro_definition.arity, Error.t) result =
  let matches (arity : Macro_definition.arity) =
    match split_params [] arity.params with
    | Error _ -> false
    | Ok (fixed, None) -> List.length fixed = List.length args
    | Ok (fixed, Some _) -> List.length args >= List.length fixed
  in
  match List.find_opt matches definition.arities with
  | Some (arity : Macro_definition.arity) -> Ok arity
  | None ->
      Error.error
        (definition.name ^ " called with unsupported macro arity "
       ^ string_of_int (List.length args))

let expand ~compiler_env (definition : Macro_definition.t) args =
  let context = { compiler_env; namespace = definition.namespace; locals = [] } in
  Result.bind (invoke_definition context definition args) form_of_value
