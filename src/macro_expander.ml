open Ast
module Env = Compiler_environment

type value =
  | Form of form
  | Closure of closure
  | Macro_function of Macro_definition.t
  | Builtin of string
  | Juxt of value list
  | Volatile of value ref
  | Recur of value list

and closure = {
  name : string option;
  params : form list;
  body : form list;
  locals : locals;
  namespace : string;
}

and locals = (string * value) list

type context = { compiler_env : Env.t; namespace : string; locals : locals }

let gensym_counter = ref 0

let is_unqualified_compile_time_primitive = function
  | "assert" | "str" | "subs" | "namespace" | "identity" | "num"
  | "boolean" | "string?" | "char?" | "regex?" | "regex-source"
  | "float?" | "symbol?" | "keyword?" | "vector?" | "map?" | "seq?"
  | "sequential?" | "empty?" | "not-empty" | "reverse" | "concat"
  | "clojure.core/concat" | "count" | "take" | "drop" | "/" | "nil?"
  | "even?" | "partition" | "first" | "second" | "last" | "next"
  | "nnext" | "butlast" | "=" | "list" | "cons" | "conj" | "assoc"
  | "meta" | "with-meta" | "vary-meta" | "vec" | "map" | "mapcat"
  | "filter"
  | "into" | "juxt" | "reduce" | "apply" | "volatile!" | "deref"
  | "gensym" | "clojure.test/expand-are" ->
      true
  | _ -> false

let compile_time_primitive_name name =
  let unqualified =
    if String.starts_with ~prefix:"clojure.core/" name then
      String.sub name 13 (String.length name - 13)
    else if String.starts_with ~prefix:"cljs.core/" name then
      String.sub name 10 (String.length name - 10)
    else name
  in
  if is_unqualified_compile_time_primitive unqualified then Some unqualified
  else None

let is_compile_time_primitive name =
  Option.is_some (compile_time_primitive_name name)

let nil = Form (FSymbol "nil")

let rec string_of_form = function
  | FSymbol "nil" -> "nil"
  | FSymbol name -> name
  | FCoreSymbol symbol -> Ast.core_symbol_name symbol
  | FKeyword keyword -> keyword
  | FString value -> Printf.sprintf "%S" value
  | FRegex value -> "#" ^ Printf.sprintf "%S" value
  | FInt value -> string_of_int value
  | FFloat value -> value
  | FChar value -> "\\" ^ String.make 1 value
  | FBool value -> string_of_bool value
  | FList forms ->
      "(" ^ String.concat " " (List.map string_of_form forms) ^ ")"
  | FVector forms ->
      "[" ^ String.concat " " (List.map string_of_form forms) ^ "]"
  | FMap entries ->
      let entries =
        List.map
          (fun (key, value) ->
            string_of_form key ^ " " ^ string_of_form value)
          entries
      in
      "{" ^ String.concat ", " entries ^ "}"

let string_of_value = function
  | Form (FSymbol "nil") -> Ok ""
  | Form (FString value) -> Ok value
  | Form (FChar value) -> Ok (String.make 1 value)
  | Form form -> Ok (string_of_form form)
  | Closure _ | Macro_function _ | Builtin _ | Juxt _ | Volatile _ | Recur _ ->
      Error.error "str expects macro form values"

let form_of_value = function
  | Form form -> Ok form
  | Closure _ | Macro_function _ | Builtin _ | Juxt _ | Volatile _ | Recur _ ->
      Error.error "expected macro form"

let rec sequence_forms = function
  | Form (FList [ FSymbol "__type-hint"; _; form ]) ->
      sequence_forms (Form form)
  | Form (FVector forms) ->
      let rec attach_metadata attached = function
        | FSymbol metadata :: form :: rest
          when String.starts_with ~prefix:"^" metadata ->
            attach_metadata
              (FList [ FSymbol "__type-hint"; FSymbol metadata; form ]
              :: attached)
              rest
        | form :: rest -> attach_metadata (form :: attached) rest
        | [] -> List.rev attached
      in
      Ok (attach_metadata [] forms)
  | Form (FList forms) -> Ok forms
  | Form (FMap entries) ->
      Ok (List.map (fun (key, value) -> FVector [ key; value ]) entries)
  | Form (FSymbol "nil") -> Ok []
  | Form (FSymbol symbol) ->
      Error.error ("expected sequential macro value, got symbol " ^ symbol)
  | Form (FKeyword keyword) ->
      Error.error ("expected sequential macro value, got keyword " ^ keyword)
  | Form _ -> Error.error "expected sequential macro value, got scalar form"
  | Closure _ -> Error.error "expected sequential macro value, got function"
  | Macro_function _ ->
      Error.error "expected sequential macro value, got function"
  | Builtin _ | Juxt _ ->
      Error.error "expected sequential macro value, got function"
  | Recur _ -> Error.error "recur is only valid in macro loop tail position"
  | Volatile _ -> Error.error "expected sequential macro value, got volatile"

let concat_sequence_values values =
  let rec concat reversed = function
    | [] -> Ok (Form (FList (List.rev reversed)))
    | value :: rest ->
        Result.bind (sequence_forms value) (fun forms ->
            concat (List.rev_append forms reversed) rest)
  in
  concat [] values

let truthy = function
  | Form (FSymbol "nil" | FBool false) -> false
  | Form _ | Closure _ | Macro_function _ | Builtin _ | Juxt _ | Volatile _
  | Recur _ ->
      true

let strip_internal_metadata = function
  | FList [ FSymbol "__type-hint"; _; form ] -> form
  | form -> form

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
            {
              error with
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
    | pattern :: patterns -> (
        let value, values =
          match values with
          | [] -> (nil, [])
          | value :: values -> (Form value, values)
        in
        match bind_pattern locals pattern value with
        | Error _ as err -> err
        | Ok locals -> loop locals patterns values)
  in
  loop locals patterns values

let bind_params locals params args =
  match split_params [] params with
  | Error _ as err -> err
  | Ok (fixed, rest_pattern) -> (
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
        match bind_fixed locals fixed args with
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
  | Ok (fixed, rest_pattern) -> (
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
        match bind_fixed locals fixed args with
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
  | FCoreSymbol _ as form -> Ok (Form form)
  | FSymbol name -> (
      match lookup_local name context.locals with
      | Some value -> Ok value
      | None -> (
          match
            Env.find_macro_value ~scope:context.namespace name
              context.compiler_env
          with
          | Some initial_value -> eval context initial_value
          | None -> (
              match
                Env.find_macro_function ~scope:context.namespace name
                  context.compiler_env
              with
              | Some definition -> Ok (Macro_function definition)
              | None
                when List.mem name
                       [
                         "assoc";
                         "conj";
                         "concat";
                         "clojure.core/concat";
                         "identity";
                         "list";
                         "first";
                         "second";
                         "last";
                         "next";
                         "nnext";
                         "butlast";
                       ] ->
                  Ok (Builtin name)
              | None
                when String.starts_with ~prefix:"java." name
                     || String.starts_with ~prefix:"javax." name
                     || String.starts_with ~prefix:"clojure.lang." name ->
                  Error.error
                    "Java interop is not supported; use static LG types and functions"
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
  | FList
      [
        FSymbol ("__lg_if-let" | "__lg_if-some" as binding_name);
        FVector [ pattern; expression ];
        then_form;
        else_form;
      ] -> (
      match eval context expression with
      | Error _ as err -> err
      | Ok value ->
          let present =
            match binding_name with
            | "__lg_if-let" -> truthy value
            | "__lg_if-some" -> (
                match value with Form (FSymbol "nil") -> false | _ -> true)
            | _ -> assert false
          in
          if not present then eval context else_form
          else (
            match bind_pattern context.locals pattern value with
            | Error _ as err -> err
            | Ok locals -> eval { context with locals } then_form))
  | FList
      (FSymbol ("__lg_when-let" | "__lg_when-some" as binding_name)
      :: FVector [ pattern; expression ] :: body) -> (
      match eval context expression with
      | Error _ as err -> err
      | Ok value -> (
          let present =
            match binding_name with
            | "__lg_when-let" -> truthy value
            | "__lg_when-some" -> (
                match value with Form (FSymbol "nil") -> false | _ -> true)
            | _ -> assert false
          in
          if not present then Ok nil
          else
            match bind_pattern context.locals pattern value with
            | Error _ as err -> err
            | Ok locals -> eval_body { context with locals } body))
  | FList (FSymbol "let" :: FVector bindings :: body) ->
      eval_let context bindings body
  | FList (FSymbol "binding" :: FVector bindings :: body) ->
      eval_let context bindings body
  | FList (FSymbol "loop" :: FVector bindings :: body) ->
      eval_loop context bindings body
  | FList (FSymbol "recur" :: arguments) ->
      Result.map (fun values -> Recur values) (eval_forms context arguments)
  | FList (FSymbol "fn" :: FSymbol name :: FVector params :: body) ->
      Ok
        (Closure
           {
             name = Some name;
             params;
             body;
             locals = context.locals;
             namespace = context.namespace;
           })
  | FList (FSymbol "fn" :: FVector params :: body) ->
      Ok
        (Closure
           {
             name = None;
             params;
             body;
             locals = context.locals;
             namespace = context.namespace;
           })
  | FList (FSymbol "do" :: body) -> eval_body context body
  | FList [ FKeyword keyword; target ] -> (
      match eval context target with
      | Error _ as error -> error
      | Ok (Form (FMap entries)) -> (
          match List.assoc_opt (FKeyword keyword) entries with
          | Some value -> Ok (Form value)
          | None -> Ok nil)
      | Ok _ -> Ok nil)
  | FList (FSymbol "__lg_logical-and" :: forms) ->
      eval_and context forms
  | FList (FSymbol "__lg_logical-or" :: forms) -> eval_or context forms
  | FList
      (FSymbol ("condp" | "clojure.core/condp")
      :: predicate :: target :: clauses) ->
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

and eval_loop context bindings body =
  let rec evaluate_bindings locals patterns values = function
    | [] -> Ok (List.rev patterns, List.rev values)
    | pattern :: expression :: rest -> (
        match eval { context with locals } expression with
        | Error _ as error -> error
        | Ok value -> (
            match bind_pattern locals pattern value with
            | Error _ as error -> error
            | Ok locals ->
                evaluate_bindings locals (pattern :: patterns) (value :: values)
                  rest))
    | _ -> Error.error "macro loop requires binding pairs"
  in
  let rec bind_values locals patterns values =
    match (patterns, values) with
    | [], [] -> Ok locals
    | pattern :: patterns, value :: values -> (
        match bind_pattern locals pattern value with
        | Error _ as error -> error
        | Ok locals -> bind_values locals patterns values)
    | _ -> Error.error "macro recur argument count mismatch"
  in
  Result.bind (evaluate_bindings context.locals [] [] bindings)
    (fun (patterns, initial_values) ->
      let rec iterate values =
        Result.bind (bind_values context.locals patterns values) (fun locals ->
            match eval_body { context with locals } body with
            | Ok (Recur values) -> iterate values
            | result -> result)
      in
      iterate initial_values)

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
          if truthy value then eval context expression
          else eval_cond context rest)
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
                if candidate = target then eval context expression
                else select rest)
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
      match compile_time_primitive_name name with
      | Some primitive -> eval_builtin context primitive arg_forms
      | None -> (
          match
            Env.find_macro ~scope:context.namespace name context.compiler_env
          with
          | Some definition -> (
              match invoke_definition context definition arg_forms with
              | Error _ as error -> error
              | Ok (Form expanded) -> eval context expanded
              | Ok _ -> Error.error "macro expansion must return a form")
          | None -> (
              match
                Env.find_macro_function ~scope:context.namespace name
                  context.compiler_env
              with
              | Some definition ->
                  invoke_function_definition context definition arg_forms
              | None -> eval_builtin context name arg_forms)))

and apply_value context callable args =
  match callable with
  | Closure closure -> (
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
      match arg_forms with
      | Error _ as err -> err
      | Ok arg_forms -> (
          let closure_locals =
            match closure.name with
            | None -> closure.locals
            | Some name -> (name, callable) :: closure.locals
          in
          match bind_params closure_locals closure.params arg_forms with
          | Error _ as err -> err
          | Ok locals ->
              eval_body
                { context with namespace = closure.namespace; locals }
                closure.body))
  | Macro_function definition -> (
      let placeholders = List.map (fun _ -> FSymbol "nil") args in
      match select_arity definition placeholders with
      | Error _ as error -> error
      | Ok arity -> (
          match bind_value_params context.locals arity.params args with
          | Error _ as error -> error
          | Ok locals ->
              eval_body
                { context with namespace = definition.namespace; locals }
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
          | Form (FList forms), Ok value -> Ok (Form (FList (value :: forms)))
          | _, (Error _ as error) -> error
          | _ -> Error.error "conj expects a macro vector or list")
      | _ -> Error.error "conj expects two macro arguments")
  | Builtin "assoc" -> (
      match args with
      | [ Form (FMap entries); Form key; Form value ] ->
          let entries = (key, value) :: List.remove_assoc key entries in
          Ok (Form (FMap entries))
      | _ -> Error.error "assoc expects a macro map, key, and value")
  | Builtin "list" ->
      let rec collect forms = function
        | [] -> Ok (Form (FList (List.rev forms)))
        | value :: rest -> (
            match form_of_value value with
            | Error _ as error -> error
            | Ok form -> collect (form :: forms) rest)
      in
      collect [] args
  | Builtin ("concat" | "clojure.core/concat") ->
      concat_sequence_values args
  | Builtin
      (("first" | "second" | "last" | "next" | "nnext" | "butlast") as name)
    -> (
      match args with
      | [ value ] -> sequence_operation name value
      | _ -> Error.error (name ^ " expects one macro argument"))
  | Juxt functions ->
      let rec invoke results = function
        | [] -> Ok (Form (FVector (List.rev results)))
        | function_ :: rest -> (
            match apply_value context function_ args with
            | Error _ as error -> error
            | Ok value -> (
                match form_of_value value with
                | Error _ as error -> error
                | Ok form -> invoke (form :: results) rest))
      in
      invoke [] functions
  | Builtin name -> Error.error ("unsupported macro function value " ^ name)
  | Recur _ -> Error.error "recur value is not callable"
  | _ -> Error.error "macro value is not callable"

and invoke_definition context (definition : Macro_definition.t) arg_forms =
  match select_arity definition arg_forms with
  | Error _ as err -> err
  | Ok arity -> (
      match bind_params context.locals arity.params arg_forms with
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

and expand_are assertion_symbol parameters expression arguments =
  let rec substitute bindings = function
    | FSymbol name as form ->
        Option.value (List.assoc_opt name bindings) ~default:form
    | FList forms -> FList (List.map (substitute bindings) forms)
    | FVector forms -> FVector (List.map (substitute bindings) forms)
    | FMap entries ->
        FMap
          (List.map
             (fun (key, value) ->
               (substitute bindings key, substitute bindings value))
             entries)
    | form -> form
  in
  let rec take count taken remaining =
    if count = 0 then Ok (List.rev taken, remaining)
    else
      match remaining with
      | [] -> Error.error "The number of args doesn't match are's argv."
      | value :: rest -> take (count - 1) (value :: taken) rest
  in
  match parameters with
  | [] when arguments = [] ->
      Ok (Form (FList [ FList [ FSymbol assertion_symbol; expression ] ]))
  | [] -> Error.error "The number of args doesn't match are's argv."
  | parameters ->
      let rec expanded_assertions accumulated = function
        | [] -> Ok (Form (FList (List.rev accumulated)))
        | arguments ->
            Result.bind (take (List.length parameters) [] arguments)
              (fun (values, rest) ->
                let bindings = List.combine parameters values in
                let assertion =
                  FList
                    [
                      FSymbol assertion_symbol;
                      substitute bindings expression;
                    ]
                in
                expanded_assertions (assertion :: accumulated) rest)
      in
      expanded_assertions [] arguments

and parse_are_parameters accumulated = function
  | [] -> Ok (List.rev accumulated)
  | FSymbol name :: rest -> parse_are_parameters (name :: accumulated) rest
  | _ -> Error.error "are expects a vector of symbols"

and eval_builtin context name arg_forms =
  let eval_args () = eval_forms context arg_forms in
  let unary fn =
    match eval_args () with
    | Ok [ value ] -> fn value
    | Ok _ -> Error.error (name ^ " expects one macro argument")
    | Error _ as err -> err
  in
  match name with
  | "assert" -> (
      let fail = function
        | None -> Error.error "Assert failed"
        | Some message ->
            Result.bind (eval context message) (fun message ->
                Result.bind (string_of_value message) Error.error)
      in
      match arg_forms with
      | [ condition ] ->
          Result.bind (eval context condition) (fun condition ->
              if truthy condition then Ok nil else fail None)
      | [ condition; message ] ->
          Result.bind (eval context condition) (fun condition ->
              if truthy condition then Ok nil else fail (Some message))
      | _ -> Error.error "assert expects one or two macro arguments")
  | "str" ->
      Result.bind (eval_args ()) (fun values ->
          let rec concatenate buffer = function
            | [] -> Ok (Form (FString (Buffer.contents buffer)))
            | value :: rest ->
                Result.bind (string_of_value value) (fun value ->
                    Buffer.add_string buffer value;
                    concatenate buffer rest)
          in
          concatenate (Buffer.create 32) values)
  | "subs" -> (
      match eval_args () with
      | Ok [ Form (FString value); Form (FInt start); Form (FInt finish) ]
        when start >= 0 && finish >= start && finish <= String.length value ->
          Ok (Form (FString (String.sub value start (finish - start))))
      | Ok _ ->
          Error.error
            "subs expects a string and valid start/end integer indexes"
      | Error _ as error -> error)
  | "namespace" ->
      unary (function
        | Form (FSymbol value) -> (
            match String.index_opt value '/' with
            | Some index -> Ok (Form (FString (String.sub value 0 index)))
            | None -> Ok nil)
        | _ -> Error.error "namespace expects a macro symbol")
  | "identity" | "num" -> unary (fun value -> Ok value)
  | "boolean" -> unary (fun value -> Ok (Form (FBool (truthy value))))
  | "string?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FString _) -> true | _ -> false))))
  | "char?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FChar _) -> true | _ -> false))))
  | "regex?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FRegex _) -> true | _ -> false))))
  | "regex-source" ->
      unary (function
        | Form (FRegex value) -> Ok (Form (FString value))
        | _ -> Error.error "regex-source expects a macro regex")
  | "float?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FFloat _) -> true | _ -> false))))
  | "symbol?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FSymbol _) -> true | _ -> false))))
  | "keyword?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool
                  (match value with Form (FKeyword _) -> true | _ -> false))))
  | "vector?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FVector _) -> true | _ -> false))))
  | "map?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FMap _) -> true | _ -> false))))
  | "seq?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool (match value with Form (FList _) -> true | _ -> false))))
  | "sequential?" ->
      unary (fun value ->
          Ok
            (Form
               (FBool
                  (match value with
                  | Form (FList _ | FVector _) -> true
                  | _ -> false))))
  | "empty?" ->
      unary (fun value ->
          sequence_forms value
          |> Result.map (fun forms -> Form (FBool (forms = []))))
  | "not-empty" ->
      unary (fun value ->
          sequence_forms value |> Result.map (function [] -> nil | _ -> value))
  | "reverse" ->
      unary (fun value ->
          sequence_forms value
          |> Result.map (fun forms -> Form (FList (List.rev forms))))
  | "concat" | "clojure.core/concat" ->
      Result.bind (eval_args ()) concat_sequence_values
  | "count" ->
      unary (fun value ->
          sequence_forms value
          |> Result.map (fun forms -> Form (FInt (List.length forms))))
  | "even?" ->
      unary (function
        | Form (FInt value) -> Ok (Form (FBool (value mod 2 = 0)))
        | _ -> Error.error "even? expects an integer macro argument")
  | "partition" -> (
      match eval_args () with
      | Ok [ Form (FInt size); collection ] when size > 0 ->
          Result.map
            (fun forms ->
              let rec take count taken remaining =
                if count = 0 then Some (List.rev taken, remaining)
                else
                  match remaining with
                  | [] -> None
                  | form :: rest -> take (count - 1) (form :: taken) rest
              in
              let rec groups grouped remaining =
                match take size [] remaining with
                | Some (group, rest) -> groups (FList group :: grouped) rest
                | None -> Form (FList (List.rev grouped))
              in
              groups [] forms)
            (sequence_forms collection)
      | Ok _ ->
          Error.error
            "partition expects a positive integer and macro collection"
      | Error _ as error -> error)
  | "take" | "drop" -> (
      match eval_args () with
      | Ok [ Form (FInt count); collection ] ->
          Result.map
            (fun forms ->
              let rec take count taken forms =
                if count <= 0 then List.rev taken
                else
                  match forms with
                  | [] -> List.rev taken
                  | form :: rest -> take (count - 1) (form :: taken) rest
              in
              let rec drop count forms =
                if count <= 0 then forms
                else
                  match forms with
                  | [] -> []
                  | _ :: rest -> drop (count - 1) rest
              in
              Form
                (FList
                   (if name = "take" then take count [] forms
                    else drop count forms)))
            (sequence_forms collection)
      | Ok _ -> Error.error (name ^ " expects an int and collection")
      | Error _ as error -> error)
  | "/" -> (
      match eval_args () with
      | Ok [ Form (FInt left); Form (FInt right) ] when right <> 0 ->
          Ok (Form (FInt (left / right)))
      | Ok _ -> Error.error "/ expects two integer macro arguments"
      | Error _ as error -> error)
  | "nil?" -> unary (fun value -> Ok (Form (FBool (value = nil))))
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
          | Form (FVector forms), Ok value ->
              Ok (Form (FVector (forms @ [ value ])))
          | Form (FList forms), Ok value -> Ok (Form (FList (value :: forms)))
          | _, (Error _ as err) -> err
          | _ -> Error.error "conj expects a macro vector or list")
      | Ok _ -> Error.error "conj expects two macro arguments"
      | Error _ as err -> err)
  | "assoc" -> (
      match eval_args () with
      | Ok [ Form (FMap entries); Form key; Form value ] ->
          Ok (Form (FMap ((key, value) :: List.remove_assoc key entries)))
      | Ok _ -> Error.error "assoc expects a macro map, key, and value"
      | Error _ as error -> error)
  | "meta" ->
      unary (function
        | Form
            (FList
              [ FSymbol "__type-hint"; (FSymbol _ as annotation); _value ]) ->
            Ok (Form (FMap [ (FKeyword ":tag", annotation) ]))
        | Form _ -> Ok (Form (FMap []))
        | _ -> Error.error "meta expects a macro form")
  | "with-meta" -> (
      match eval_args () with
      | Ok [ Form form; Form (FMap entries) ] ->
          let form = strip_internal_metadata form in
          let form =
            match List.assoc_opt (FKeyword ":tag") entries with
            | Some (FSymbol _ as annotation) ->
                FList [ FSymbol "__type-hint"; annotation; form ]
            | Some _ | None -> form
          in
          Ok (Form form)
      | Ok _ -> Error.error "with-meta expects a form and metadata map"
      | Error _ as error -> error)
  | "vary-meta" -> eval_vary_meta context arg_forms
  | "vec" ->
      unary (fun value ->
          sequence_forms value |> Result.map (fun forms -> Form (FVector forms)))
  | "map" | "mapcat" -> eval_map context name arg_forms
  | "filter" -> eval_filter context arg_forms
  | "into" -> eval_into context arg_forms
  | "juxt" -> Result.map (fun functions -> Juxt functions) (eval_args ())
  | "reduce" -> eval_reduce context arg_forms
  | "apply" -> eval_apply context arg_forms
  | "volatile!" -> unary (fun value -> Ok (Volatile (ref value)))
  | "deref" ->
      unary (function
        | Volatile value -> Ok !value
        | _ -> Error.error "deref expects a volatile macro value")
  | "gensym" ->
      incr gensym_counter;
      Ok (Form (FSymbol ("G__" ^ string_of_int !gensym_counter)))
  | "clojure.test/expand-are" -> (
      match eval_args () with
      | Ok
          [
            Form (FVector parameters);
            Form expression;
            Form (FList arguments);
          ] ->
          Result.bind (parse_are_parameters [] parameters) (fun parameters ->
              expand_are "clojure.test/is" parameters expression arguments)
      | Ok
          [
            Form (FSymbol assertion_symbol);
            Form (FVector parameters);
            Form expression;
            Form (FList arguments);
          ] ->
          Result.bind (parse_are_parameters [] parameters) (fun parameters ->
              expand_are assertion_symbol parameters expression arguments)
      | Ok _ ->
          Error.error
            "clojure.test/expand-are expects an optional assertion symbol, parameters, expression, and arguments"
      | Error _ as error -> error)
  | _ -> Error.error ("unsupported macro function " ^ name)

and eval_apply context = function
  | callable_form :: argument_forms when argument_forms <> [] -> (
      let fixed_forms, sequence_form =
        match List.rev argument_forms with
        | sequence_form :: reversed_fixed ->
            (List.rev reversed_fixed, sequence_form)
        | [] -> assert false
      in
      match
        ( eval context callable_form,
          eval_forms context fixed_forms,
          eval context sequence_form )
      with
      | (Error _ as error), _, _
      | _, (Error _ as error), _
      | _, _, (Error _ as error) ->
          error
      | Ok callable, Ok fixed, Ok sequence ->
          Result.bind (sequence_forms sequence) (fun forms ->
              apply_value context callable
                (fixed @ List.map (fun form -> Form form) forms)))
  | _ -> Error.error "apply expects a function and an argument sequence"

and eval_filter context = function
  | [ predicate_form; collection_form ] -> (
      match (eval context predicate_form, eval context collection_form) with
      | (Error _ as error), _ | _, (Error _ as error) -> error
      | Ok predicate, Ok collection ->
          Result.bind (sequence_forms collection) (fun forms ->
              let rec filter selected = function
                | [] -> Ok (Form (FList (List.rev selected)))
                | form :: rest -> (
                    match apply_value context predicate [ Form form ] with
                    | Error _ as error -> error
                    | Ok keep ->
                        filter
                          (if truthy keep then form :: selected else selected)
                          rest)
              in
              filter [] forms))
  | _ -> Error.error "filter expects a predicate and collection"

and eval_into context = function
  | [ target_form; source_form ] -> (
      match (eval context target_form, eval context source_form) with
      | (Error _ as error), _ | _, (Error _ as error) -> error
      | Ok target, Ok source ->
          Result.bind (sequence_forms source) (fun forms ->
              match target with
              | Form (FVector values) -> Ok (Form (FVector (values @ forms)))
              | Form (FList values) ->
                  Ok (Form (FList (List.rev_append forms values)))
              | Form (FMap entries) ->
                  let rec add entries = function
                    | [] -> Ok (Form (FMap entries))
                    | FVector [ key; value ] :: rest
                    | FList [ key; value ] :: rest ->
                        add ((key, value) :: List.remove_assoc key entries) rest
                    | _ -> Error.error "into map expects key/value entries"
                  in
                  add entries forms
              | _ -> Error.error "into expects a macro collection"))
  | _ -> Error.error "into expects a target and source collection"

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
            match List.rev forms with
            | _ :: rest -> Form (FList (List.rev rest))
            | [] -> nil)
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
                    | Ok value -> (
                        let emitted =
                          if name = "mapcat" then sequence_forms value
                          else
                            form_of_value value
                            |> Result.map (fun form -> [ form ])
                        in
                        match emitted with
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

and eval_vary_meta context = function
  | form :: function_form :: extra_forms -> (
      match
        ( eval context form,
          eval context function_form,
          eval_forms context extra_forms )
      with
      | (Error _ as error), _, _ -> error
      | _, (Error _ as error), _ -> error
      | _, _, (Error _ as error) -> error
      | Ok form, Ok fn, Ok extras -> (
          match apply_value context fn (Form (FMap []) :: extras) with
          | Error _ as error -> error
          | Ok _ -> (
              match form with
              | Form form -> Ok (Form (strip_internal_metadata form))
              | _ -> Ok form)))
  | _ ->
      Error.error "vary-meta expects a form, function, and optional arguments"

and syntax_quote context form =
  let generated = ref [] in
  let rec quote = function
    | FList [ FSymbol "unquote"; expression ] -> eval context expression
    | FList [ FSymbol "if-cljs"; then_form; _else_form ] ->
        (* LG exposes a namespace-bearing macro environment and uses the
           portable defrecord model on every runtime target. Select the same
           branch as ClojureScript without evaluating JVM compiler helpers. *)
        quote then_form
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
            let is_macro =
              Option.is_some
                (Env.find_macro ~scope:context.namespace name
                   context.compiler_env)
              || Option.is_some
                   (Env.find_inline_macro ~scope:context.namespace name
                      context.compiler_env)
            in
            if is_macro then context.namespace ^ "/" ^ name else name
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

let expand ~scope ~compiler_env (definition : Macro_definition.t) args =
  let direct_unary_expansion =
    match (definition.arities, args) with
    | ( [
          {
            Macro_definition.params = [ FSymbol parameter ];
            body =
              [
                FList
                  [
                    FSymbol "syntax-quote";
                    FList
                      [
                        FSymbol callee;
                        FList [ FSymbol "unquote"; FSymbol argument ];
                      ];
                  ];
              ];
          };
        ],
        [ value ] )
      when String.equal parameter argument ->
        let callee =
          if String.contains callee '/' then callee
          else
            let is_macro =
              Option.is_some
                (Env.find_macro ~scope:definition.namespace callee compiler_env)
              || Option.is_some
                   (Env.find_inline_macro ~scope:definition.namespace callee
                      compiler_env)
            in
            if is_macro then definition.namespace ^ "/" ^ callee else callee
        in
        Some (FList [ FSymbol callee; value ])
    | _ -> None
  in
  match direct_unary_expansion with
  | Some expanded -> Ok expanded
  | None when definition.name = "declare+" ->
    let rec declared_name = function
      | FList [ FSymbol "__type-hint"; _; FSymbol name ] :: _ -> Ok name
      | FSymbol metadata :: rest when String.starts_with ~prefix:"^" metadata ->
          declared_name rest
      | FSymbol name :: _ -> Ok name
      | _ -> Error.error "declare+ expects a function name"
    in
    Result.map
      (fun name -> FList [ FSymbol "declare"; FSymbol name ])
      (declared_name args)
  | None ->
      let macro_environment =
        Form (FMap [ (FKeyword ":ns", FString scope) ])
      in
      let context =
        {
          compiler_env;
          namespace = definition.namespace;
          locals = [ ("&env", macro_environment) ];
        }
      in
      Result.bind (invoke_definition context definition args) form_of_value

let rec expand_all ~scope ~compiler_env = function
  | FList (FSymbol ("quote" | "syntax-quote") :: _ as forms) ->
      Ok (FList forms)
  | FList (FSymbol "record" :: record_type :: field_forms) ->
      let rec expand_fields expanded = function
        | [] ->
            Ok
              (FList
                 (FSymbol "record" :: record_type :: List.rev expanded))
        | FList [ field_name; value ] :: rest ->
            Result.bind (expand_all ~scope ~compiler_env value) (fun value ->
                expand_fields
                  (FList [ field_name; value ] :: expanded)
                  rest)
        | _ -> Error.error "record fields must be (field value) pairs"
      in
      expand_fields [] field_forms
  | FList (FSymbol "let" :: FVector bindings :: body_forms) ->
      let shadow_pattern env pattern =
        Destructure.pattern_names pattern
        |> List.fold_left
             (fun env name -> Env.without_source_callable ~scope name env)
             env
      in
      let rec expand_bindings env expanded = function
        | pattern :: value :: rest ->
            Result.bind
              (expand_all ~scope ~compiler_env:env pattern)
              (fun pattern ->
                Result.bind
                  (expand_all ~scope ~compiler_env:env value)
                  (fun value ->
                    expand_bindings (shadow_pattern env pattern)
                      (value :: pattern :: expanded) rest))
        | [] ->
            Result.map
              (fun body_forms ->
                FList
                  (FSymbol "let"
                  :: FVector (List.rev expanded)
                  :: body_forms))
              (expand_all_forms ~scope ~compiler_env:env body_forms)
        | remaining ->
            Result.bind
              (expand_all_forms ~scope ~compiler_env:env remaining)
              (fun remaining ->
                Result.map
                  (fun body_forms ->
                    FList
                      (FSymbol "let"
                      :: FVector (List.rev_append expanded remaining)
                      :: body_forms))
                  (expand_all_forms ~scope ~compiler_env:env body_forms))
      in
      expand_bindings compiler_env [] bindings
  | FList (FSymbol name :: args) -> (
      match Env.find_macro ~scope name compiler_env with
      | Some definition
        when not
               (Env.source_callable_shadowed ~scope name compiler_env)
        ->
          Result.bind (expand ~scope ~compiler_env definition args) (fun expanded ->
              expand_all ~scope ~compiler_env expanded)
      | Some _ | None -> (
          match Env.find_inline_macro ~scope name compiler_env with
          | Some definition
            when not
                   (Env.source_callable_shadowed ~scope name compiler_env) ->
              Result.bind (expand ~scope ~compiler_env definition args)
                (fun expanded -> expand_all ~scope ~compiler_env expanded)
          | Some _ | None ->
              Result.map
                (fun forms -> FList forms)
                (expand_all_forms ~scope ~compiler_env
                   (FSymbol name :: args))))
  | FList forms ->
      Result.map
        (fun forms -> FList forms)
        (expand_all_forms ~scope ~compiler_env forms)
  | FVector forms ->
      Result.map
        (fun forms -> FVector forms)
        (expand_all_forms ~scope ~compiler_env forms)
  | FMap pairs ->
      let rec expand_pairs expanded = function
        | [] -> Ok (FMap (List.rev expanded))
        | (key, value) :: rest ->
            Result.bind (expand_all ~scope ~compiler_env key) (fun key ->
                Result.bind (expand_all ~scope ~compiler_env value)
                  (fun value ->
                    expand_pairs ((key, value) :: expanded) rest))
      in
      expand_pairs [] pairs
  | form -> Ok form

and expand_all_forms ~scope ~compiler_env forms =
  let rec loop expanded = function
    | [] -> Ok (List.rev expanded)
    | form :: rest ->
        Result.bind (expand_all ~scope ~compiler_env form) (fun form ->
            loop (form :: expanded) rest)
  in
  loop [] forms
