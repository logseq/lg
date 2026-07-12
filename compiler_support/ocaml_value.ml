type argument_label =
  | Unlabelled
  | Labelled of string
  | Optional of string

type value_type =
  | Variable
  | Arrow of argument_label * value_type * value_type
  | Tuple of value_type list
  | Constructor of string * value_type list
  | Opaque

type constructor_type = {
  arguments : value_type list;
  result : value_type;
}

let initialized = ref false

let init include_dirs =
  if not !initialized then (
    Compmisc.init_path ();
    initialized := true);
  List.iter (fun dir -> Load_path.add_dir ~hidden:false dir) include_dirs

let argument_label = function
  | Asttypes.Nolabel -> Unlabelled
  | Labelled name -> Labelled name
  | Optional name -> Optional name

let rec normalize type_expr =
  match (Types.Transient_expr.repr type_expr).desc with
  | Tvar _ | Tunivar _ -> Variable
  | Tarrow (label, argument, result, _) ->
      Arrow (argument_label label, normalize argument, normalize result)
  | Ttuple elements -> Tuple (List.map (fun (_, ty) -> normalize ty) elements)
  | Tconstr (path, arguments, _) ->
      Constructor (Path.name path, List.map normalize arguments)
  | _ -> Opaque

let exception_message exn =
  Format.asprintf "%a" Location.report_exception exn |> String.trim

let lookup ~include_dirs name =
  try
    init include_dirs;
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> Error ("invalid OCaml value name " ^ name)
    | Some longident ->
        let env = Compmisc.initial_env () in
        let _, description =
          Env.lookup_value ~use:false ~loc:Location.none longident env
        in
        Ok (normalize description.val_type)
  with exn -> Error (exception_message exn)

let lookup_constructor ~include_dirs name =
  try
    init include_dirs;
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> Error ("invalid OCaml constructor name " ^ name)
    | Some longident ->
        let env = Compmisc.initial_env () in
        let description =
          Env.lookup_constructor ~use:false ~loc:Location.none Env.Positive
            longident env
        in
        Ok
          { arguments = List.map normalize description.cstr_args;
            result = normalize description.cstr_res }
  with exn -> Error (exception_message exn)
