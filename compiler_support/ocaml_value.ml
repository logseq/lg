type argument_label =
  | Unlabelled
  | Labelled of string
  | Optional of string

type value_type =
  | Variable of int
  | Arrow of argument_label * value_type * value_type
  | Tuple of value_type list
  | Constructor of string * value_type list
  | Opaque

type constructor_type = {
  arguments : value_type list;
  result : value_type;
}

let initialized = ref false
let initial_env_cache = ref None

let init include_dirs =
  let uses_melange =
    List.exists (fun path -> Filename.basename path = "melange") include_dirs
  in
  let standard_include_dirs =
    if uses_melange then []
    else
      [ "unix"; "str" ]
      |> List.map (Filename.concat Config.standard_library)
      |> List.filter Sys.file_exists
  in
  let include_dirs = standard_include_dirs @ include_dirs in
  if not !initialized then (
    if not uses_melange then
      Clflags.include_dirs :=
        List.sort_uniq String.compare (include_dirs @ !Clflags.include_dirs);
    Compmisc.init_path ();
    initialized := true);
  match !initial_env_cache with
  | Some (cached_dirs, env) when cached_dirs = include_dirs -> env
  | Some _ | None ->
      List.iter (fun dir -> Load_path.add_dir ~hidden:false dir) include_dirs;
      let env = Compmisc.initial_env () in
      initial_env_cache := Some (include_dirs, env);
      env

let argument_label = function
  | Asttypes.Nolabel -> Unlabelled
  | Labelled name -> Labelled name
  | Optional name -> Optional name

let rec normalize type_expr =
  let type_expr = Btype.proxy type_expr in
  let id = Types.get_id type_expr in
  match (Types.Transient_expr.repr type_expr).desc with
  | Tvar _ | Tunivar _ -> Variable id
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
    let env = init include_dirs in
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> Error ("invalid OCaml value name " ^ name)
    | Some longident ->
        let _, description =
          Env.lookup_value ~use:false ~loc:Location.none longident env
        in
        Ok (normalize description.val_type)
  with exn -> Error (exception_message exn)

let lookup_constructor ~include_dirs name =
  try
    let env = init include_dirs in
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> Error ("invalid OCaml constructor name " ^ name)
    | Some longident ->
        let description =
          Env.lookup_constructor ~use:false ~loc:Location.none Env.Positive
            longident env
        in
        Ok
          { arguments = List.map normalize description.cstr_args;
            result = normalize description.cstr_res }
  with exn -> Error (exception_message exn)

let lookup_label ~include_dirs name =
  try
    let env = init include_dirs in
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> Error ("invalid OCaml record field name " ^ name)
    | Some longident ->
        let description =
          Env.lookup_label ~use:false ~loc:Location.none Env.Projection longident
            env
        in
        Ok (normalize description.lbl_arg)
  with exn -> Error (exception_message exn)
