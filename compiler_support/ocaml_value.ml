type argument_label =
  | Unlabelled
  | Labelled of string
  | Optional of string

type value_type =
  | Variable of int
  | Arrow of argument_label * value_type * value_type
  | Tuple of value_type list
  | Constructor of string * value_type list
  | Variant of (string * value_type option) list * bool * string list
  | Opaque

type constructor_type = {
  arguments : value_type list;
  result : value_type;
}

type record_field = {
  label_name : string;
  label_type : value_type;
  mutable_ : bool;
}

type record_type = { fields : record_field list }

let initialized = ref false
let initial_env_cache = ref None
let configured_melange = ref None
let known_include_dirs = ref []

let unique_directories directories =
  List.fold_left
    (fun unique directory ->
      if List.mem directory unique then unique else unique @ [ directory ])
    [] directories

let init ?melange include_dirs =
  let detected_melange =
    List.exists (fun path -> Filename.basename path = "melange") include_dirs
  in
  let uses_melange =
    match melange, !configured_melange with
    | Some selected, _ -> selected
    | None, Some configured -> configured
    | None, None -> detected_melange
  in
  if Option.is_some !configured_melange
     && !configured_melange <> Some uses_melange then (
    known_include_dirs := [];
    Clflags.include_dirs := [];
    initialized := false;
    initial_env_cache := None;
    Env.reset_cache ();
    Envaux.reset_cache ());
  configured_melange := Some uses_melange;
  let standard_include_dirs =
    if uses_melange then []
    else
      [ "unix"; "str" ]
      |> List.map (Filename.concat Config.standard_library)
      |> List.filter Sys.file_exists
  in
  let include_dirs =
    if uses_melange then
      List.filter
        (fun path ->
          Filename.basename path <> "byte"
          && Filename.basename path <> "native")
        include_dirs
    else include_dirs
  in
  let include_dirs =
    unique_directories
      (standard_include_dirs @ include_dirs @ !known_include_dirs)
  in
  known_include_dirs := include_dirs;
  Clflags.include_dirs :=
    unique_directories
      (if uses_melange then include_dirs
       else include_dirs @ !Clflags.include_dirs);
  if not !initialized then (
    Clflags.no_std_include := uses_melange;
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
  match Types.get_desc type_expr with
  | Tvariant row ->
      let fields = Types.row_fields row in
      let rec convert tags required = function
        | [] -> Variant (List.sort compare tags, Types.row_closed row, List.rev required)
        | (tag, field) :: rest ->
            (match Types.row_field_repr field with
             | Rabsent -> convert tags required rest
             | Rpresent payload -> convert ((tag, Option.map normalize payload) :: tags) (tag :: required) rest
             | Reither (true, [], _) -> convert ((tag, None) :: tags) required rest
             | Reither (false, [payload], _) -> convert ((tag, Some (normalize payload)) :: tags) required rest
             | Reither _ -> Opaque) in
      convert [] [] fields
  | _ -> normalize_nonvariant type_expr
and normalize_nonvariant type_expr =
  let type_expr = Btype.proxy type_expr in
  let id = Types.get_id type_expr in
  match (Types.Transient_expr.repr type_expr).desc with
  | Tvar _ | Tunivar _ -> Variable id
  | Tarrow (label, argument, result, _) ->
      Arrow (argument_label label, normalize argument, normalize result)
  | Ttuple elements -> Tuple (List.map (fun (_, ty) -> normalize ty) elements)
  | Tconstr (path, arguments, _) ->
      Constructor (Path.name path, List.map normalize arguments)
  | Tpoly (body, _) | Tlink body -> normalize body
  | _ -> Opaque

let exception_message exn =
  Format.asprintf "%a" Location.report_exception exn |> String.trim

let canonical_type_path ~include_dirs name =
  try
    let env = init include_dirs in
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> name
    | Some longident ->
        let path, _ = Env.lookup_type ~use:false ~loc:Location.none longident env in
        Path.name (Env.normalize_type_path None env path)
  with _ -> name

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

let lookup_record ~include_dirs name =
  try
    let env = init include_dirs in
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> Error ("invalid OCaml record type name " ^ name)
    | Some longident ->
        let _, declaration =
          Env.lookup_type ~use:false ~loc:Location.none longident env
        in
        (match declaration.type_kind with
        | Type_record (labels, _) ->
            Ok
              {
                fields =
                  List.map
                    (fun (label : Types.label_declaration) ->
                      {
                        label_name = Ident.name label.ld_id;
                        label_type = normalize label.ld_type;
                        mutable_ = label.ld_mutable = Asttypes.Mutable;
                      })
                    labels;
              }
        | _ -> Error ("OCaml type " ^ name ^ " is not a record"))
  with exn -> Error (exception_message exn)

let lookup_type_manifest ~include_dirs name =
  try
    let env = init include_dirs in
    match Longident.unflatten (String.split_on_char '.' name) with
    | None -> Error ("invalid OCaml type name " ^ name)
    | Some longident ->
        let _, declaration =
          Env.lookup_type ~use:false ~loc:Location.none longident env
        in
        (match declaration.type_manifest with
        | Some manifest -> Ok (normalize manifest)
        | None -> Error ("OCaml type " ^ name ^ " is not a transparent alias"))
  with exn -> Error (exception_message exn)
