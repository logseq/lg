type entry = Ast.form * Location.t
type identity = Source_node_id.t * Location.t

module Form_table = Hashtbl.Make (struct
  type t = Ast.form

  let equal left right = left == right

  let leaf_hash = function
    | Ast.FSymbol value -> Hashtbl.hash (0, value)
    | Ast.FCoreSymbol value -> Hashtbl.hash (1, value)
    | Ast.FKeyword value -> Hashtbl.hash (2, value)
    | Ast.FString value -> Hashtbl.hash (3, value)
    | Ast.FRegex value -> Hashtbl.hash (4, value)
    | Ast.FInt value -> Hashtbl.hash (5, value)
    | Ast.FFloat value -> Hashtbl.hash (6, value)
    | Ast.FDecimal value -> Hashtbl.hash (7, value)
    | Ast.FChar value -> Hashtbl.hash (8, value)
    | Ast.FBool value -> Hashtbl.hash (9, value)
    | Ast.FList _ -> 10
    | Ast.FVector _ -> 11
    | Ast.FMap _ -> 12

  let hash = function
    | (Ast.FSymbol _ | Ast.FCoreSymbol _ | Ast.FKeyword _ | Ast.FString _
      | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _ | Ast.FDecimal _ | Ast.FChar _
      | Ast.FBool _)
      as form ->
        leaf_hash form
    | Ast.FList forms -> (
        match forms with
        | head :: _ -> Hashtbl.hash (9, List.length forms, leaf_hash head)
        | [] -> 9)
    | Ast.FVector forms -> Hashtbl.hash (10, List.length forms)
    | Ast.FMap fields -> (
        match fields with
        | (key, _) :: _ ->
            Hashtbl.hash (11, List.length fields, leaf_hash key)
        | [] -> 12)
end)

type provenance = identity Form_table.t

let identities : identity Form_table.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Form_table.create 0)

let source_unit : string option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let current_source_unit () =
  Domain.DLS.get source_unit |> Option.value ~default:"unknown-source"

let find_identity form = Form_table.find_opt (Domain.DLS.get identities) form
let find form = Option.map snd (find_identity form)

let enrich_error form (error : Error.t) =
  match find_identity form with
  | None -> error
  | Some (node_id, location) ->
      let error_belongs_to_identity =
        match error.location with
        | None -> true
        | Some error_location ->
            error_location.loc_start.pos_fname = location.loc_start.pos_fname
            && error_location.loc_start.pos_cnum = location.loc_start.pos_cnum
            && error_location.loc_end.pos_cnum = location.loc_end.pos_cnum
      in
      let related =
        if not error_belongs_to_identity then []
        else
          Source_node_id.origins node_id
          |> List.map (fun (origin : Source_node_id.origin) ->
                 {
                   Error.location = origin.location;
                   message = origin.message;
                 })
      in
      let same_related (left : Error.related) (right : Error.related) =
        left.message = right.message
        && left.location.loc_start.pos_fname
           = right.location.loc_start.pos_fname
        && left.location.loc_start.pos_cnum = right.location.loc_start.pos_cnum
        && left.location.loc_end.pos_cnum = right.location.loc_end.pos_cnum
      in
      let additions =
        List.fold_left
          (fun accumulated candidate ->
            if
              List.exists (same_related candidate) error.related
              || List.exists (same_related candidate) accumulated
            then accumulated
            else candidate :: accumulated)
          [] related
        |> List.rev
      in
      let related = error.related @ additions in
      {
        error with
        location = Some (Option.value error.location ~default:location);
        related;
      }

let replace_identity form identity =
  Form_table.replace (Domain.DLS.get identities) form identity

let copy_location ~source ~target =
  match find_identity source with
  | None -> ()
  | Some identity -> replace_identity target identity

let anonymous_record_owner owner =
  match Domain.DLS.get source_unit with
  | None -> owner
  | Some source_unit when owner = "" -> source_unit
  | Some source_unit -> source_unit ^ "." ^ owner

let with_source_unit unit_id f =
  let previous = Domain.DLS.get source_unit in
  Domain.DLS.set source_unit (Some unit_id);
  Fun.protect ~finally:(fun () -> Domain.DLS.set source_unit previous) f

let with_locations entries f =
  let previous = Domain.DLS.get identities in
  let current = Form_table.create (List.length entries) in
  let unit_id = current_source_unit () in
  List.iter
    (fun (form, location) ->
      let node_id = Source_node_id.create ~source_unit:unit_id location in
      Form_table.replace current form (node_id, location))
    entries;
  Domain.DLS.set identities current;
  Fun.protect ~finally:(fun () -> Domain.DLS.set identities previous) f

let rec fold_forms f acc form =
  let acc = f acc form in
  match form with
  | Ast.FList forms | Ast.FVector forms -> List.fold_left (fold_forms f) acc forms
  | Ast.FMap entries ->
      List.fold_left
        (fun acc (key, value) -> fold_forms f (fold_forms f acc key) value)
        acc entries
  | Ast.FSymbol _ | Ast.FCoreSymbol _ | Ast.FKeyword _ | Ast.FString _
  | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _ | Ast.FDecimal _ | Ast.FChar _
  | Ast.FBool _ ->
      acc

let capture forms =
  let provenance = Form_table.create 16 in
  let capture_form () form =
    match find_identity form with
    | Some identity -> Form_table.replace provenance form identity
    | None -> ()
  in
  List.iter
    (fun form ->
      ignore (fold_forms capture_form () form))
    forms;
  provenance

let register_macro_expansion ~call_site ~arguments ~definition_provenance
    ~template_location expanded =
  let call_identity = find_identity call_site in
  let argument_table = Form_table.create 16 in
  List.iter
    (fun argument ->
      ignore
        (fold_forms
           (fun () form ->
             match find_identity form with
             | Some identity -> Form_table.replace argument_table form identity
             | None -> ())
           () argument))
    arguments;
  match call_identity with
  | None -> ()
  | Some (call_node_id, call_location) ->
      let unit_id = current_source_unit () in
      let rec register path form =
        match Form_table.find_opt argument_table form with
        | Some identity -> replace_identity form identity
        | None ->
            let template_origins =
              match Form_table.find_opt definition_provenance form with
              | Some (template_id, template_location)
                when template_location <> call_location ->
                  {
                    Source_node_id.location = template_location;
                    message = "generated from this macro template";
                  }
                  :: Source_node_id.origins template_id
              | Some (template_id, _) -> Source_node_id.origins template_id
              | None -> (
                  match template_location with
                  | Some location ->
                      [
                        {
                          Source_node_id.location;
                          message = "generated from this macro template";
                        };
                      ]
                  | None -> Source_node_id.origins call_node_id)
            in
            let node_id =
              Source_node_id.generated ~source_unit:unit_id
                ~location:call_location ~path ~origins:template_origins
            in
            replace_identity form (node_id, call_location);
            (match form with
            | Ast.FList forms | Ast.FVector forms ->
                List.iteri (fun index child -> register (index :: path) child) forms
            | Ast.FMap entries ->
                List.iteri
                  (fun index (key, value) ->
                    register (0 :: index :: path) key;
                    register (1 :: index :: path) value)
                  entries
            | Ast.FSymbol _ | Ast.FCoreSymbol _ | Ast.FKeyword _ | Ast.FString _
            | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _ | Ast.FDecimal _
            | Ast.FChar _ | Ast.FBool _ -> ())
      in
      register [] expanded
