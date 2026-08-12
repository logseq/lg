let write writer text = Buffer.add_string writer text

let print_namespace_maps = Runtime_reference.of_value false

let render_strings separator print_length values =
  let rec append writer remaining = function
    | [] -> ()
    | value :: rest -> (
        match remaining with
        | Some n when n <= 0 ->
            if Buffer.length writer > 0 then Buffer.add_string writer separator;
            Buffer.add_string writer "..."
        | Some n ->
            if Buffer.length writer > 0 then Buffer.add_string writer separator;
            Buffer.add_string writer value;
            append writer (Some (n - 1)) rest
        | None ->
            if Buffer.length writer > 0 then Buffer.add_string writer separator;
            Buffer.add_string writer value;
            append writer None rest)
  in
  let writer = Buffer.create 64 in
  append writer print_length values;
  Buffer.contents writer

let render printer =
  let writer = Buffer.create 64 in
  printer writer;
  Buffer.contents writer

let render_values select_printer separator print_length values =
  let writer = Buffer.create 64 in
  let rec append first remaining values =
    match values () with
    | Seq.Nil -> ()
    | Seq.Cons ((printers, value), rest) -> (
        match remaining with
        | Some n when n <= 0 ->
            if not first then Buffer.add_string writer separator;
            Buffer.add_string writer "..."
        | Some n ->
            if not first then Buffer.add_string writer separator;
            Buffer.add_string writer (select_printer printers value);
            append false (Some (n - 1)) rest
        | None ->
            if not first then Buffer.add_string writer separator;
            Buffer.add_string writer (select_printer printers value);
            append false None rest)
  in
  append true print_length values;
  Buffer.contents writer

let render_display_values separator values =
  render_values (fun (display, _) value -> display value) separator None values

let render_readable_values separator values =
  render_values (fun (_, readable) value -> readable value) separator None values

let render_display_values_with_length separator print_length values =
  render_values (fun (display, _) value -> display value) separator print_length
    values

let render_readable_values_with_length separator print_length values =
  render_values (fun (_, readable) value -> readable value) separator print_length
    values
