let write writer text = Buffer.add_string writer text

let print_namespace_maps = ref false

let render printer =
  let writer = Buffer.create 64 in
  printer writer;
  Buffer.contents writer

let render_values select_printer separator values =
  let writer = Buffer.create 64 in
  let first = ref true in
  Seq.iter
    (fun (printers, value) ->
      if !first then first := false else Buffer.add_string writer separator;
      Buffer.add_string writer (select_printer printers value))
    values;
  Buffer.contents writer

let render_display_values separator values =
  render_values (fun (display, _) value -> display value) separator values

let render_readable_values separator values =
  render_values (fun (_, readable) value -> readable value) separator values
