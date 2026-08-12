let write writer text = Buffer.add_string writer text

let output_line text newline flush_on_newline =
  print_string text;
  if newline then (
    print_string "\n";
    if flush_on_newline then flush stdout)
  else ()

let string_ends_with_dot value =
  let length = String.length value in
  length > 0 && value.[length - 1] = '.'

let ensure_decimal_fraction value =
  if string_ends_with_dot value then value ^ "0" else value

let clj_display_float value = string_of_float value |> ensure_decimal_fraction

let clj_readable_float value =
  if Float.is_nan value then "##NaN"
  else if value = Float.infinity then "##Inf"
  else if value = Float.neg_infinity then "##-Inf"
  else clj_display_float value

let integral_float value =
  Float.is_finite value && Float.floor value = value

let cljs_display_float value =
  if Float.is_nan value then "NaN"
  else if value = Float.infinity then "Infinity"
  else if value = Float.neg_infinity then "-Infinity"
  else if integral_float value then string_of_int (int_of_float value)
  else string_of_float value

let cljs_readable_float value =
  if Float.is_nan value then "##NaN"
  else if value = Float.infinity then "##Inf"
  else if value = Float.neg_infinity then "##-Inf"
  else cljs_display_float value

let clj_readable_char = function
  | ' ' -> "\\space"
  | '\n' -> "\\newline"
  | '\t' -> "\\tab"
  | '\r' -> "\\return"
  | '\b' -> "\\backspace"
  | '\012' -> "\\formfeed"
  | value -> "\\" ^ String.make 1 value

let cljs_readable_char value = Printf.sprintf "%S" (String.make 1 value)

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
