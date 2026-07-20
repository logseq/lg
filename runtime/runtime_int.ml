let hash_combine seed hash_value =
  let open Int32 in
  let seed = of_int seed in
  let hash = of_int hash_value in
  let mixed =
    add hash
      (add (-1640531527l)
         (add (shift_left seed 6) (shift_right seed 2)))
  in
  to_int (logxor seed mixed)

let hash_combine_int64 seed hash_value =
  Int64.of_int
    (hash_combine (Int64.to_int seed) (Int64.to_int hash_value))

let format_hex value width =
  if width < 0L || width > Int64.of_int Sys.max_string_length then
    invalid_arg "hex width is out of range";
  let digits = "0123456789abcdef" in
  let rec encode value encoded =
    if value = 0L then encoded
    else
      let digit = Int64.logand value 15L |> Int64.to_int in
      encode (Int64.shift_right_logical value 4) (digits.[digit] :: encoded)
  in
  let encoded = if value = 0L then [ '0' ] else encode value [] in
  let encoded = String.of_seq (List.to_seq encoded) in
  let padding = max 0 (Int64.to_int width - String.length encoded) in
  String.make padding '0' ^ encoded
