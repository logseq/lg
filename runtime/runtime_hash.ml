let c1 = Int32.of_string "0xcc9e2d51"
let c2 = Int32.of_string "0x1b873593"

let rotate_left value bits =
  Int32.logor (Int32.shift_left value bits)
    (Int32.shift_right_logical value (32 - bits))

let mix_k1 value =
  value |> Int32.mul c1 |> fun value -> rotate_left value 15
  |> fun value -> Int32.mul value c2

let mix_h1 hash value =
  Int32.logxor hash value |> fun hash -> rotate_left hash 13
  |> fun hash -> Int32.add (Int32.mul hash 5l) 0xe6546b64l

let fmix hash length =
  let hash = Int32.logxor hash (Int32.of_int length) in
  let hash = Int32.logxor hash (Int32.shift_right_logical hash 16) in
  let hash = Int32.mul hash 0x85ebca6bl in
  let hash = Int32.logxor hash (Int32.shift_right_logical hash 13) in
  let hash = Int32.mul hash 0xc2b2ae35l in
  Int32.logxor hash (Int32.shift_right_logical hash 16)

let hash_int32 value =
  if value = 0l then 0
  else fmix (mix_h1 0l (mix_k1 value)) 4 |> Int32.to_int

let hash_int64 value =
  if value = 0L then 0
  else
    let low = Int64.to_int32 value in
    let high = Int64.shift_right_logical value 32 |> Int64.to_int32 in
    let hash = mix_h1 0l (mix_k1 low) in
    fmix (mix_h1 hash (mix_k1 high)) 8 |> Int32.to_int

let hash_int value =
  if Sys.word_size = 32 then
    hash_int64
      (Int64.of_float (Runtime_int_melange.to_float_unchecked value))
  else hash_int64 (Int64.of_int value)

let clojure_string_hash value =
  String.fold_left
    (fun hash char ->
      Int32.add (Int32.mul hash 31l) (Int32.of_int (Char.code char)))
    0l value

let hash_string value = clojure_string_hash value |> hash_int32

let hash_unencoded_chars value =
  let rec pairs hash index =
    if index + 1 >= String.length value then hash
    else
      let packed =
        Int32.logor
          (Int32.of_int (Char.code value.[index]))
          (Int32.shift_left (Int32.of_int (Char.code value.[index + 1])) 16)
      in
      pairs (mix_h1 hash (mix_k1 packed)) (index + 2)
  in
  let hash = pairs 0l 0 in
  let hash =
    if String.length value land 1 = 0 then hash
    else
      let last = Char.code value.[String.length value - 1] |> Int32.of_int in
      Int32.logxor hash (mix_k1 last)
  in
  fmix hash (2 * String.length value) |> Int32.to_int

let hash_float value =
  let bits = Int64.bits_of_float value in
  let low = Int64.to_int32 bits in
  let high = Int64.shift_right_logical bits 32 |> Int64.to_int32 in
  Int32.logxor low high |> Int32.to_int

let hash_combine left right = Runtime_int.hash_combine left right

let split_identifier value =
  match String.rindex_opt value '/' with
  | None -> (None, value)
  | Some separator ->
      ( Some (String.sub value 0 separator),
        String.sub value (separator + 1) (String.length value - separator - 1) )

let hash_symbol value =
  let namespace, name = split_identifier value in
  hash_combine (hash_unencoded_chars name)
    (namespace |> Option.map clojure_string_hash |> Option.value ~default:0l
    |> Int32.to_int)

let hash_keyword value =
  let value =
    if String.starts_with ~prefix:":" value then
      String.sub value 1 (String.length value - 1)
    else value
  in
  Int32.add (Int32.of_int (hash_symbol value)) (-1640531527l)
  |> Int32.to_int

let mix_collection_hash hash count =
  fmix (mix_h1 0l (mix_k1 (Int32.of_int hash))) count |> Int32.to_int

let hash_ordered hashes =
  let hash, count =
    Seq.fold_left
      (fun (hash, count) value ->
        (Int32.add (Int32.mul hash 31l) (Int32.of_int value), count + 1))
      (1l, 0) hashes
  in
  mix_collection_hash (Int32.to_int hash) count

let hash_unordered hashes =
  let hash, count =
    Seq.fold_left
      (fun (hash, count) value ->
        (Int32.add hash (Int32.of_int value), count + 1))
      (0l, 0) hashes
  in
  mix_collection_hash (Int32.to_int hash) count
