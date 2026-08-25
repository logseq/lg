type t =
  | Byte_string of string
  | Integer of int64
  | List of t list
  | Dictionary of (string * t) list

let max_byte_string_bytes = 8 * 1024 * 1024
let max_nesting_depth = 64
let max_value_bytes = 8 * 1024 * 1024

let validate_integer text =
  let length = String.length text in
  if length = 0 then Error "empty bencode integer"
  else if String.equal text "-0" then Error "negative zero is not canonical bencode"
  else if length > 1 && text.[0] = '0' then
    Error "bencode integer has a leading zero"
  else if length > 2 && text.[0] = '-' && text.[1] = '0' then
    Error "negative bencode integer has a leading zero"
  else
    match Int64.of_string_opt text with
    | Some value -> Ok value
    | None -> Error "invalid bencode integer"

let validate_length text =
  let length = String.length text in
  if length = 0 then Error "empty bencode byte string length"
  else if length > 1 && text.[0] = '0' then
    Error "bencode byte string length has a leading zero"
  else
    match int_of_string_opt text with
    | Some value when value <= max_byte_string_bytes -> Ok value
    | Some _ -> Error "bencode byte string exceeds maximum size"
    | None -> Error "invalid bencode byte string length"

let to_string value =
  let output = Buffer.create 128 in
  let rec encode depth = function
    | _ when depth > max_nesting_depth ->
        Error "bencode value exceeds maximum nesting depth"
    | Byte_string value ->
        if String.length value > max_byte_string_bytes then
          Error "bencode byte string exceeds maximum size"
        else (
          Buffer.add_string output (string_of_int (String.length value));
          Buffer.add_char output ':';
          Buffer.add_string output value;
          Ok ())
    | Integer value ->
        Buffer.add_char output 'i';
        Buffer.add_string output (Int64.to_string value);
        Buffer.add_char output 'e';
        Ok ()
    | List values ->
        Buffer.add_char output 'l';
        let result = List.fold_left (fun result value -> Result.bind result (fun () -> encode (depth + 1) value)) (Ok ()) values in
        Result.map
          (fun () -> Buffer.add_char output 'e')
          result
    | Dictionary entries ->
        Buffer.add_char output 'd';
        let entries = List.sort (fun (left, _) (right, _) -> String.compare left right) entries in
        let encode_entry result (key, value) =
          Result.bind result (fun () ->
              Result.bind (encode (depth + 1) (Byte_string key)) (fun () ->
                  encode (depth + 1) value))
        in
        let result = List.fold_left encode_entry (Ok ()) entries in
        Result.map
          (fun () -> Buffer.add_char output 'e')
          result
  in
  Result.bind (encode 0 value) (fun () ->
      if Buffer.length output > max_value_bytes then
        Error "bencode value exceeds maximum size"
      else Ok (Buffer.contents output))

type source = { next : unit -> (char option, string) result }

let limited_source next =
  let consumed = ref 0 in
  {
    next =
      (fun () ->
        if !consumed >= max_value_bytes then
          Error "bencode value exceeds maximum size"
        else
          Result.map
            (function
              | None -> None
              | Some character ->
                  incr consumed;
                  Some character)
            (next ()));
  }

let read_required source context =
  Result.bind (source.next ()) (function
    | Some character -> Ok character
    | None -> Error ("truncated bencode " ^ context))

let read_until source delimiter context =
  let output = Buffer.create 16 in
  let rec loop () =
    Result.bind (read_required source context) (fun character ->
        if character = delimiter then Ok (Buffer.contents output)
        else (
          Buffer.add_char output character;
          loop ()))
  in
  loop ()

let read_exact source length =
  let output = Bytes.create length in
  let rec loop index =
    if index = length then Ok (Bytes.unsafe_to_string output)
    else
      Result.bind (read_required source "byte string") (fun character ->
          Bytes.set output index character;
          loop (index + 1))
  in
  loop 0

let parse source =
  let rec value depth first =
    if depth > max_nesting_depth then
      Error "bencode value exceeds maximum nesting depth"
    else
      match first with
      | 'i' ->
          Result.bind (read_until source 'e' "integer") (fun text ->
              Result.map (fun value -> Integer value) (validate_integer text))
      | 'l' ->
          let rec values collected =
            Result.bind (read_required source "list") (function
              | 'e' -> Ok (List (List.rev collected))
              | first ->
                  Result.bind (value (depth + 1) first) (fun value ->
                      values (value :: collected)))
          in
          values []
      | 'd' ->
          let rec entries collected =
            Result.bind (read_required source "dictionary") (function
              | 'e' -> Ok (Dictionary (List.rev collected))
              | first ->
                  Result.bind (byte_string first) (fun key ->
                      Result.bind
                        (read_required source "dictionary value")
                        (fun first ->
                          Result.bind (value (depth + 1) first) (fun value ->
                              entries ((key, value) :: collected)))))
          and byte_string first =
            if first < '0' || first > '9' then
              Error "bencode dictionary key must be a byte string"
            else
              let length_text = Buffer.create 8 in
              Buffer.add_char length_text first;
              let rec length () =
                Result.bind
                  (read_required source "byte string length")
                  (function
                    | ':' ->
                        Result.bind
                          (validate_length (Buffer.contents length_text))
                          (read_exact source)
                    | character when character >= '0' && character <= '9' ->
                        Buffer.add_char length_text character;
                        length ()
                    | _ -> Error "invalid bencode byte string length")
              in
              length ()
          in
          entries []
      | first when first >= '0' && first <= '9' ->
          Result.map (fun value -> Byte_string value) (byte_string first)
      | _ -> Error "invalid bencode value prefix"
  and byte_string first =
    let length_text = Buffer.create 8 in
    Buffer.add_char length_text first;
    let rec length () =
      Result.bind (read_required source "byte string length") (function
        | ':' ->
            Result.bind (validate_length (Buffer.contents length_text))
              (read_exact source)
        | character when character >= '0' && character <= '9' ->
            Buffer.add_char length_text character;
            length ()
        | _ -> Error "invalid bencode byte string length")
    in
    length ()
  in
  Result.bind (source.next ()) (function
    | None -> Ok None
    | Some first -> Result.map Option.some (value 0 first))

let read input =
  let next () =
    try Ok (Some (input_char input)) with End_of_file -> Ok None | Sys_error message -> Error ("unable to read bencode value: " ^ message)
  in
  parse (limited_source next)

let of_string encoded =
  let index = ref 0 in
  let next () =
    if !index = String.length encoded then Ok None
    else
      let character = encoded.[!index] in
      incr index;
      Ok (Some character)
  in
  Result.bind (parse (limited_source next)) (function
    | None -> Error "empty bencode input"
    | Some value ->
        if !index = String.length encoded then Ok value
        else Error "trailing data after bencode value")

let write output value =
  Result.bind (to_string value) (fun encoded ->
      try
        output_string output encoded;
        flush output;
        Ok ()
      with Sys_error message -> Error ("unable to write bencode value: " ^ message))
