type 'a t = {
  values : 'a option array;
  mutable length : int;
  mutable frozen : bool;
}

let create capacity =
  if capacity < 0 then invalid_arg "chunk-buffer capacity must be non-negative";
  { values = Array.make capacity None; length = 0; frozen = false }

let append buffer value =
  if buffer.frozen then invalid_arg "chunk-buffer is already frozen";
  if buffer.length >= Array.length buffer.values then
    invalid_arg "chunk-buffer capacity exceeded";
  buffer.values.(buffer.length) <- Some value;
  buffer.length <- buffer.length + 1

let to_array buffer =
  if buffer.frozen then invalid_arg "chunk-buffer is already frozen";
  let values =
    Array.init buffer.length (fun index ->
        match buffer.values.(index) with
        | Some value -> value
        | None -> invalid_arg "chunk-buffer contains an uninitialized slot")
  in
  buffer.frozen <- true;
  values
