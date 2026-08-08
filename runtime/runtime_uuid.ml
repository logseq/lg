type t = {
  high : int;
  low : int;
  text : string option;
}

let initialized = lazy (Random.self_init ())

let random_word () =
  Lazy.force initialized;
  (Random.bits () lsl 30) lor Random.bits ()

let randomuuid () = { high = random_word (); low = random_word (); text = None }

let create high low = { high; low; text = None }

let of_string text =
  { high = 0; low = 0; text = Some (String.lowercase_ascii text) }

let valid_string text =
  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  let rec valid_at index =
    if index = String.length text then true
    else if index = 8 || index = 13 || index = 18 || index = 23 then
      text.[index] = '-' && valid_at (index + 1)
    else is_hex text.[index] && valid_at (index + 1)
  in
  String.length text = 36 && valid_at 0

let to_string uuid =
  match uuid.text with
  | Some text -> text
  | None -> Printf.sprintf "%016x-%016x" uuid.high uuid.low

let getmostsignificantbits uuid = uuid.high

let getleastsignificantbits uuid = uuid.low
