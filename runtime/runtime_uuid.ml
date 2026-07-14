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

let of_string text = { high = 0; low = 0; text = Some text }

let to_string uuid =
  match uuid.text with
  | Some text -> text
  | None -> Printf.sprintf "%016x-%016x" uuid.high uuid.low

let getmostsignificantbits uuid = uuid.high

let getleastsignificantbits uuid = uuid.low
