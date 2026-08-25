type value = {
  rendered : string;
  type_name : string;
}

let published = ref None
let clear () = published := None
let publish type_name rendered = published := Some { rendered; type_name }

let take () =
  let value = !published in
  published := None;
  value
