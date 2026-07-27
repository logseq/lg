module Edn = Lg_edn_backend
module Runtime_edn = Lg_runtime.Runtime_edn

let () =
  let count = 20_000 in
  let value =
    Edn.Map
      [|
        (Edn.String "values", Edn.Vector (Array.init count (fun value -> Edn.Int (Int64.of_int value))));
        (Edn.String "name", Edn.String "benchmark");
      |]
  in
  let encoded = Runtime_edn.write_json_string value in
  let decoded = Runtime_edn.read_json_string encoded in
  assert (decoded = value)
