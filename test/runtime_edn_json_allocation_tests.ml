module Edn = Lg_edn_backend
module Runtime_edn = Lg_runtime.Runtime_edn

let () =
  let count = 100_000 in
  let value =
    Edn.Map
      [|
        ( Edn.String "values",
          Edn.Vector
            (Array.init count (fun value ->
                 Edn.Int (Int64.of_int value))) );
        (Edn.String "name", Edn.String "benchmark");
      |]
  in
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  let _encoded = Runtime_edn.write_json_string value in
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 5_000_000. then
    failwith
      (Printf.sprintf
         "JSON writing copied the complete closed EDN tree: %.0f bytes"
         allocated_bytes);
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  let _decoded = Runtime_edn.read_json_string _encoded in
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 16_000_000. then
    failwith
      (Printf.sprintf
         "JSON reading copied the complete closed EDN tree: %.0f bytes"
         allocated_bytes)
