module Serialization_value = Datascript_runtime.Serialization_value
module Storage_value = Datascript_runtime.Storage_value

let () =
  let count = 100_000 in
  let value = Serialization_value.encode_non_keyword (Datascript_runtime.Data_value.Int 1) in
  let datom = Serialization_value.datom 1 0 value 1 in
  let datoms = Rrbvec.of_array (Array.make count datom) in
  let indexes = Rrbvec.of_array (Array.init count Fun.id) in
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  let serialized =
    Serialization_value.database count 536870912 count 1 "nil"
      (Rrbvec.of_list [ ":value" ])
      Rrbvec.empty datoms (Some indexes) (Some indexes) 32
      Storage_value.Strong
  in
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 14_000_000. then
    failwith
      (Printf.sprintf
         "serialized database copied RRB vectors through lists: %.0f bytes"
         allocated_bytes);
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  ignore (Serialization_value.datoms serialized);
  ignore (Serialization_value.aevt serialized);
  ignore (Serialization_value.avet serialized);
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 6_000_000. then
    failwith
      (Printf.sprintf
         "serialized database access copied arrays through lists: %.0f bytes"
         allocated_bytes);
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  for _ = 1 to count do
    ignore (Serialization_value.datom_entity datom);
    ignore (Serialization_value.datom_attribute datom);
    ignore (Serialization_value.datom_value datom);
    ignore (Serialization_value.datom_tx datom)
  done;
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 1_024. then
    failwith
      (Printf.sprintf
         "serialized datom access copied fields through lists: %.0f bytes"
         allocated_bytes)
