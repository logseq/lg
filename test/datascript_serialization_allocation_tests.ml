module Serialization_value = Datascript_runtime.Serialization_value
module Storage_value = Datascript_runtime.Storage_value

let serialized_json count =
  let buffer = Buffer.create (count * 24) in
  Printf.bprintf buffer
    {|{"count":%d,"tx0":536870912,"max-eid":0,"max-tx":536870912,"schema":"nil","attrs":[":value"],"keywords":[],"eavt":[|}
    count;
  for index = 0 to count - 1 do
    if index > 0 then Buffer.add_char buffer ',';
    Printf.bprintf buffer "[%d,0,%d,0]" index index
  done;
  Buffer.add_string buffer
    {|],"aevt":null,"avet":null,"branching-factor":32,"ref-type":"strong"}|};
  Buffer.contents buffer

let () =
  let count = 100_000 in
  let value = Serialization_value.encode_non_keyword (Datascript_runtime.Data_value.Int 1) in
  let datom = Serialization_value.datom 1 0 value 1 in
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  let _distinct_datoms =
    Array.init count (fun index ->
        Serialization_value.datom index 0 value index)
  in
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 16_000_000. then
    failwith
      (Printf.sprintf
         "serialized datom structural integers remain wide-boxed: %.0f bytes"
         allocated_bytes);
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
         allocated_bytes);
  let json_datom_count = 50_000 in
  let source = serialized_json json_datom_count in
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  let prepared =
    Serialization_value.prepare (Lg_edn_backend.Json_source source)
  in
  if Serialization_value.prepared_count prepared <> json_datom_count then
    failwith "prepared JSON count changed";
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 23_000_000. then
    failwith
      (Printf.sprintf
         "JSON preparation materialized one record per datom: %.0f bytes"
         allocated_bytes);
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  for index = 0 to json_datom_count - 1 do
    let datom = Serialization_value.prepared_datom prepared index in
    ignore (Serialization_value.prepared_datom_entity datom);
    ignore (Serialization_value.prepared_datom_attribute datom);
    ignore (Serialization_value.prepared_datom_value datom);
    ignore (Serialization_value.prepared_datom_tx datom)
  done;
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  if allocated_bytes >= 8_000_000. then
    failwith
      (Printf.sprintf
         "prepared JSON datoms unpacked the same row repeatedly: %.0f bytes"
         allocated_bytes)
