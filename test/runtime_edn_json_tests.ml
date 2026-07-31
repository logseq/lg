module Edn = Lg_edn_backend
module Runtime_edn = Lg_runtime.Runtime_edn

let assert_json expected value =
  assert (String.equal expected (Runtime_edn.write_json_string value))

let () =
  assert_json "null" Edn.Nil;
  assert_json "true" (Edn.Bool true);
  assert_json {|["value",42,1.5,"NaN","Infinity","-Infinity"]|}
    (Edn.Vector
       [|
         Edn.String "value";
         Edn.Small_int 42;
         Edn.Float 1.5;
         Edn.Float nan;
         Edn.Float infinity;
         Edn.Float neg_infinity;
       |]);
  assert_json
    {|{"name":"Ada","kind":":person","tagged":{"tag":"uuid","value":"id"}}|}
    (Edn.Map
       [|
         (Edn.String "name", Edn.String "Ada");
         (Edn.String "kind", Edn.Keyword "person");
         (Edn.String "tagged", Edn.Tagged ("uuid", Edn.String "id"));
       |]);
  assert_json {|["9007199254740992"]|}
    (Edn.List [| Edn.Int 9007199254740992L |]);
  assert_json {|[1,2,"value",3]|}
    (Edn.Int4_vector (1, 2, Edn.String "value", 3));
  assert_json {|[[1,2,"first",3],[4,5,"second",6]]|}
    (Edn.Vector
       [|
         Edn.Int4_vector (1, 2, Edn.String "first", 3);
         Edn.Int4_vector (4, 5, Edn.String "second", 6);
       |]);
  assert_json
    {|[[1,2,"first",3],[4,5,{"tag":"tag","value":"value"},6],7]|}
    (Edn.Vector
       [|
         Edn.Int4_vector (1, 2, Edn.String "first", 3);
         Edn.Int4_vector (4, 5, Edn.Tagged ("tag", Edn.String "value"), 6);
         Edn.Small_int 7;
       |]);
  assert_json "[]" (Edn.Int_vector [||]);
  assert_json {|[1,2,3]|} (Edn.Int_vector [| 1; 2; 3 |])

let () =
  let value =
    Edn.Vector
      [|
        Edn.String "quote: \"";
        Edn.String "backslash: \\";
        Edn.String "line\nbreak";
        Edn.String "control: \001";
        Edn.String "unicode: λ";
      |]
  in
  let encoded = Runtime_edn.write_json_string value in
  assert (Runtime_edn.read_json_string encoded = value)

let () =
  let source = {|{"name":"Ada","values":[1,2,3]}|} in
  let value = Runtime_edn.read_json_source source in
  assert (String.equal source (Runtime_edn.write_json_string value))

let () =
  let count = 20_000 in
  let value =
    Edn.Map
      [|
        ( Edn.String "values",
          Edn.Vector (Array.init count (fun value -> Edn.Small_int value)) );
        (Edn.String "name", Edn.String "benchmark");
      |]
  in
  let encoded = Runtime_edn.write_json_string value in
  let decoded = Runtime_edn.read_json_string encoded in
  assert (decoded = value)

let () =
  match Sys.getenv_opt "LG_EDN_JSON_WRITE_STRESS_COUNT" with
  | None -> ()
  | Some source ->
      let count = int_of_string source in
      let value =
        Edn.Vector
          (Array.init count (fun value -> Edn.Small_int value))
      in
      ignore (Runtime_edn.write_json_string value)
