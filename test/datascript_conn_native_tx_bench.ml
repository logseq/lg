module B = Datascript_conn_native_base
module M = Lg_runtime.Runtime_map
module V = Datascript_runtime.Data_value

let entity index =
  B.datascript_db_tx_entity
    (M.of_list
    [
      (":db/id", V.Int (-index));
      (":item/id", V.Int index);
      (":item/status", V.String "pending");
    ])

let operation entity_id value =
  B.datascript_db_tx_add (V.Entity_id entity_id) ":item/id" (V.Int value)

let person index =
  let name = "name-" ^ string_of_int ((index * 7919) mod 8) in
  let last_name =
    "last-" ^ string_of_int ((index * 3571) mod 6)
  in
  B.datascript_db_tx_entity
    (M.of_list
    [
      (":db/id", V.Ref_to (V.Temp_id (string_of_int index)));
      (":id", V.Int index);
      (":name", V.String name);
      (":last-name", V.String last_name);
      (":full-name", V.String (name ^ " " ^ last_name));
      ( ":alias",
        V.Vector
             [
               V.String "A. C. Q. W.";
               V.String "A. C. Q. W.";
               V.String "A. C. Q. W.";
               V.String "A. C. Q. W.";
               V.String "A. C. Q. W.";
             ] );
      (":sex", V.Keyword ":male");
      (":age", V.Int ((index * 7919) mod 100));
      (":salary", V.Int ((index * 7919) mod 100_000));
    ])

let benchmark_schema =
  M.of_list
    [
      ( ":id",
        M.of_list
          [
            (":db/unique", V.Keyword ":db.unique/identity");
          ] );
      ( ":follows",
        M.of_list
          [
            (":db/valueType", V.Keyword ":db.type/ref");
            (":db/cardinality", V.Keyword ":db.cardinality/many");
          ] );
      ( ":alias",
        M.of_list
          [
            (":db/cardinality", V.Keyword ":db.cardinality/many");
          ] );
    ]

let () =
  let count = int_of_string Sys.argv.(1) in
  let mode = if Array.length Sys.argv > 2 then Sys.argv.(2) else "seq" in
  let database =
    if mode = "people" || mode = "people-random" || mode = "people-vector" then
      B.datascript_core_empty_db__arity_1_1 benchmark_schema
    else B.datascript_core_empty_db__arity_0_0 ()
  in
  let values =
    List.init count (fun index ->
        let index =
          if mode = "people-random" then
            (((index + 1) * 7919) mod count) + 1
          else index + 1
        in
        if mode = "people" || mode = "people-random" || mode = "people-vector" then
          [ person index ]
        else if mode = "three-ops" then
          [
            operation (-index) index;
            B.datascript_db_tx_add
              (V.Entity_id (-index))
              ":item/status"
              (V.String "pending");
          ]
        else if mode = "ops" then [ operation (-index) index ]
        else if mode = "same-tempid" then [ operation (-1) index ]
        else if mode = "positive-ops" then [ operation index index ]
        else [ entity index ])
    |> List.concat
  in
  let transactions =
    match mode with
    | "list" | "vector" | "people-vector" | "seq" ->
        Rrbvec.of_list values
    | "ops" | "three-ops" | "same-tempid" | "positive-ops" | "people"
    | "people-random" ->
        Rrbvec.of_list values
    | mode -> invalid_arg ("unknown transaction collection: " ^ mode)
  in
  let started_at = Unix.gettimeofday () in
  let _ = B.datascript_core_db_with database transactions in
  let elapsed = Unix.gettimeofday () -. started_at in
  Printf.printf "%d %.6f\n" count elapsed
