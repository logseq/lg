module D = Lg_runtime.Runtime_dynamic

let entity index =
  D.map
    [
      (D.keyword ":db/id", D.int (Int64.neg index));
      (D.keyword ":item/id", D.int index);
      (D.keyword ":item/status", D.string "pending");
    ]

let operation entity_id value =
  D.vector
    (Rrbvec.of_list
       [
         D.keyword ":db/add";
         D.int entity_id;
         D.keyword ":item/id";
         D.int value;
       ])

let () =
  let count = int_of_string Sys.argv.(1) in
  let mode = if Array.length Sys.argv > 2 then Sys.argv.(2) else "seq" in
  if
    mode = "dynamic-map" || mode = "dynamic-pair"
    || mode = "dynamic-conversion"
  then (
    let started_at = Unix.gettimeofday () in
    let map = ref (D.map []) in
    let reverse = ref (D.map []) in
    for index = 1 to count do
      let key = Int64.of_int (-index) |> D.int in
      let value = Int64.of_int index |> D.int in
      map := D.assoc !map key value;
      if mode = "dynamic-pair" then
        reverse := D.assoc !reverse value (D.set (Seq.return key))
    done;
    if mode = "dynamic-conversion" then
      let static =
        D.entries !map |> Lg_runtime.Runtime_map.of_list_dynamic
      in
      let static =
        Datascript_conn_native_base.datascript_util_removem
          (fun _ -> false) static
      in
      ignore (D.map (Lg_runtime.Runtime_map.to_list static));
    Printf.printf "%d %.6f\n" count (Unix.gettimeofday () -. started_at);
    exit 0);
  let database =
    Datascript_conn_native_base.datascript_core_empty_db__arity_0_0 ()
  in
  let values =
    List.init count (fun index ->
        let index = Int64.of_int (index + 1) in
        if mode = "three-ops" then
          [
            operation (Int64.neg index) index;
            D.vector
              (Rrbvec.of_list
                 [
                   D.keyword ":db/add";
                   D.int (Int64.neg index);
                   D.keyword ":item/status";
                   D.string "pending";
                 ]);
          ]
        else if mode = "ops" then [ operation (Int64.neg index) index ]
        else if mode = "same-tempid" then [ operation (-1L) index ]
        else if mode = "positive-ops" then [ operation index index ]
        else [ entity index ])
    |> List.concat
  in
  let transactions =
    match mode with
    | "list" -> D.list values
    | "vector" -> D.vector (Rrbvec.of_list values)
    | "seq" -> D.seq (List.to_seq values)
    | "ops" | "three-ops" | "same-tempid" | "positive-ops" -> D.list values
    | mode -> invalid_arg ("unknown transaction collection: " ^ mode)
  in
  let started_at = Unix.gettimeofday () in
  let _ =
    Datascript_conn_native_base.datascript_core_db_with database transactions
  in
  let elapsed = Unix.gettimeofday () -. started_at in
  Printf.printf "%d %.6f\n" count elapsed
