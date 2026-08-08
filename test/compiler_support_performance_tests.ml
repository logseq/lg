let expect_lookup name =
  match Lg_compiler_support.Ocaml_value.lookup ~include_dirs:[] name with
  | Ok _ -> ()
  | Error message -> failwith (name ^ ": " ^ message)

let test_distinct_lookups_share_the_initial_environment () =
  let names =
    [
      "List.map";
      "List.mapi";
      "List.rev_map";
      "List.filter_map";
      "List.concat_map";
      "List.fold_left";
      "List.fold_right";
      "List.iter";
      "List.iteri";
      "List.length";
      "List.compare_lengths";
      "List.compare_length_with";
      "List.cons";
      "List.hd";
      "List.tl";
      "List.nth";
      "List.nth_opt";
      "List.rev";
      "List.init";
      "List.append";
      "List.rev_append";
      "List.concat";
      "List.flatten";
      "List.combine";
      "List.split";
      "List.mem";
      "List.memq";
      "List.assoc";
      "List.assq";
      "List.mem_assoc";
      "List.mem_assq";
      "List.remove_assoc";
      "List.remove_assq";
      "List.find";
      "List.find_opt";
      "List.find_map";
      "List.filter";
      "List.find_all";
      "List.partition";
      "List.partition_map";
      "List.sort";
      "List.stable_sort";
      "List.fast_sort";
      "List.sort_uniq";
      "List.merge";
      "List.to_seq";
      "List.of_seq";
      "List.equal";
      "List.compare";
      "Array.length";
      "Array.get";
      "Array.set";
      "Array.make";
      "Array.create_float";
      "Array.init";
      "Array.make_matrix";
      "Array.append";
      "Array.concat";
      "Array.sub";
      "Array.copy";
      "Array.fill";
      "Array.blit";
      "Array.to_list";
      "Array.of_list";
      "Array.iter";
      "Array.iteri";
      "Array.map";
      "Array.mapi";
      "Array.fold_left";
      "Array.fold_right";
      "Array.iter2";
      "Array.map2";
      "Array.for_all";
      "Array.exists";
      "Array.mem";
      "Array.memq";
      "Array.find_opt";
      "Array.find_map";
      "Array.split";
      "Array.combine";
      "Array.sort";
      "Array.stable_sort";
      "Array.fast_sort";
      "Array.to_seq";
      "Array.to_seqi";
      "Array.of_seq";
    ]
  in
  let started_at = Sys.time () in
  List.iter expect_lookup names;
  let elapsed = Sys.time () -. started_at in
  if elapsed > 0.01 then
    failwith
      (Printf.sprintf
         "distinct OCaml signature lookups rebuilt the initial environment: %.3fs"
         elapsed)

let () = test_distinct_lookups_share_the_initial_environment ()
