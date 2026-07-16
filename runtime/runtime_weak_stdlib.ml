let make value =
  let reference = Weak.create 1 in
  Weak.set reference 0 (Some value);
  Runtime_weak.of_getter (fun () -> Weak.get reference 0)
