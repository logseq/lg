open Lg_runtime

type 'a weak_ref

external create : 'a -> 'a weak_ref = "WeakRef" [@@mel.new]

external deref : 'a weak_ref -> 'a option = "deref"
  [@@mel.send] [@@mel.return { undefined_to_opt }]

let make value =
  let reference = create value in
  Runtime_weak.of_getter (fun () -> deref reference)
