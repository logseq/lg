let write writer text = Buffer.add_string writer text

let print_namespace_maps = ref (Runtime_dynamic.bool false)

let render printer =
  let writer = Buffer.create 64 in
  printer writer;
  Buffer.contents writer
