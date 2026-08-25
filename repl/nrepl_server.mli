val serve_connection :
  worker_path:string ->
  state_path:string ->
  input:in_channel ->
  output:out_channel ->
  unit
