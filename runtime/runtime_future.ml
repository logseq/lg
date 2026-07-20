type 'a t = { value : 'a }

let call function_ = { value = function_ () }
let get future = future.value
let realized _future = true
