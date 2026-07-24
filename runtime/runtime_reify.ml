type 'a t = { payload : 'a }

let make payload = { payload }
let payload value = value.payload
