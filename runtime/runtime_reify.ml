type 'a t = { payload : 'a; dynamic : Runtime_dynamic.t }

let make payload dynamic = { payload; dynamic }
let payload value = value.payload
let dynamic value = value.dynamic
