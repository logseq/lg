let deref reference = !reference

let reset reference value =
  reference := value;
  value

let vreset reference value =
  reference := value;
  value

let swap reference update = reset reference (update !reference)
