type serialized_datom = {
  e : int;
  a : string;
  v : Data_value.t;
  tx : int;
}

type serialized_node = {
  keys : serialized_datom Rrbvec.t;
  addresses : int Rrbvec.t option;
}

type serialized_index = {
  address : int;
  shift : int;
  count : int;
}

type ref_type = Lg_runtime.Runtime_ref_type.t = Strong | Weak

type serialized_root = {
  schema :
    (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
    option;
  max_eid : int;
  max_tx : int;
  eavt : int;
  aevt : int;
  avet : int;
  eavt_metadata : serialized_index;
  aevt_metadata : serialized_index;
  avet_metadata : serialized_index;
  max_address : int;
  branching_factor : int;
  ref_type : ref_type;
}

type t =
  | Stored_node of serialized_node
  | Stored_root of serialized_root
  | Stored_tail of serialized_datom Rrbvec.t Rrbvec.t

let serialized_datom e a v tx = { e; a; v; tx }
let serialized_keyword_datom e (a : Lg_runtime.Runtime_keyword.t) v tx =
  { e; a; v; tx }

let serialized_node keys addresses = { keys; addresses }
let serialized_index address shift count = { address; shift; count }
let datom_e datom = datom.e
let datom_a datom = datom.a
let datom_v datom = datom.v
let datom_tx datom = datom.tx
let node_keys node = node.keys
let node_addresses node = node.addresses
let index_address index = index.address
let index_shift index = index.shift
let index_count index = index.count

let serialized_root schema max_eid max_tx eavt aevt avet eavt_metadata
    aevt_metadata avet_metadata max_address branching_factor ref_type =
  {
    schema;
    max_eid;
    max_tx;
    eavt;
    aevt;
    avet;
    eavt_metadata;
    aevt_metadata;
    avet_metadata;
    max_address;
    branching_factor;
    ref_type;
  }

let root_schema root = root.schema
let root_max_eid root = root.max_eid
let root_max_tx root = root.max_tx
let root_eavt root = root.eavt
let root_aevt root = root.aevt
let root_avet root = root.avet
let root_eavt_metadata root = root.eavt_metadata
let root_aevt_metadata root = root.aevt_metadata
let root_avet_metadata root = root.avet_metadata
let root_max_address root = root.max_address
let root_branching_factor root = root.branching_factor
let root_ref_type root = root.ref_type
