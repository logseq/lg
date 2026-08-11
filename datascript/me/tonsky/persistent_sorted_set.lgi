(ns me.tonsky.persistent-sorted-set)

(type-record tree [value]
  (keys :array<value>)
  (children :array<option<tree<value>>>)
  (_weak-children :array<option<weak<tree<value>>>>)
  (_addresses :array<option<int>>)
  (_address :ref<option<int>>)
  (_dirty :ref<bool>))

(type-record storage [value owner write]
  (restore :fn<int;option<tree<value>>>)
  (accessed :fn<int;unit>)
  (store :fn<tree<value>;option<int>;int>)
  (delete :fn<array<int>;unit>)
  (owner :option<owner>)
  (pending-deletes :ref<array<int>>)
  (drain-writes :fn<unit;vector<write>>)
  (ref-type :Lg_runtime.Runtime_ref_type.t))

(type-record btset [value owner write]
  (root :ref<option<tree<value>>>)
  (_weak-root :ref<option<weak<tree<value>>>>)
  (ref-type :ref<Lg_runtime.Runtime_ref_type.t>)
  (branching-factor :ref<int>)
  (shift :ref<int>)
  (cnt :ref<int>)
  (comparator :fn<value;value;ordering>)
  (storage :ref<option<storage<value;owner;write>>>)
  (_address :ref<option<int>>))

#?(:native
   (type-record iterator [value owner write]
     (iter-set :btset<value;owner;write>)
     (iter-left :int)
     (iter-right :int)
     (iter-keys :array<value>)
     (iter-idx :int))
   :cljs
   (type-record iterator [value owner write]
     (iter-set :btset<value;owner;write>)
     (iter-left :float)
     (iter-right :float)
     (iter-keys :array<value>)
     (iter-idx :int)))

(type-record set-settings
  (branching-factor :int)
  (ref-type :Lg_runtime.Runtime_ref_type.t))

(signature me.tonsky.persistent-sorted-set/restore-child
  [value owner write]
  :fn<array<option<tree<value>>>;array<option<weak<tree<value>>>>;int;storage<value;owner;write>;int;tree<value>>)

(signature me.tonsky.persistent-sorted-set/node-child
  [value owner write]
  :fn<tree<value>;int;option<storage<value;owner;write>>;tree<value>>)

(signature me.tonsky.persistent-sorted-set/node-address
  [value]
  :fn<tree<value>;option<int>>)

(signature me.tonsky.persistent-sorted-set/node-lookup
  [value owner write]
  :fn<tree<value>;fn<value;value;ordering>;value;option<storage<value;owner;write>>;option<value>>)

(signature me.tonsky.persistent-sorted-set/storage-drain-writes
  [value owner write]
  :fn<storage<value;owner;write>;vector<write>>)

(signature me.tonsky.persistent-sorted-set/storage-with-ref-type
  [value owner write]
  :fn<storage<value;owner;write>;Lg_runtime.Runtime_ref_type.t;storage<value;owner;write>>)

(signature me.tonsky.persistent-sorted-set/node-store
  [value owner write]
  :fn<tree<value>;storage<value;owner;write>;int>)

(signature me.tonsky.persistent-sorted-set/with-ref-type
  [value owner write]
  :fn<btset<value;owner;write>;Lg_runtime.Runtime_ref_type.t;btset<value;owner;write>>)

(signature me.tonsky.persistent-sorted-set/restore-root
  [value owner write]
  :fn<btset<value;owner;write>;tree<value>>)

(signature me.tonsky.persistent-sorted-set/set-root
  [value owner write]
  :fn<btset<value;owner;write>;tree<value>>)

(signature me.tonsky.persistent-sorted-set/delete-address
  [value owner write]
  :fn<option<storage<value;owner;write>>;option<int>;unit>)

(signature me.tonsky.persistent-sorted-set/delete-removed-addresses
  [value owner write]
  :fn<option<storage<value;owner;write>>;array<option<int>>;int;int;array<option<int>>;unit>)

(signature me.tonsky.persistent-sorted-set/rotate
  [value owner write]
  :fn<tree<value>;bool;option<tree<value>>;option<tree<value>>;option<storage<value;owner;write>>;array<tree<value>>>)

(signature me.tonsky.persistent-sorted-set/node-disj
  [value owner write]
  :fn<tree<value>;fn<value;value;ordering>;value;bool;option<tree<value>>;option<tree<value>>;option<storage<value;owner;write>>;option<array<tree<value>>>>)

(signature me.tonsky.persistent-sorted-set/node-collect-addresses
  [value owner write]
  :fn<tree<value>;option<storage<value;owner;write>>;vector<int>;vector<int>>)

(signature me.tonsky.persistent-sorted-set/node-keys-at-path
  [value owner write]
  #?(:native
     :fn<tree<value>;int;int;option<storage<value;owner;write>>;array<value>>
     :cljs
     :fn<tree<value>;float;int;option<storage<value;owner;write>>;array<value>>))

(signature me.tonsky.persistent-sorted-set/node-value-at-path
  [value owner write]
  #?(:native
     :fn<tree<value>;int;int;option<storage<value;owner;write>>;value>
     :cljs
     :fn<tree<value>;float;int;option<storage<value;owner;write>>;value>))

#?(:native
   (signature me.tonsky.persistent-sorted-set/-seek*
     [value owner write]
     :fn<tree<value>;value;fn<value;value;ordering>;int;option<storage<value;owner;write>>;option<int>>)
   :cljs
   (signature me.tonsky.persistent-sorted-set/-seek*
     [value owner write]
     :fn<tree<value>;value;fn<value;value;ordering>;int;option<storage<value;owner;write>>;option<float>>))

#?(:native
   (signature me.tonsky.persistent-sorted-set/-rseek*
     [value owner write]
     :fn<tree<value>;value;fn<value;value;ordering>;int;option<storage<value;owner;write>>;int>)
   :cljs
   (signature me.tonsky.persistent-sorted-set/-rseek*
     [value owner write]
     :fn<tree<value>;value;fn<value;value;ordering>;int;option<storage<value;owner;write>>;float>))

#?(:native
   (signature me.tonsky.persistent-sorted-set/-next-path
     [value owner write]
     :fn<tree<value>;int;int;option<storage<value;owner;write>>;option<int>>)
   :cljs
   (signature me.tonsky.persistent-sorted-set/-next-path
     [value owner write]
     :fn<tree<value>;float;int;option<storage<value;owner;write>>;option<float>>))

#?(:native
   (signature me.tonsky.persistent-sorted-set/-distance
     [value owner write]
     :fn<btset<value;owner;write>;tree<value>;int;int;int;int>)
   :cljs
   (signature me.tonsky.persistent-sorted-set/-distance
     [value owner write]
     :fn<btset<value;owner;write>;tree<value>;float;float;int;int>))

#?(:native
   (signature me.tonsky.persistent-sorted-set/slice-bounds-with-keys
     [value owner write]
     :fn<tree<value>;value;value;fn<value;value;ordering>;int;option<storage<value;owner;write>>;option<tuple<int;int;array<value>>>>)
   :cljs
   (signature me.tonsky.persistent-sorted-set/slice-bounds-with-keys
     [value owner write]
     :fn<tree<value>;value;value;fn<value;value;ordering>;int;option<storage<value;owner;write>>;option<tuple<float;float;array<value>>>>))

(signature me.tonsky.persistent-sorted-set/iterator-option-seq
  [value owner write]
  :fn<option<iterator<value;owner;write>>;seq<value>>)

(signature me.tonsky.persistent-sorted-set/set-seq
  [value owner write]
  :fn<btset<value;owner;write>;seq<value>>)

(signature me.tonsky.persistent-sorted-set/set-addresses
  [value owner write]
  :fn<btset<value;owner;write>;vector<int>>)

(signature me.tonsky.persistent-sorted-set/from-sequential
  [value]
  :fn<fn<value;value;ordering>;seqable<value>;btset<value;unit;unit>>)

(signature me.tonsky.persistent-sorted-set/seek-first
  [value owner write]
  :fn<btset<value;owner;write>;value;fn<value;value;ordering>;option<value>>)

(signature me.tonsky.persistent-sorted-set/seek [value storage]
  :overload<fn<seqable<value;storage>;value;seq<value>>;fn<seqable<value;storage>;value;fn<value;value;ordering>;seq<value>>>)

(signature me.tonsky.persistent-sorted-set/set-slice-reduce-with
  [value owner write result]
  :fn<btset<value;owner;write>;value;value;fn<value;value;ordering>;fn<result;value;result>;result;result>)

(signature me.tonsky.persistent-sorted-set/restore
  [value owner write]
  :overload<fn<int;storage<value;owner;write>;btset<value;owner;write>>;fn<int;storage<value;owner;write>;set-settings;btset<value;owner;write>>>)

(signature me.tonsky.persistent-sorted-set/restore-by
  [value owner write]
  :overload<fn<fn<value;value;ordering>;int;storage<value;owner;write>;int;int;btset<value;owner;write>>;fn<fn<value;value;ordering>;int;storage<value;owner;write>;int;int;Lg_runtime.Runtime_ref_type.t;btset<value;owner;write>>;fn<fn<value;value;ordering>;int;storage<value;owner;write>;int;int;Lg_runtime.Runtime_ref_type.t;int;btset<value;owner;write>>>)

(signature me.tonsky.persistent-sorted-set/store
  [value owner write]
  :fn<btset<value;owner;write>;storage<value;owner;write>;int>)
