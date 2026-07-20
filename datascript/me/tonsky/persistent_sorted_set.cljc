(ns me.tonsky.persistent-sorted-set
  (:refer-clojure :exclude [conj disj contains?])
  (:require
    [me.tonsky.persistent-sorted-set.arrays :as arrays]))

(def max-safe-path #?(:native 2147483648 :cljs 2147483648.0))
(def bits-per-level 5)
(def max-len 32)
(def min-len 16)
(def max-safe-level 6)
(def bit-mask 31)
(def factors
  #?(:native
     (array
      1 32 1024 32768 1048576 33554432 1073741824 34359738368
      1099511627776 35184372088832 1125899906842624)
     :cljs
     (array
      1.0 32.0 1024.0 32768.0 1048576.0 33554432.0 1073741824.0
      34359738368.0 1099511627776.0 35184372088832.0 1125899906842624.0)))
(def empty-path #?(:native 0 :cljs 0.0))

(defn path-get
  #?(:native [^:int path ^:int level]
     :cljs [^:float path ^:int level])
  #?(:native
     (if (< level max-safe-level)
       (bit-and (bit-shift-right path (* level bits-per-level)) bit-mask)
       (bit-and (quot path (aget factors level)) bit-mask))
     :cljs
     (if (< level max-safe-level)
       (bit-and
        (bit-shift-right-zero-fill (int path) (* level bits-per-level))
        bit-mask)
       (bit-and
        (int (Float.floor (/ path (aget factors level))))
        bit-mask))))

(defn path-set
  #?(:native [^:int path ^:int level ^:int idx]
     :cljs [^:float path ^:int level ^:int idx])
  #?(:native
     (let [small? (and (< path max-safe-path) (< level max-safe-level))
           old (path-get path level)
           factor (aget factors level)
           minus (if small?
                   (bit-shift-left old (* level bits-per-level))
                   (* old factor))
           plus (if small?
                  (bit-shift-left idx (* level bits-per-level))
                  (* idx factor))]
       (+ (- path minus) plus))
     :cljs
     (let [old (path-get path level)
           factor (aget factors level)]
       (Float.add
        (Float.sub path (Float.mul (double old) factor))
        (Float.mul (double idx) factor)))))

(defn path-inc #?(:native [^:int path] :cljs [^:float path])
  #?(:native (inc path) :cljs (Float.add path 1.0)))

(defn path-dec #?(:native [^:int path] :cljs [^:float path])
  #?(:native (dec path) :cljs (Float.sub path 1.0)))

(defn path-cmp
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  #?(:native (- path1 path2) :cljs (Float.sub path1 path2)))

(defn path-lt
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  (< path1 path2))

(defn path-lte
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  (<= path1 path2))

(defn path-eq
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  (= path1 path2))

(defn path-same-leaf
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  #?(:native
     (if (and (< path1 max-safe-path) (< path2 max-safe-path))
       (= (bit-shift-right path1 bits-per-level)
          (bit-shift-right path2 bits-per-level))
       (= (quot path1 max-len)
          (quot path2 max-len)))
     :cljs
     (= (Float.floor (/ path1 32.0))
        (Float.floor (/ path2 32.0)))))

(defn path-str #?(:native [^:int path] :cljs [^:float path])
  #?(:native
     (loop [result []
            path path]
       (if (= path 0)
         (vec (reverse result))
         (recur (clojure.core/conj result (mod path max-len))
                (quot path max-len))))
     :cljs
     (loop [result []
            path path]
       (if (= path 0.0)
         (vec (reverse result))
         (recur (clojure.core/conj result (int (Float.rem path 32.0)))
                (Float.floor (/ path 32.0)))))))

(defn binary-search-l [cmp arr right key]
  (loop [left 0
         right right]
    (if (<= left right)
      (let [middle (arrays/half (+ left right))
            middle-key (arrays/aget arr middle)]
        (if (neg? #?(:melange (uncurried-compare cmp middle-key key)
                     :default (cmp middle-key key)))
          (recur (inc middle) right)
          (recur left (dec middle))))
      left)))

(defn binary-search-r [cmp arr right key]
  (loop [left 0
         right right]
    (if (<= left right)
      (let [middle (arrays/half (+ left right))
            middle-key (arrays/aget arr middle)]
        (if (pos? #?(:melange (uncurried-compare cmp middle-key key)
                     :default (cmp middle-key key)))
          (recur left (dec middle))
          (recur (inc middle) right)))
      left)))

(defn lookup-exact [cmp arr key]
  (let [length (arrays/alength arr)
        idx (binary-search-l cmp arr (dec length) key)]
    (if (and (< idx length)
             (= 0 #?(:melange
                     (uncurried-compare cmp (arrays/aget arr idx) key)
                     :default
                     (cmp (arrays/aget arr idx) key))))
      idx
      -1)))

(defn lookup-range [cmp arr key]
  (let [length (arrays/alength arr)
        idx (binary-search-l cmp arr (dec length) key)]
    (if (= idx length)
      -1
      idx)))

(defn cut-n-splice [arr cut-from cut-to splice-from splice-to values]
  (let [values-length (arrays/alength values)
        left-length (- splice-from cut-from)
        right-length (- cut-to splice-to)
        values-end (+ left-length values-length)
        result-length (+ left-length values-length right-length)]
    (if (= 0 result-length)
      (arrays/empty-array)
      (let [initial
            (cond
              (> left-length 0) (arrays/aget arr cut-from)
              (> values-length 0) (arrays/aget values 0)
              :else (arrays/aget arr splice-to))
            result (Array.make result-length initial)]
        (arrays/acopy arr cut-from splice-from result 0)
        (arrays/acopy values 0 values-length result left-length)
        (arrays/acopy arr splice-to cut-to result values-end)
        result))))

(defn splice [arr splice-from splice-to values]
  (cut-n-splice arr 0 (arrays/alength arr) splice-from splice-to values))

(defn insert [arr idx values]
  (cut-n-splice arr 0 (arrays/alength arr) idx idx values))

(defn merge-n-split [left right]
  (let [left-length (arrays/alength left)
        right-length (arrays/alength right)
        total-length (+ left-length right-length)
        result-left-length (arrays/half total-length)
        result-right-length (- total-length result-left-length)
        combined-at
        (fn [idx]
          (if (< idx left-length)
            (arrays/aget left idx)
            (arrays/aget right (- idx left-length))))
        result-left (Array.make result-left-length (combined-at 0))
        result-right
        (Array.make
         result-right-length
         (combined-at result-left-length))]
    (if (<= left-length result-left-length)
      (do
        (arrays/acopy left 0 left-length result-left 0)
        (arrays/acopy
         right 0 (- result-left-length left-length) result-left left-length)
        (arrays/acopy
         right (- result-left-length left-length) right-length result-right 0))
      (do
        (arrays/acopy left 0 result-left-length result-left 0)
        (arrays/acopy left result-left-length left-length result-right 0)
        (arrays/acopy
         right 0 right-length result-right (- left-length result-left-length))))
    (arrays/array result-left result-right)))

(defn eq-arr [cmp left left-from left-to right right-from right-to]
  (let [length (- left-to left-from)]
    (and
     (= length (- right-to right-from))
     (loop [idx 0]
       (cond
         (= idx length)
         true

         (not (= 0
                 (cmp
                  (arrays/aget left (+ idx left-from))
                  (arrays/aget right (+ idx right-from)))))
         false

         :else
         (recur (inc idx)))))))

(defn check-n-splice [cmp arr from to new-arr]
  (if (eq-arr cmp arr from to new-arr 0 (arrays/alength new-arr))
    arr
    (splice arr from to new-arr)))

(type-record tree [value]
             (keys :array<value>)
             (children :array<option<tree<value>>>)
             (_weak-children :array<option<weak<tree<value>>>>)
             (_addresses :array<option<int64>>)
             (_address :ref<option<int64>>)
             (_dirty :ref<bool>))

(type-record storage [value]
  (restore :fn<int64;option<tree<value>>>)
  (accessed :fn<int64;unit>)
  (store :fn<tree<value>;option<int64>;int64>)
  (delete :fn<array<int64>;unit>)
  (owner :option<dynamic>)
  (pending-deletes :ref<array<int64>>)
  (drain-writes :fn<unit;vector<vector<dynamic>>>))

(defn make-storage [restore accessed store delete]
  (record storage
          (restore restore)
          (accessed accessed)
          (store store)
          (delete delete)
          (owner nil)
          (pending-deletes (volatile! (arrays/empty-array)))
          (drain-writes (fn [_] []))))

(defn make-storage-with-owner
  [restore accessed store delete owner pending-deletes drain-writes]
  (record storage
          (restore restore)
          (accessed accessed)
          (store store)
          (delete delete)
          (owner (Some owner))
          (pending-deletes pending-deletes)
          (drain-writes drain-writes)))

(defn storage-owner [storage]
  (:owner storage))

(defn storage-pending-deletes [storage]
  (:pending-deletes storage))

(defn storage-drain-writes [storage]
  ((:drain-writes storage) (Stdlib.ignore 0)))

(defn- make-tree [keys children weak-children addresses address dirty]
  (record tree (keys keys) (children children) (_weak-children weak-children) (_addresses addresses) (_address (volatile! address)) (_dirty (volatile! dirty))))

(defn node-address [node]
  (deref (:_address node)))

(defn node-addresses [node]
  (:_addresses node))

(defmacro node-child-count [node]
  `(arrays/alength (:children ~node)))

(defn node-children [node]
  (let [children (:children node)
        weak-children (:_weak-children node)]
    (loop [idx 0
           result []]
      (if (= idx (arrays/alength children))
        (arrays/into-array result)
        (if-some [child (arrays/aget children idx)]
          (recur (inc idx) (clojure.core/conj result child))
          (if-some [reference (arrays/aget weak-children idx)]
            (if-some [child (weak-deref reference)]
              (recur (inc idx) (clojure.core/conj result child))
              (Stdlib.failwith "persistent sorted set child was collected"))
            (Stdlib.failwith "persistent sorted set child is not loaded")))))))

(defn- children-addresses [children]
  (arrays/amap (fn [child] (node-address child)) children))

(defn- loaded-children [children]
  (arrays/amap (fn [child] (Some child)) children))

(defn new-leaf [keys]
  (make-tree
   keys (arrays/empty-array) (arrays/empty-array) (arrays/empty-array) nil true))

(defn- new-leaf-address [keys address]
  (make-tree
   keys (arrays/empty-array) (arrays/empty-array) (arrays/empty-array) address true))

(defn new-restored-leaf [keys address]
  (make-tree
   keys (arrays/empty-array) (arrays/empty-array) (arrays/empty-array) (Some address) false))

(defn new-node [keys children]
  (make-tree
   keys
   (loaded-children children)
   (arrays/make-array (arrays/alength children) nil)
   (children-addresses children)
   nil
   true))

(defn- new-node-state [keys children weak-children addresses address]
  (make-tree keys children weak-children addresses address true))

(defn new-restored-node [keys addresses address]
  (make-tree
   keys
   (arrays/amap (fn [_address] nil) addresses)
   (arrays/amap (fn [_address] nil) addresses)
   addresses
   (Some address)
   false))

(defmacro node-lim-key [node]
  `(arrays/alast (:keys ~node)))

(defmacro node-len [node]
  `(arrays/alength (:keys ~node)))

(defmacro node-keys [node]
  `(:keys ~node))

(defn- restore-child [children weak-children idx storage address]
  (let [restore (:restore storage)]
    (match (restore address)
      (Some child)
      (do
        (arrays/aset children idx (Some child))
        (arrays/aset weak-children idx nil)
        child)
      None
      (Stdlib.failwith "persistent sorted set storage returned no child"))))

(defn node-child [node idx storage]
  (let [children (:children node)
        weak-children (:_weak-children node)
        addresses (:_addresses node)]
    (if-some [child (arrays/aget children idx)]
      child
      (match (arrays/aget weak-children idx)
        (Some reference)
        (match (weak-deref reference)
          (Some child)
          (do
            (arrays/aset children idx (Some child))
            (arrays/aset weak-children idx nil)
            (match storage
              (Some storage-value)
              (match (arrays/aget addresses idx)
                (Some address)
                (let [accessed (:accessed storage-value)]
                  (accessed address))
                None nil)
              None nil)
            child)
          None
          (match storage
            (Some storage-value)
            (match (arrays/aget addresses idx)
              (Some address)
              (restore-child
                children weak-children idx storage-value address)
              None
              (Stdlib.failwith "persistent sorted set child has no address"))
            None
            (Stdlib.failwith "persistent sorted set child requires storage")))
        None
        (match storage
          (Some storage-value)
          (match (arrays/aget addresses idx)
            (Some address)
            (restore-child children weak-children idx storage-value address)
            None
            (Stdlib.failwith "persistent sorted set child has no address"))
          None
          (Stdlib.failwith "persistent sorted set child requires storage"))))))

(defn node-fold [node f initial storage]
  (if (= 0 (node-child-count node))
    (let [keys (:keys node)]
      (loop [idx 0
             result initial]
        (if (= idx (arrays/alength keys))
          result
          (recur
           (inc idx)
           #?(:melange
              (uncurried-call f result (arrays/aget keys idx))
              :default
              (f result (arrays/aget keys idx)))))))
    (loop [idx 0
           result initial]
      (if (= idx (node-child-count node))
        result
        (recur
         (inc idx)
         (node-fold (node-child node idx storage) f result storage))))))

(defn- delete-address [storage address]
  (if-some [storage-value storage]
    (if-some [address-value address]
      (let [delete (:delete storage-value)]
        (delete (arrays/array address-value)))
      nil)
    nil))

(defn- address-option-equals? [candidate address]
  (match candidate
    (Some value) (= value address)
    None false))

(defn- address-present? [addresses address]
  (loop [idx 0]
    (if (= idx (arrays/alength addresses))
      false
      (if (address-option-equals? (arrays/aget addresses idx) address)
        true
        (recur (inc idx))))))

(defn- removed-address? [candidate new-addresses]
  (match candidate
    (Some address)
    (not (address-present? new-addresses address))
    None false))

(defn- address-value [candidate]
  (match candidate
    (Some address) address
    None (Stdlib.failwith "persistent sorted set address is unavailable")))

(defn- removed-addresses [addresses from to new-addresses]
  (arrays/into-array
   (map
    address-value
    (filter
     (fn [candidate] (removed-address? candidate new-addresses))
     (array-to-seq (arrays/aslice addresses from to))))))

(defn- delete-removed-addresses [storage addresses from to new-addresses]
  (if-some [storage-value storage]
    (let [removed
          (removed-addresses addresses from to new-addresses)]
      (when (> (arrays/alength removed) 0)
        (let [delete (:delete storage-value)]
          (delete removed))))
    nil))

(defn- node-merge [node next storage]
  (delete-address storage (deref (:_address next)))
  (new-node-state
   (arrays/aconcat (:keys node) (:keys next))
   (arrays/aconcat
    (:children node) (:children next))
   (arrays/aconcat
    (:_weak-children node) (:_weak-children next))
   (arrays/aconcat
    (:_addresses node) (:_addresses next))
   (deref (:_address node))))

(defn- node-merge-n-split [node next]
  (let [split-keys
        (merge-n-split
         (:keys node) (:keys next))
        split-children
        (merge-n-split
         (:children node) (:children next))
        split-weak-children
        (merge-n-split
         (:_weak-children node) (:_weak-children next))
        split-addresses
        (merge-n-split
         (:_addresses node) (:_addresses next))]
    (arrays/array
     (new-node-state
      (arrays/aget split-keys 0)
      (arrays/aget split-children 0)
      (arrays/aget split-weak-children 0)
      (arrays/aget split-addresses 0)
      (deref (:_address node)))
     (new-node-state
      (arrays/aget split-keys 1)
      (arrays/aget split-children 1)
      (arrays/aget split-weak-children 1)
      (arrays/aget split-addresses 1)
      (deref (:_address next))))))

(defn- rotate [node root? left right storage]
  (cond
    root? (arrays/array node)
    (> (node-len node) min-len)
    (if-some [left-node left]
      (if-some [right-node right]
        (arrays/array left-node node right-node)
        (arrays/array left-node node))
      (if-some [right-node right]
        (arrays/array node right-node)
        (arrays/array node)))
    :else
    (if-some [left-node left]
      (if (<= (node-len left-node) min-len)
        (if-some [right-node right]
          (arrays/array (node-merge left-node node storage) right-node)
          (arrays/array (node-merge left-node node storage)))
        (if-some [right-node right]
          (if (<= (node-len right-node) min-len)
            (arrays/array left-node (node-merge node right-node storage))
            (if (< (node-len left-node) (node-len right-node))
              (let [nodes (node-merge-n-split left-node node)]
                (arrays/array
                 (arrays/aget nodes 0) (arrays/aget nodes 1) right-node))
              (let [nodes (node-merge-n-split node right-node)]
                (arrays/array
                 left-node (arrays/aget nodes 0) (arrays/aget nodes 1)))))
          (node-merge-n-split left-node node)))
      (if-some [right-node right]
        (if (<= (node-len right-node) min-len)
          (arrays/array (node-merge node right-node storage))
          (node-merge-n-split node right-node))
        (arrays/array node)))))

(defn node-lookup [node cmp key storage]
  (loop [node node]
    (let [keys (:keys node)]
      (if (= 0 (node-child-count node))
        (let [idx (lookup-exact cmp keys key)]
          (when-not (= -1 idx) (arrays/aget keys idx)))
        (let [idx (lookup-range cmp keys key)]
          (if (= -1 idx)
            nil
            (recur (node-child node idx storage))))))))

(defn node-conj [node cmp key storage]
  (let [keys (:keys node)
        children (:children node)
        weak-children (:_weak-children node)
        addresses (:_addresses node)
        address (deref (:_address node))]
    (if (= 0 (arrays/alength children))
      (let [idx (binary-search-l cmp keys (dec (arrays/alength keys)) key)
            keys-length (arrays/alength keys)]
        (cond
          (and (< idx keys-length)
               (= 0 #?(:melange
                       (uncurried-compare cmp key (arrays/aget keys idx))
                       :default
                       (cmp key (arrays/aget keys idx)))))
          nil
          (= keys-length max-len)
          (let [middle (arrays/half (inc keys-length))]
            (if (> idx middle)
              (arrays/array
               (new-leaf-address (arrays/aslice keys 0 middle) address)
               (new-leaf
                (cut-n-splice
                 keys middle keys-length idx idx (arrays/array key))))
              (arrays/array
               (new-leaf-address
                (cut-n-splice keys 0 middle idx idx (arrays/array key))
                address)
               (new-leaf (arrays/aslice keys middle keys-length)))))
          :else
          (arrays/array
           (new-leaf-address
            (splice keys idx idx (arrays/array key)) address))))
      (let [idx (binary-search-l cmp keys (- (arrays/alength keys) 2) key)
            child (node-child node idx storage)]
        (if-some [child-nodes (node-conj child cmp key storage)]
          (let [new-keys
                (check-n-splice
                 cmp keys idx (inc idx)
                 (arrays/amap (fn [child] (node-lim-key child)) child-nodes))
                new-children
                (splice
                 children idx (inc idx) (loaded-children child-nodes))
                new-weak-children
                (splice
                 weak-children
                 idx
                 (inc idx)
                 (arrays/make-array (arrays/alength child-nodes) nil))
                new-addresses
                (splice
                 addresses idx (inc idx) (children-addresses child-nodes))]
            (if (<= (arrays/alength new-children) max-len)
              (arrays/array
               (new-node-state
                new-keys new-children new-weak-children new-addresses address))
              (let [middle (arrays/half (arrays/alength new-children))]
                (arrays/array
                 (new-node-state
                  (arrays/aslice new-keys 0 middle)
                  (arrays/aslice new-children 0 middle)
                  (arrays/aslice new-weak-children 0 middle)
                  (arrays/aslice new-addresses 0 middle)
                  address)
                 (new-node-state
                  (arrays/aslice new-keys middle (arrays/alength new-keys))
                  (arrays/aslice
                   new-children middle (arrays/alength new-children))
                  (arrays/aslice
                   new-weak-children
                   middle
                   (arrays/alength new-weak-children))
                  (arrays/aslice
                   new-addresses middle (arrays/alength new-addresses))
                  nil)))))
          nil)))))

(defn node-disj [node cmp key root? left right storage]
  (let [keys (:keys node)
        children (:children node)
        weak-children (:_weak-children node)]
    (if (= 0 (arrays/alength children))
      (let [idx (lookup-exact cmp keys key)]
        (if (= -1 idx)
          nil
          (rotate
           (new-leaf-address
            (splice keys idx (inc idx) (arrays/empty-array))
            (deref (:_address node)))
           root? left right storage)))
      (let [idx (lookup-range cmp keys key)]
        (if (= -1 idx)
          nil
          (let [children-length (arrays/alength children)
                child (node-child node idx storage)
                left-child
                (when (> idx 0) (node-child node (dec idx) storage))
                right-child
                (when (< (inc idx) children-length)
                  (node-child node (inc idx) storage))]
            (if-some [disjoined
                      (node-disj
                       child cmp key false left-child right-child storage)]
              (let [left-idx (if (> idx 0) (dec idx) idx)
                    right-idx
                    (if (< (inc idx) children-length) (+ idx 2) (inc idx))
                    new-keys
                    (check-n-splice
                     cmp keys left-idx right-idx
                     (arrays/amap
                      (fn [child] (node-lim-key child)) disjoined))
                    new-children
                    (splice
                     children left-idx right-idx (loaded-children disjoined))
                    new-weak-children
                    (splice
                     weak-children
                     left-idx
                     right-idx
                     (arrays/make-array (arrays/alength disjoined) nil))
                    new-addresses
                    (splice
                     (:_addresses node)
                     left-idx
                     right-idx
                     (children-addresses disjoined))]
                (when (> right-idx left-idx)
                  (delete-removed-addresses
                   storage
                   (:_addresses node)
                   left-idx
                   right-idx
                   new-addresses))
                (rotate
                 (new-node-state
                  new-keys
                  new-children
                  new-weak-children
                  new-addresses
                  (deref (:_address node)))
                 root? left right storage))
              nil)))))))

(defn node-store [node storage]
  (let [children (:children node)
        weak-children (:_weak-children node)
        addresses (:_addresses node)]
    (loop [idx 0]
      (if (< idx (arrays/alength children))
        (do
          (if-some [child (arrays/aget children idx)]
            (do
              (when (or
                     (nil? (arrays/aget addresses idx))
                     (deref (:_dirty child)))
                (let [child-address (node-store child storage)]
                  (arrays/aset addresses idx (Some child-address)))))
            (if-some [reference (arrays/aget weak-children idx)]
              (if-some [child (weak-deref reference)]
                (do
                  (arrays/aset children idx (Some child))
                  (arrays/aset weak-children idx nil)
                  (when (or
                         (nil? (arrays/aget addresses idx))
                         (deref (:_dirty child)))
                    (let [child-address (node-store child storage)]
                      (arrays/aset addresses idx (Some child-address)))))
                nil)
              nil))
          (recur (inc idx)))
        nil))
    (if (or
         (= true (deref (:_dirty node)))
         (nil? (deref (:_address node))))
      (let [store (:store storage)
            address (store node (deref (:_address node)))]
        (vreset! (:_address node) (Some address))
        (vreset! (:_dirty node) false)
        address)
      (match
       (deref (:_address node))
        (Some address)
        address
        None
        (Stdlib.failwith "stored persistent sorted set node has no address")))))

(def average-len 24)

(type-record btset [value]
             (root :ref<option<tree<value>>>)
             (_weak-root :ref<option<weak<tree<value>>>>)
             (shift :int)
             (cnt :int)
             (comparator :fn<value;value;int>)
             (storage :ref<option<storage<value>>>)
             (_address :ref<option<int64>>))

(defn- make-set [root shift cnt comparator storage]
  (record btset (root (volatile! (Some root))) (_weak-root (volatile! nil)) (shift shift) (cnt cnt) (comparator comparator) (storage (volatile! storage)) (_address (volatile! nil))))

(defn- make-restored-set [shift cnt comparator storage address]
  (record btset (root (volatile! nil)) (_weak-root (volatile! nil)) (shift shift) (cnt cnt) (comparator comparator) (storage (volatile! (Some storage))) (_address (volatile! (Some address)))))

(defn- restore-root [set]
  (match (deref (:storage set))
    (Some storage)
    (match (deref (:_address set))
      (Some address)
      (let [restore (:restore storage)]
        (match (restore address)
          (Some root)
          (do
            (vreset! (:root set) (Some root))
            (vreset! (:_weak-root set) nil)
            root)
          None
          (Stdlib.failwith "persistent sorted set storage returned no root")))
      None
      (Stdlib.failwith "restored persistent sorted set has no address"))
    None
    (Stdlib.failwith "restored persistent sorted set has no storage")))

(defn set-root [set]
  (if-some [root (deref (:root set))]
    root
    (if-some [reference (deref (:_weak-root set))]
      (if-some [root (weak-deref reference)]
        (do
          (vreset! (:root set) (Some root))
          (vreset! (:_weak-root set) nil)
          root)
        (restore-root set))
      (restore-root set))))

(defn set-shift [set]
  (:shift set))

(defn set-count [set]
  (:cnt set))

(defn set-comparator [set]
  (:comparator set))

(defn set-storage [set]
  (deref (:storage set)))

(defn- ^:vector<int64> node-collect-addresses
  [node storage ^:vector<int64> addresses]
  (let [addresses
        (match (node-address node)
          (Some address) (clojure.core/conj addresses address)
          None addresses)]
    (loop [idx 0
           addresses addresses]
      (if (= idx (node-child-count node))
        addresses
        (recur
         (inc idx)
         (node-collect-addresses
          (node-child node idx storage)
          storage
          addresses))))))

(defn ^:vector<int64> set-addresses [set]
  (node-collect-addresses (set-root set) (set-storage set) []))

#?(:native
   (type-record iterator [value]
                (iter-root :tree<value>)
                (iter-shift :int)
                (iter-left :int)
                (iter-right :int)
                (iter-keys :array<value>)
                (iter-idx :int)
                (iter-storage :option<storage<value>>))
   :cljs
   (type-record iterator [value]
                (iter-root :tree<value>)
                (iter-shift :int)
                (iter-left :float)
                (iter-right :float)
                (iter-keys :array<value>)
                (iter-idx :int)
                (iter-storage :option<storage<value>>)))

(defn node-keys-at-path [root current-path shift storage]
  (loop [node root
         level shift]
    (if (pos? level)
      (recur
       (node-child node (path-get current-path level) storage)
       (dec level))
      (node-keys node))))

(defn node-value-at-path [root current-path shift storage]
  (arrays/aget
   (node-keys-at-path root current-path shift storage)
   (path-get current-path 0)))

(defn rightmost-path [root current-path level storage]
  (loop [node root
         current-path current-path
         level level]
    (if (pos? level)
      (let [last-idx (dec (node-child-count node))]
        (recur
         (node-child node last-idx storage)
         (path-set current-path level last-idx)
         (dec level)))
      (path-set
       current-path
       0
       (dec (arrays/alength (node-keys node)))))))

(defn- next-path-inner [node current-path level storage]
  (let [idx (path-get current-path level)]
    (if (pos? level)
      (let [child (node-child node idx storage)]
        (if-some [sub-path
                  (next-path-inner child current-path (dec level) storage)]
          (Some (path-set sub-path level idx))
          (if (< (inc idx) (node-child-count node))
            (Some (path-set empty-path level (inc idx)))
            nil)))
      (if (< (inc idx) (node-len node))
        (Some (path-set empty-path 0 (inc idx)))
        nil))))

(defn next-path [root current-path shift storage]
  (if (path-lt current-path empty-path)
    empty-path
    (if-some [result (next-path-inner root current-path shift storage)]
      result
      (path-inc
       (rightmost-path root empty-path shift storage)))))

(defn- prev-path-inner [node current-path level storage]
  (let [idx (path-get current-path level)]
    (cond
      (and (= 0 level) (= 0 idx))
      nil

      (= 0 level)
      (Some (path-set empty-path 0 (dec idx)))

      (>= idx (node-len node))
      (Some (rightmost-path node current-path level storage))

      :else
      (let [child (node-child node idx storage)]
        (if-some [sub-path
                  (prev-path-inner child current-path (dec level) storage)]
          (Some (path-set sub-path level idx))
          (if (= 0 idx)
            nil
            (let [previous-idx (dec idx)
                  previous-path
                  (rightmost-path
                   (node-child node previous-idx storage)
                   current-path
                   (dec level)
                   storage)]
              (Some (path-set previous-path level previous-idx)))))))))

(defn prev-path [root current-path shift storage]
  (if (> (path-get current-path (inc shift)) 0)
    (rightmost-path root current-path shift storage)
    (if-some [result (prev-path-inner root current-path shift storage)]
      result
      (path-dec empty-path))))

(defn seek-path [root key cmp shift storage]
  (loop [node root
         current-path empty-path
         level shift]
    (let [keys (node-keys node)
          keys-length (arrays/alength keys)]
      (if (= 0 level)
        (let [idx (binary-search-l cmp keys (dec keys-length) key)]
          (if (= idx keys-length)
            nil
            (Some (path-set current-path 0 idx))))
        (let [idx (binary-search-l cmp keys (- keys-length 2) key)]
          (recur
           (node-child node idx storage)
           (path-set current-path level idx)
           (dec level)))))))

(defn rseek-path [root key cmp shift storage]
  (loop [node root
         current-path empty-path
         level shift]
    (let [keys (node-keys node)
          keys-length (arrays/alength keys)]
      (if (= 0 level)
        (path-set
         current-path
         0
         (binary-search-r cmp keys (dec keys-length) key))
        (let [idx (binary-search-r cmp keys (- keys-length 2) key)]
          (recur
           (node-child node idx storage)
           (path-set current-path level idx)
           (dec level)))))))

(defn node-seq-between [root left right shift storage]
  (seq-unfold
   (fn [current]
     (if (path-lt current right)
       (Some
        (tuple
         (node-value-at-path root current shift storage)
         (next-path root current shift storage)))
       nil))
   left))

(defn node-rseq-between [root left right shift storage]
  (let [before-left (prev-path root left shift storage)]
    (seq-unfold
     (fn [current]
       (if (path-lt before-left current)
         (Some
          (tuple
           (node-value-at-path root current shift storage)
           (prev-path root current shift storage)))
         nil))
     (prev-path root right shift storage))))

(defn node-seq [root shift storage]
  (if (= 0 shift)
    (array-to-seq (node-keys root))
    (seq-flat-map
     (fn [idx]
       (node-seq (node-child root idx storage) (dec shift) storage))
     (arrays/into-array (range 0 (node-child-count root))))))

(defn node-rseq [root shift storage]
  (if (= 0 shift)
    (array-to-rseq (node-keys root))
    (seq-flat-map-rev
     (fn [idx]
       (node-rseq (node-child root idx storage) (dec shift) storage))
     (arrays/into-array (range 0 (node-child-count root))))))

(defn- make-iterator [root shift left right keys idx storage]
  (record iterator (iter-root root) (iter-shift shift) (iter-left left) (iter-right right) (iter-keys keys) (iter-idx idx) (iter-storage storage)))

(defn iter-first [iterator]
  (arrays/aget
   (:iter-keys iterator)
   (:iter-idx iterator)))

(defn iter-next [iterator]
  (let [root (:iter-root iterator)
        shift (:iter-shift iterator)
        left (:iter-left iterator)
        right (:iter-right iterator)
        keys (:iter-keys iterator)
        idx (:iter-idx iterator)
        storage (:iter-storage iterator)]
    (if (< (inc idx) (arrays/alength keys))
      (let [next-left (path-inc left)]
        (if (path-lt next-left right)
          (Some
           (make-iterator
            root shift next-left right keys (inc idx) storage))
          nil))
      (let [next-left (next-path root left shift storage)]
        (if (path-lt next-left right)
          (Some
           (make-iterator
            root
            shift
            next-left
            right
            (node-keys-at-path root next-left shift storage)
            (path-get next-left 0)
            storage))
          nil)))))

(defn node-iter [root shift storage]
  (if (= 0 (node-len root))
    nil
    (let [left empty-path
          right
          (next-path
           root
           (rightmost-path root empty-path shift storage)
           shift
           storage)]
      (Some
       (make-iterator
        root
        shift
        left
        right
        (node-keys-at-path root left shift storage)
        0
        storage)))))

(defn- partition-array [values]
  (let [length (arrays/alength values)]
    (loop [offset 0
           parts []]
      (if (= offset length)
        (arrays/into-array parts)
        (let [remaining (- length offset)
              size
              (cond
                (<= remaining max-len) remaining
                (>= remaining (+ average-len min-len)) average-len
                :else (arrays/half remaining))
              next-offset (+ offset size)]
          (recur
           next-offset
           (clojure.core/conj
            parts (arrays/aslice values offset next-offset))))))))

(defn- unique-count [values cmp]
  (let [length (arrays/alength values)]
    (if (= 0 length)
      0
      (loop [idx 1
             previous (arrays/aget values 0)
             count 1]
        (if (= idx length)
          count
          (let [value (arrays/aget values idx)]
            (recur
             (inc idx)
             value
             (if (= 0 #?(:melange (uncurried-compare cmp value previous)
                         :default (cmp value previous)))
               count
               (inc count)))))))))

(defn sorted-array-distinct [values cmp]
  (let [length (arrays/alength values)
        distinct-count (unique-count values cmp)]
    (if (= length distinct-count)
      values
      (let [result
            (arrays/make-array distinct-count (arrays/aget values 0))]
        (when (> distinct-count 0)
          (arrays/aset result 0 (arrays/aget values 0))
          (loop [source-idx 1
                 result-idx 1
                 previous (arrays/aget values 0)]
            (if (< source-idx length)
              (let [value (arrays/aget values source-idx)]
                (if (= 0 #?(:melange (uncurried-compare cmp value previous)
                            :default (cmp value previous)))
                  (recur (inc source-idx) result-idx value)
                  (do
                    (arrays/aset result result-idx value)
                    (recur (inc source-idx) (inc result-idx) value))))
              nil)))
        result))))

(defn from-sorted-array-with-storage [cmp values storage]
  (let [leaves
        (arrays/amap
         (fn [keys] (new-leaf keys))
         (partition-array values))]
    (loop [current-level leaves
           shift 0]
      (let [length (arrays/alength current-level)]
        (cond
          (= 0 length)
          (make-set (new-leaf (arrays/empty-array)) 0 0 cmp storage)

          (= 1 length)
          (make-set
           (arrays/aget current-level 0)
           shift
           (arrays/alength values)
           cmp
           storage)

          :else
          (recur
           (arrays/amap
            (fn [children]
              (new-node
               (arrays/amap
                (fn [child] (node-lim-key child))
                children)
               children))
            (partition-array current-level))
           (inc shift)))))))

(defn from-sorted-array-base [cmp values]
  (from-sorted-array-with-storage cmp values nil))

(defn from-sequential [cmp values]
  (let [sorted (into-array values)]
    (asort! cmp sorted)
    (from-sorted-array-base cmp (sorted-array-distinct sorted cmp))))

(defn empty-set [cmp]
  (make-set (new-leaf (arrays/empty-array)) 0 0 cmp nil))

(defn set-lookup [set key]
  (node-lookup
   (set-root set) (set-comparator set) key (set-storage set)))

(defn set-contains? [set key]
  (let [cmp (set-comparator set)
        storage (set-storage set)]
    (loop [node (set-root set)]
      (let [keys (:keys node)
            length (arrays/alength keys)
            idx (binary-search-l cmp keys (dec length) key)]
        (if (= 0 (node-child-count node))
          (and (< idx length)
               (= 0 #?(:melange
                       (uncurried-compare cmp (arrays/aget keys idx) key)
                       :default
                       (cmp (arrays/aget keys idx) key))))
          (if (= idx length)
            false
            (if-some [child (arrays/aget (:children node) idx)]
              (recur child)
              (recur (node-child node idx storage)))))))))

(defn set-conj-with [set key cmp]
  (if-some [roots
            (node-conj
             (set-root set) cmp key (set-storage set))]
    (if (= 1 (arrays/alength roots))
      (make-set
       (arrays/aget roots 0)
       (set-shift set)
       (inc (set-count set))
       (set-comparator set)
       (set-storage set))
      (make-set
       (new-node
        (arrays/amap
         (fn [root] (node-lim-key root))
         roots)
        roots)
       (inc (set-shift set))
       (inc (set-count set))
       (set-comparator set)
       (set-storage set)))
    set))

(defn set-conj [set key]
  (set-conj-with set key (set-comparator set)))

(defn set-disj-with [set key cmp]
  (if-some [roots
            (node-disj
             (set-root set)
             cmp
             key
             true
             nil
             nil
             (set-storage set))]
    (let [root (arrays/aget roots 0)]
      (if (and (> (set-shift set) 0)
               (= 1 (node-len root)))
        (make-set
         (node-child root 0 (set-storage set))
         (dec (set-shift set))
         (dec (set-count set))
         (set-comparator set)
         (set-storage set))
        (make-set
         root
         (set-shift set)
         (dec (set-count set))
         (set-comparator set)
         (set-storage set))))
    set))

(defn set-disj [set key]
  (set-disj-with set key (set-comparator set)))

(defn set-seq [set]
  (node-seq
   (set-root set) (set-shift set) (set-storage set)))

(defn set-rseq [set]
  (node-rseq
   (set-root set) (set-shift set) (set-storage set)))

(defn set-iter [set]
  (node-iter
   (set-root set) (set-shift set) (set-storage set)))

(defn set-reduce [set f initial]
  (node-fold (set-root set) f initial (set-storage set)))

(defn- set-slice-bounds [set key-from key-to cmp]
  (if-some [left
            (seek-path
             (set-root set)
             key-from
             cmp
             (set-shift set)
             (set-storage set))]
    (let [right
          (rseek-path
           (set-root set)
           key-to
           cmp
           (set-shift set)
           (set-storage set))]
      (if (path-lt left right)
        (Some (tuple left right))
        nil))
    nil))

(defn set-slice-with [set key-from key-to cmp]
  (if-some [bounds (set-slice-bounds set key-from key-to cmp)]
    (match bounds
      (tuple left right)
      (Some
       (node-seq-between
        (set-root set) left right (set-shift set) (set-storage set))))
    nil))

(defn set-slice [set key-from key-to]
  (set-slice-with set key-from key-to (set-comparator set)))

(defn set-rslice-with [set key-from key-to cmp]
  (if-some [bounds (set-slice-bounds set key-to key-from cmp)]
    (match bounds
      (tuple left right)
      (Some
       (node-rseq-between
        (set-root set) left right (set-shift set) (set-storage set))))
    nil))

(defn set-rslice [set key-from key-to]
  (set-rslice-with set key-from key-to (set-comparator set)))

(defn restore-by [cmp address storage shift count]
  (make-restored-set shift count cmp storage address))

(defn store [set storage]
  (when (nil? (set-storage set))
    (vreset! (:storage set) (Some storage)))
  (let [root (set-root set)
        storage-value
        (match (set-storage set)
          (Some value) value
          None (Stdlib.failwith "persistent sorted set storage is unavailable"))
        address (node-store root storage-value)]
    (vreset! (:_address set) (Some address))
    (vreset! (:_weak-root set) nil)
    (vreset! (:root set) (Some root))
    address))

(extend-type btset Seqable
             (-seq [set] (set-seq set)))

(extend-type btset Counted
             (-count [set] (set-count set)))

(defn- set-equiv [set ^:dynamic other]
  (and
   (instance? btset other)
   (let [other-set (__lg_dynamic-narrow set other)]
     (and
      (= (set-count set) (set-count other-set))
      (loop [left (seq set)
             right (seq other-set)]
        (cond
          (empty? left) (empty? right)
          (empty? right) false
          :else
          (if-some [left-value (first left)]
            (if-some [right-value (first right)]
              (if
               (zero? ((set-comparator set) left-value right-value))
                (recur (rest left) (rest right))
                false)
              false)
            false)))))))

(extend-type btset IEquiv
             (-equiv [set ^:dynamic other] (set-equiv set other)))

(extend-type btset Emptyable
             (-empty [set] (empty-set (set-comparator set))))

(extend-type btset IReversible
             (-rseq [set] (set-rseq set)))

(extend-type btset IEditableCollection
             (-as-transient [set] set))

(extend-type btset ITransientCollection
             (-conj! [set key] (set-conj set key))
             (-persistent! [set] set))

(extend-type btset ITransientSet
             (-disjoin! [set key] (set-disj set key)))

(extend-type btset Reducible
             (-reduce [set f initial] (set-reduce set f initial)))

(defn conj [set key cmp]
  (set-conj-with set key cmp))

(defn disj [set key cmp]
  (set-disj-with set key cmp))

(defn slice
  ([set key-from key-to]
   (slice set key-from key-to (set-comparator set)))
  ([set key-from key-to cmp]
   (set-slice-with set key-from key-to cmp)))

(defn rslice
  ([set key-from key-to]
   (rslice set key-from key-to (set-comparator set)))
  ([set key-from key-to cmp]
   (set-rslice-with set key-from key-to cmp)))

(defn seek
  ([values target]
   (seek values target (fn [left right] (compare left right))))
  ([values target cmp]
   (filter
    #(not
      (neg?
       #?(:melange (uncurried-compare cmp % target)
          :default (cmp % target))))
    values)))

(defn comparator [set]
  (set-comparator set))

(defn- with-storage [set storage]
  (do
    (vreset! (:storage set) (Some storage))
    set))

(defn from-sorted-array
  ([cmp values]
   (from-sorted-array cmp values (arrays/alength values)))
  ([cmp values length]
   (from-sorted-array-base cmp (arrays/aslice values 0 length)))
  ([cmp values length opts]
   (let [set (from-sorted-array-base cmp (arrays/aslice values 0 length))]
     (if-some [storage (:storage opts)]
       (with-storage set storage)
       set))))

(defn sorted-set-with-comparator [cmp opts]
  (let [set (empty-set cmp)]
    (if-some [storage (:storage opts)]
      (with-storage set storage)
      set)))

(defn sorted-set* [opts]
  (sorted-set-with-comparator (:cmp opts) opts))

(defn settings [_]
  (assoc {}
         :branching-factor 32
         :ref-type :strong))
