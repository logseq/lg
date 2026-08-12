(ns me.tonsky.persistent-sorted-set
  (:refer-clojure :exclude [conj disj contains?])
  (:require
    [me.tonsky.persistent-sorted-set.arrays :as arrays]
    [ocaml.Stdlib :as stdlib]))

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
  [path level]
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
  [path level idx]
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

(defn path-inc [path]
  #?(:native (inc path) :cljs (Float.add path 1.0)))

(defn path-dec [path]
  #?(:native (dec path) :cljs (Float.sub path 1.0)))

(defn path-cmp
  [path1 path2]
  #?(:native (- path1 path2) :cljs (Float.sub path1 path2)))

(defn path-lt
  [path1 path2]
  #?(:native (< path1 path2)
     :cljs (< (Float.sub path1 path2) 0.0)))

(defn path-lte
  [path1 path2]
  #?(:native (<= path1 path2)
     :cljs (<= (Float.sub path1 path2) 0.0)))

(defn path-eq
  [path1 path2]
  #?(:native (= path1 path2)
     :cljs (= (Float.sub path1 path2) 0.0)))

(defn path-same-leaf
  [path1 path2]
  #?(:native
     (if (and (< path1 max-safe-path) (< path2 max-safe-path))
       (= (bit-shift-right path1 bits-per-level)
          (bit-shift-right path2 bits-per-level))
       (= (quot path1 max-len)
          (quot path2 max-len)))
     :cljs
     (= (Float.floor (/ path1 32.0))
        (Float.floor (/ path2 32.0)))))

(defn path-str [path]
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
  #?(:melange
     (array-binary-search-left cmp arr right key)
     :default
     (loop [left 0
            right right]
       (if (<= left right)
         (let [middle (arrays/half (+ left right))
               middle-key (arrays/aget arr middle)]
           (if (neg? (uncurried-compare cmp middle-key key))
             (recur (inc middle) right)
             (recur left (dec middle))))
         left))))

(defn binary-search-r [cmp arr right key]
  #?(:melange
     (array-binary-search-right cmp arr right key)
     :default
     (loop [left 0
            right right]
       (if (<= left right)
         (let [middle (arrays/half (+ left right))
               middle-key (arrays/aget arr middle)]
           (if (pos? (uncurried-compare cmp middle-key key))
             (recur left (dec middle))
             (recur (inc middle) right)))
         left))))

(defn lookup-exact [cmp arr key]
  (let [length (arrays/alength arr)
        idx (binary-search-l cmp arr (dec length) key)]
    (if (and (< idx length)
             (= 0 (uncurried-compare cmp (arrays/aget arr idx) key)))
      idx
      #?(:melange -1.0 :default -1))))

(defn lookup-range [cmp arr key]
  (let [length (arrays/alength arr)
        idx (binary-search-l cmp arr (dec length) key)]
    (if (= idx length)
      #?(:melange -1.0 :default -1)
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

(defn cut-n-splice-one [arr cut-from cut-to splice-from splice-to value]
  (let [left-length (- splice-from cut-from)
        right-length (- cut-to splice-to)
        value-end (inc left-length)
        result-length (+ value-end right-length)]
    (if (= 0 result-length)
      (arrays/empty-array)
      (let [result (Array.make result-length value)]
        (arrays/acopy arr cut-from splice-from result 0)
        (arrays/aset result left-length value)
        (arrays/acopy arr splice-to cut-to result value-end)
        result))))

(defn splice [arr splice-from splice-to values]
  (cut-n-splice arr 0 (arrays/alength arr) splice-from splice-to values))

(defn insert [arr idx values]
  (cut-n-splice arr 0 (arrays/alength arr) idx idx values))

(defn insert-one [arr idx value]
  (cut-n-splice-one arr 0 (arrays/alength arr) idx idx value))

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

(defn eq-arr
  [cmp left left-from left-to right right-from right-to]
  (let [length (- left-to left-from)]
    (and
     (= length (- right-to right-from))
     (loop [idx 0]
       (cond
         (= idx length)
         true

         (not (= 0
                 (uncurried-compare cmp
                  (arrays/aget left (+ idx left-from))
                  (arrays/aget right (+ idx right-from)))))
         false

         :else
         (recur (inc idx)))))))

(defn check-n-splice [cmp arr from to new-arr]
  (if (eq-arr cmp arr from to new-arr 0 (arrays/alength new-arr))
    arr
    (splice arr from to new-arr)))

(defn make-storage [restore accessed store delete]
  (record storage
          (restore restore)
          (accessed accessed)
          (store store)
          (delete delete)
          (owner nil)
          (pending-deletes (volatile! (arrays/empty-array)))
          (drain-writes (fn [_] []))
          (ref-type (Lg_runtime.Runtime_ref_type.Weak))))

(defn make-storage-with-owner
  [restore accessed store delete owner pending-deletes drain-writes]
  (record storage
          (restore restore)
          (accessed accessed)
          (store store)
          (delete delete)
          (owner (Some owner))
          (pending-deletes pending-deletes)
          (drain-writes drain-writes)
          (ref-type (Lg_runtime.Runtime_ref_type.Weak))))

(defn storage-with-ref-type
  [storage ref-type]
  (record storage
          (restore (:restore storage))
          (accessed (:accessed storage))
          (store (:store storage))
          (delete (:delete storage))
          (owner (:owner storage))
          (pending-deletes (:pending-deletes storage))
          (drain-writes (:drain-writes storage))
          (ref-type ref-type)))

(defn storage-owner [storage]
  (:owner storage))

(defn storage-pending-deletes [storage]
  (:pending-deletes storage))

(defn storage-drain-writes [storage]
  ((:drain-writes storage) (Stdlib.ignore 0)))

(defn- make-tree
  [keys children weak-children addresses address dirty]
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
   keys (arrays/empty-array) (arrays/empty-array) (arrays/empty-array) None true))

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
   None
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

(defn- retain-child [children weak-children idx storage child]
  (match (:ref-type storage)
    Lg_runtime.Runtime_ref_type.Strong
    (do
      (arrays/aset children idx (Some child))
      (arrays/aset weak-children idx nil)
      child)
    Lg_runtime.Runtime_ref_type.Weak
    (do
      (arrays/aset children idx nil)
      (arrays/aset weak-children idx (Some (weak-ref child)))
      child)))

(defn- restore-child [children weak-children idx storage address]
  (let [restore (:restore storage)]
    (match (restore address)
      (Some child)
      (retain-child children weak-children idx storage child)
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

(defn- address-option-equals?
  [candidate address]
  (match candidate
    (Some value) (= value address)
    None false))

(defn- address-present?
  [addresses address]
  (loop [idx 0]
    (if (= idx (arrays/alength addresses))
      false
      (if (address-option-equals? (arrays/aget addresses idx) address)
        true
        (recur (inc idx))))))

(defn- removed-address?
  [candidate new-addresses]
  (match candidate
    (Some address)
    (not (address-present? new-addresses address))
    None false))

(defn- address-value [candidate]
  (match candidate
    (Some address) address
    None (Stdlib.failwith "persistent sorted set address is unavailable")))

(defn- removed-addresses
  [addresses
   from
   to
   new-addresses]
  (arrays/into-array
   (map
    address-value
    (filter
     (fn [candidate] (removed-address? candidate new-addresses))
     (array-to-seq (arrays/aslice addresses from to))))))

(defn- delete-removed-addresses
  [storage addresses from to new-addresses]
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
   (arrays/aconcat (.-keys node) (.-keys next))
   (arrays/aconcat
    (.-children node) (.-children next))
   (arrays/aconcat
    (.-_weak-children node) (.-_weak-children next))
   (arrays/aconcat
    (.-_addresses node) (.-_addresses next))
   (deref (:_address node))))

(defn- node-merge-n-split [node next]
  (let [split-keys
        (merge-n-split
         (.-keys node) (.-keys next))
        split-children
        (merge-n-split
         (.-children node) (.-children next))
        split-weak-children
        (merge-n-split
         (.-_weak-children node) (.-_weak-children next))
        split-addresses
        (merge-n-split
         (.-_addresses node) (.-_addresses next))]
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
          (if (= -1 idx)
            None
            (Some (arrays/aget keys idx))))
        (let [idx (lookup-range cmp keys key)]
          (if (= -1 idx)
            None
            (recur
             (node-child
              node #?(:melange (int idx) :default idx) storage))))))))

(defn node-conj [node cmp key storage]
  (let [keys (:keys node)
        children (:children node)
        weak-children (:_weak-children node)
        addresses (:_addresses node)
        address (deref (:_address node))]
    (if (= 0 (arrays/alength children))
      (let [idx (binary-search-l cmp keys (dec (arrays/alength keys)) key)
            idx-int #?(:melange (int idx) :default idx)
            keys-length (arrays/alength keys)]
        (cond
          (and (< idx keys-length)
              (= 0 (uncurried-compare cmp key (arrays/aget keys idx))))
          nil
          (= keys-length max-len)
          (let [middle (arrays/half (inc keys-length))]
            (if (> idx middle)
              (arrays/array
               (new-leaf-address (arrays/aslice keys 0 middle) address)
               (new-leaf
                (cut-n-splice-one
                 keys middle keys-length idx-int idx-int key)))
              (arrays/array
               (new-leaf-address
                (cut-n-splice-one
                 keys 0 middle idx-int idx-int key)
                address)
               (new-leaf (arrays/aslice keys middle keys-length)))))
          :else
          (arrays/array
           (new-leaf-address
            (insert-one keys idx-int key) address))))
      (let [idx (binary-search-l cmp keys (- (arrays/alength keys) 2) key)
            idx-int #?(:melange (int idx) :default idx)
            child (node-child node idx-int storage)]
        (if-some [child-nodes (node-conj child cmp key storage)]
          (let [new-keys
                (check-n-splice
                 cmp keys idx-int (inc idx-int)
                 (arrays/amap (fn [child] (node-lim-key child)) child-nodes))
                new-children
                (splice
                 children idx-int (inc idx-int) (loaded-children child-nodes))
                new-weak-children
                (splice
                 weak-children
                 idx-int
                 (inc idx-int)
                 (arrays/make-array (arrays/alength child-nodes) nil))
                new-addresses
                (splice
                 addresses idx-int (inc idx-int)
                 (children-addresses child-nodes))]
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

(defn node-disj
  [node cmp key root? left right storage]
  (let [keys (:keys node)
        children (:children node)
        weak-children (:_weak-children node)]
    (if (= 0 (arrays/alength children))
      (let [idx (lookup-exact cmp keys key)
            idx-int #?(:melange (int idx) :default idx)]
        (if (= -1 idx)
          nil
          (rotate
           (new-leaf-address
            (splice keys idx-int (inc idx-int) (arrays/empty-array))
            (deref (:_address node)))
           root? left right storage)))
      (let [idx (lookup-range cmp keys key)
            idx-int #?(:melange (int idx) :default idx)]
        (if (= -1 idx)
          nil
          (let [children-length (arrays/alength children)
                child (node-child node idx-int storage)
                left-child
                (when (> idx 0)
                  (node-child node (dec idx-int) storage))
                right-child
                (when (< (inc idx-int) children-length)
                  (node-child node (inc idx-int) storage))]
            (if-some [disjoined
                      (node-disj
                       child cmp key false left-child right-child storage)]
              (let [left-idx (if (> idx 0) (dec idx-int) idx-int)
                    right-idx
                    (if (< (inc idx-int) children-length)
                      (+ idx-int 2)
                      (inc idx-int))
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
                   (arrays/aset addresses idx (Some child-address))))
               (retain-child children weak-children idx storage child))
             (if-some [reference (arrays/aget weak-children idx)]
               (if-some [child (weak-deref reference)]
                 (do
                   (when (or
                          (nil? (arrays/aget addresses idx))
                          (deref (:_dirty child)))
                     (let [child-address (node-store child storage)]
                       (arrays/aset addresses idx (Some child-address))))
                   (retain-child children weak-children idx storage child))
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

(def avg-len 24)

(defn- make-set [root shift cnt comparator storage]
  (record btset
          (root (volatile! (Some root)))
          (_weak-root (volatile! nil))
          (ref-type
           (volatile! (Lg_runtime.Runtime_ref_type.Weak)))
          (branching-factor (volatile! 32))
          (shift (volatile! shift))
          (cnt (volatile! cnt))
          (comparator comparator)
          (storage (volatile! storage))
          (_address (volatile! nil))))

(defn- make-restored-set [shift cnt comparator storage address]
  (record btset
          (root (volatile! nil))
          (_weak-root (volatile! nil))
          (ref-type
           (volatile! (Lg_runtime.Runtime_ref_type.Weak)))
          (branching-factor (volatile! 32))
          (shift (volatile! shift))
          (cnt (volatile! cnt))
          (comparator comparator)
          (storage (volatile! (Some storage)))
          (_address (volatile! (Some address)))))

(defn set-ref-type [set]
  (deref (:ref-type set)))

(defn with-ref-type [set ref-type]
  (vreset! (:ref-type set) ref-type)
  (match (deref (:storage set))
    (Some storage)
    (vreset!
     (:storage set)
     (Some (storage-with-ref-type storage ref-type)))
    None nil)
  set)

(defn with-branching-factor [set branching-factor]
  (when (< branching-factor 2)
    (Stdlib.invalid_arg "Branching factor must be at least 2"))
  (vreset! (:branching-factor set) branching-factor)
  set)

(defn- inherit-set-settings [source target]
  (with-branching-factor
   (with-ref-type target (set-ref-type source))
   @(:branching-factor source)))

(defn- retain-root [set root]
  (match (set-ref-type set)
    Lg_runtime.Runtime_ref_type.Weak
    (do
      (vreset! (:_weak-root set) (Some (weak-ref root)))
      (vreset! (:root set) nil)
      root)
    Lg_runtime.Runtime_ref_type.Strong
    (do
      (vreset! (:root set) (Some root))
      (vreset! (:_weak-root set) nil)
      root)))

(defn- restore-root [set]
  (match (deref (:storage set))
    (Some storage)
    (match (deref (:_address set))
      (Some address)
      (let [restore (:restore storage)]
        (match (restore address)
          (Some root)
          (do
            (retain-root set root))
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
        root
        (restore-root set))
      (restore-root set))))

(defn set-shift [set]
  (let [known (deref (:shift set))]
    (if (>= known 0)
      known
      (let [derived
            (loop [node (set-root set)
                   level 0]
              (if (= 0 (node-child-count node))
                level
                (recur
                 (node-child node 0 (set-storage set))
                 (inc level))))]
        (vreset! (:shift set) derived)
        derived))))

(defn set-count [set]
  (let [known (deref (:cnt set))]
    (if (>= known 0)
      known
      (let [derived
            (node-fold
             (set-root set)
             (fn [count _value] (inc count))
             0
             (set-storage set))]
        (vreset! (:cnt set) derived)
        derived))))

(defn set-comparator [set]
  (:comparator set))

(defn set-storage [set]
  (deref (:storage set)))

(defn- keys-for [set path]
  (loop [level (set-shift set)
         node (set-root set)]
    (if (pos? level)
      (recur
       (dec level)
       (node-child node (path-get path level) (set-storage set)))
      (node-keys node))))

(defn alter-btset [set root shift cnt]
  (inherit-set-settings
   set
   (make-set
    root shift cnt (set-comparator set) (set-storage set))))

(defn- node-collect-addresses
  [node storage addresses]
  (let [addresses
        (match (node-address node)
          (Some address) (Rrbvec.push_back addresses address)
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

(defn set-addresses [set]
  (node-collect-addresses (set-root set) (set-storage set) []))

(defn walk-addresses [set consume-fn]
  (doseq [address (set-addresses set)]
    (consume-fn address)))

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

(defn -rpath [root current-path level storage]
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

(defn- -next-path [node current-path level storage]
  (let [idx (path-get current-path level)]
    (if (pos? level)
      (let [child (node-child node idx storage)]
        (if-some [sub-path
                  (-next-path child current-path (dec level) storage)]
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
    (if-some [result (-next-path root current-path shift storage)]
      result
      (path-inc
       (-rpath root empty-path shift storage)))))

(defn- -prev-path [node current-path level storage]
  (let [idx (path-get current-path level)]
    (cond
      (and (= 0 level) (= 0 idx))
      nil

      (= 0 level)
      (Some (path-set empty-path 0 (dec idx)))

      (>= idx (node-len node))
      (Some (-rpath node current-path level storage))

      :else
      (let [child (node-child node idx storage)]
        (if-some [sub-path
                  (-prev-path child current-path (dec level) storage)]
          (Some (path-set sub-path level idx))
          (if (= 0 idx)
            nil
            (let [previous-idx (dec idx)
                  previous-path
                  (-rpath
                   (node-child node previous-idx storage)
                   current-path
                   (dec level)
                   storage)]
              (Some (path-set previous-path level previous-idx)))))))))

(defn prev-path [root current-path shift storage]
  (if (> (path-get current-path (inc shift)) 0)
    (-rpath root current-path shift storage)
    (if-some [result (-prev-path root current-path shift storage)]
      result
      (path-dec empty-path))))

(defn -seek* [root key cmp shift storage]
  (loop [node root
         current-path empty-path
         level shift]
    (let [keys (node-keys node)
          keys-length (arrays/alength keys)]
      (if (= 0 level)
        (let [idx (binary-search-l cmp keys (dec keys-length) key)]
          (if (= idx keys-length)
            nil
            (Some
             (path-set
              current-path 0 #?(:melange (int idx) :default idx)))))
        (let [idx (binary-search-l cmp keys (- keys-length 2) key)
              idx-int #?(:melange (int idx) :default idx)]
          (recur
           (node-child node idx-int storage)
           (path-set current-path level idx-int)
           (dec level)))))))

(defn -rseek* [root key cmp shift storage]
  (loop [node root
         current-path empty-path
         level shift]
    (let [keys (node-keys node)
          keys-length (arrays/alength keys)]
      (if (= 0 level)
        (path-set
         current-path
         0
         #?(:melange
            (int (binary-search-r cmp keys (dec keys-length) key))
            :default (binary-search-r cmp keys (dec keys-length) key)))
        (let [idx (binary-search-r cmp keys (- keys-length 2) key)
              idx-int #?(:melange (int idx) :default idx)]
          (recur
           (node-child node idx-int storage)
            (path-set current-path level idx-int)
            (dec level)))))))

(defn rightmost-path [root current-path level storage]
  (-rpath root current-path level storage))

(defn seek-path [root key cmp shift storage]
  (-seek* root key cmp shift storage))

(defn rseek-path [root key cmp shift storage]
  (-rseek* root key cmp shift storage))

(defn slice-bounds-with-keys [root key-from key-to cmp shift storage]
  (if-some [left (-seek* root key-from cmp shift storage)]
    (let [right (-rseek* root key-to cmp shift storage)]
      (if (path-lt left right)
        (Some
         (tuple
          left
          right
          (node-keys-at-path root left shift storage)))
        nil))
    nil))

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

(defn- make-iterator [set left right keys idx]
  (record iterator
          (iter-set set)
          (iter-left left)
          (iter-right right)
          (iter-keys keys)
          (iter-idx idx)))

(defn- btset-iter [set left right]
  (make-iterator
   set
   left
   right
   (keys-for set left)
   (path-get left 0)))

(defn iter [set left right]
  (btset-iter set left right))

(defn riter [set left right]
  (node-rseq-between
   (set-root set) left right (set-shift set) (set-storage set)))

(defn- -distance [set node left right level]
  (let [idx-left (path-get left level)
        idx-right (path-get right level)]
    (if (pos? level)
      (if (= idx-left idx-right)
        (-distance
         set
         (node-child node idx-left (set-storage set))
         left
         right
         (dec level))
        (loop [level level
               result (- idx-right idx-left)]
          (if (= 0 level)
            result
            (recur (dec level) (* result avg-len)))))
      (- idx-right idx-left))))

(defn- distance [set left right]
  (cond
    (path-eq left right) 0
    (path-eq (path-inc left) right) 1
    (path-eq
     (next-path
      (set-root set) left (set-shift set) (set-storage set))
     right)
    1
    :else
    (-distance set (set-root set) left right (set-shift set))))

(defn est-count [iterator]
  (distance
   (:iter-set iterator)
   (:iter-left iterator)
   (:iter-right iterator)))

(defn iter-first [iterator]
  (arrays/aget
   (:iter-keys iterator)
   (:iter-idx iterator)))

(defn iter-next [iterator]
  (let [set (:iter-set iterator)
        root (set-root set)
        shift (set-shift set)
        left (:iter-left iterator)
        right (:iter-right iterator)
        keys (:iter-keys iterator)
        idx (:iter-idx iterator)
        storage (set-storage set)]
    (if (< (inc idx) (arrays/alength keys))
      (let [next-left (path-inc left)]
        (if (path-lt next-left right)
          (Some
           (make-iterator
            set next-left right keys (inc idx)))
          nil))
      (let [next-left (next-path root left shift storage)]
        (if (path-lt next-left right)
          (Some
           (make-iterator
            set
            next-left
            right
            (node-keys-at-path root next-left shift storage)
            (path-get next-left 0)))
          nil)))))

(defn- iterator-option-seq [iterator]
  (seq-unfold-chunks
   (fn [state]
     (match state
       (Some value)
       (let [set (:iter-set value)
             root (set-root set)
             shift (set-shift set)
             left (:iter-left value)
             right (:iter-right value)
             keys (:iter-keys value)
             idx (:iter-idx value)
             storage (set-storage set)
             end-idx
             (if (path-same-leaf left right)
               (path-get right 0)
               (arrays/alength keys))]
         (Some
          (tuple
           keys
           idx
           end-idx
           (fn []
             (if (< end-idx (arrays/alength keys))
               nil
               (let [last
                     (path-set left 0 (dec (arrays/alength keys)))
                     next-left
                     (next-path root last shift storage)]
                 (if (path-lt next-left right)
                   (Some
                    (make-iterator
                     set
                     next-left
                     right
                     (node-keys-at-path root next-left shift storage)
                     (path-get next-left 0)))
                   nil)))))))
       None nil))
   iterator))

(defn iterator-seq [iterator]
  (iterator-option-seq (Some iterator)))

(defn node-iter [set]
  (let [root (set-root set)
        shift (set-shift set)
        storage (set-storage set)]
    (if (= 0 (node-len root))
    nil
    (let [left empty-path
          right
          (next-path
           root
           (-rpath root empty-path shift storage)
           shift
           storage)]
      (Some
       (make-iterator
        set
        left
        right
        (node-keys-at-path root left shift storage)
        0))))))

(defn arr-map-inplace [f values]
  (let [length (arrays/alength values)]
    (loop [idx 0]
      (if (< idx length)
        (do
          (arrays/aset values idx (f (arrays/aget values idx)))
          (recur (inc idx)))
        nil))
    values))

(defn- partition-size [remaining]
  (cond
    (<= remaining max-len) remaining
    (>= remaining (+ avg-len min-len)) avg-len
    :else (arrays/half remaining)))

(defn- partition-count [length]
  (loop [offset 0
         count 0]
    (if (= offset length)
      count
      (recur
       (+ offset (partition-size (- length offset)))
       (inc count)))))

(defn- partition-array [values]
  (let [length (arrays/alength values)
        parts (arrays/make-array
               (partition-count length)
               (arrays/empty-array))]
    (loop [offset 0
           part-index 0]
      (if (= offset length)
        parts
        (let [next-offset
              (+ offset (partition-size (- length offset)))]
          (arrays/aset
           parts
           part-index
           (arrays/aslice values offset next-offset))
          (recur next-offset (inc part-index)))))))

(defn arr-partition-approx [minimum maximum values]
  (let [length (arrays/alength values)
        parts (arrays/make-array
               (partition-count length)
               (arrays/empty-array))]
    (loop [offset 0
           part-index 0]
      (if (= offset length)
        parts
        (let [remaining (- length offset)
              part-size
              (cond
                (<= remaining maximum) remaining
                (>= remaining (+ avg-len minimum)) avg-len
                :else (arrays/half remaining))
              next-offset (+ offset part-size)]
          (arrays/aset
           parts
           part-index
           (arrays/aslice values offset next-offset))
          (recur next-offset (inc part-index)))))))

(defn- partition-indexed-array [values indexes]
  (let [length (arrays/alength indexes)
        parts (arrays/make-array
               (partition-count length)
               (arrays/empty-array))]
    (loop [offset 0
           part-index 0]
      (if (= offset length)
        parts
        (let [part-size (partition-size (- length offset))
              part (arrays/make-array
                    part-size
                    (arrays/aget
                     values (arrays/aget indexes offset)))]
          (loop [part-offset 0]
            (if (= part-offset part-size)
              nil
              (do
                (arrays/aset
                 part
                 part-offset
                 (arrays/aget
                  values
                  (arrays/aget indexes (+ offset part-offset))))
                (recur (inc part-offset)))))
          (arrays/aset parts part-index part)
          (recur (+ offset part-size) (inc part-index)))))))

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
             (if (zero? (uncurried-compare cmp value previous))
               count
               (inc count)))))))))

(defn- sorted-arr-distinct? [values cmp]
  (= (arrays/alength values) (unique-count values cmp)))

(defn sorted-arr-distinct [values cmp]
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
                (if (zero? (uncurried-compare cmp value previous))
                  (recur (inc source-idx) result-idx value)
                  (do
                    (arrays/aset result result-idx value)
                    (recur (inc source-idx) (inc result-idx) value))))
              nil)))
        result))))

(defn sorted-array-distinct [values cmp]
  (sorted-arr-distinct values cmp))

(defn- from-sorted-partitions-with-storage
  [cmp value-count partitions storage]
  (let [leaves (arrays/amap (fn [keys] (new-leaf keys)) partitions)]
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
           value-count
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
            (arr-partition-approx min-len max-len current-level))
           (inc shift)))))))

(defn from-sorted-array-with-storage [cmp values storage]
  (from-sorted-partitions-with-storage
   cmp
   (arrays/alength values)
   (arr-partition-approx min-len max-len values)
   storage))

(defn from-sorted-array-base [cmp values]
  (from-sorted-array-with-storage cmp values None))

(defn from-sequential [cmp values]
  (let [sorted (into-array values)]
    (asort! cmp sorted)
    (from-sorted-array-base cmp (sorted-arr-distinct sorted cmp))))

(defn empty-set [cmp]
  (make-set
   (new-leaf (arrays/empty-array)) 0 0 cmp None))

(defn sorted-set-by
  ([cmp]
   (empty-set (as-ordering cmp)))
  ([cmp & keys]
   (from-sequential (as-ordering cmp) keys)))

(defn sorted-set
  ([]
   (sorted-set-by (fn [left right] (stdlib/compare left right))))
  ([& keys]
   (from-sequential
    (as-ordering (fn [left right] (stdlib/compare left right)))
    keys)))

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
               (= 0 (uncurried-compare cmp (arrays/aget keys idx) key)))
          (if (= idx length)
            false
            (if-some [child (arrays/aget (:children node) idx)]
              (recur child)
              (recur
               (node-child
                node #?(:melange (int idx) :default idx) storage)))))))))

(defn set-conj-with [set key cmp]
  (if-some [roots
            (node-conj
             (set-root set) cmp key (set-storage set))]
    (if (= 1 (arrays/alength roots))
       (alter-btset
        set
        (arrays/aget roots 0)
        (set-shift set)
        (inc (set-count set)))
       (alter-btset
        set
        (new-node
         (arrays/amap
          (fn [root] (node-lim-key root))
          roots)
         roots)
        (inc (set-shift set))
        (inc (set-count set))))
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
        (alter-btset
         set
         (node-child root 0 (set-storage set))
         (dec (set-shift set))
         (dec (set-count set)))
        (alter-btset
         set
         root
         (set-shift set)
         (dec (set-count set)))))
    set))

(defn set-disj [set key]
  (set-disj-with set key (set-comparator set)))

(defn set-seq [set]
  (iterator-option-seq
   (node-iter set)))

(defn set-rseq [set]
  (node-rseq
   (set-root set) (set-shift set) (set-storage set)))

(defn set-iter [set]
  (node-iter set))

(defn set-reduce [set f initial]
  (node-fold (set-root set) f initial (set-storage set)))

(defn seek-first [set target cmp]
  (if-some [path
              (-seek*
               (set-root set)
               target
               cmp
               (set-shift set)
               (set-storage set))]
      (Some
       (node-value-at-path
        (set-root set)
        path
        (set-shift set)
        (set-storage set)))
    nil))

(defn- set-slice-bounds [set key-from key-to cmp]
  (if-some [left
            (-seek*
             (set-root set)
             key-from
             cmp
             (set-shift set)
             (set-storage set))]
    (let [right
          (-rseek*
           (set-root set)
           key-to
           cmp
           (set-shift set)
           (set-storage set))]
      (if (path-lt left right)
        (Some (tuple left right))
        nil))
    nil))

(defn -slice [set key-from key-to cmp]
  (let [root (set-root set)
        shift (set-shift set)
        storage (set-storage set)]
    (if-some [bounds
              (slice-bounds-with-keys
               root key-from key-to cmp shift storage)]
      (match bounds
        (tuple left right keys)
        (Some
         (iterator-seq
          (iter set left right))))
      nil)))

(defn set-slice [set key-from key-to]
  (-slice set key-from key-to (set-comparator set)))

(defn set-slice-reduce-with
  [set key-from key-to cmp f initial]
  (let [root (set-root set)
        shift (set-shift set)
        storage (set-storage set)]
    (if-some [bounds
              (slice-bounds-with-keys
               root key-from key-to cmp shift storage)]
      (match bounds
        (tuple left right first-keys)
        (loop [left left
               keys first-keys
               index (path-get left 0)
               result initial]
          (if (path-lt left right)
            (let [result
                  (uncurried-call f result (arrays/aget keys index))
                  next-index (inc index)]
              (if (< next-index (arrays/alength keys))
                (recur
                 (path-inc left)
                 keys
                 next-index
                 result)
                (let [next-left
                      (next-path root left shift storage)]
                  (if (path-lt next-left right)
                    (recur
                     next-left
                     (node-keys-at-path
                      root next-left shift storage)
                     (path-get next-left 0)
                     result)
                    result))))
            result)))
      initial)))

(defn set-rslice-with [set key-from key-to cmp]
  (if-some [bounds (set-slice-bounds set key-to key-from cmp)]
    (match bounds
      (tuple left right)
      (Some
       (riter set left right)))
    nil))

(defn set-rslice [set key-from key-to]
  (set-rslice-with set key-from key-to (set-comparator set)))

(defn restore-by
  ([cmp address storage shift count]
   (restore-by
    cmp address storage shift count
    (Lg_runtime.Runtime_ref_type.Weak)))
  ([cmp address storage shift count
    ref-type]
   (with-ref-type
    (make-restored-set shift count cmp storage address)
    ref-type))
  ([cmp address storage shift count
    ref-type
    branching-factor]
   (with-branching-factor
    (with-ref-type
     (make-restored-set shift count cmp storage address)
     ref-type)
    branching-factor)))

(defn restore
  ([address storage]
   (restore-by
    (as-ordering (fn [left right] (stdlib/compare left right)))
    address
    storage
    -1
    -1))
  ([address storage opts]
   (restore-by
    (as-ordering (fn [left right] (stdlib/compare left right)))
    address
    storage
    -1
    -1
    (:ref-type opts)
    (:branching-factor opts))))

(defn store [set storage]
  (when (nil? (set-storage set))
    (vreset!
     (:storage set)
     (Some (storage-with-ref-type storage (set-ref-type set)))))
  (let [root (set-root set)
        storage-value
        (match (set-storage set)
          (Some value) value
          None (Stdlib.failwith "persistent sorted set storage is unavailable"))
        address (node-store root storage-value)]
    (vreset! (:_address set) (Some address))
    (retain-root set root)
    address))

(extend-type btset Seqable
             (-seq [set] (set-seq set)))

(extend-type btset Counted
             (-count [set] (set-count set)))

(defn- set-equiv [set other]
  (and
   (= (set-count set) (set-count other))
   (loop [left (seq set)
          right (seq other)]
     (cond
       (empty? left) (empty? right)
       (empty? right) false
       :else
       (if-some [left-value (first left)]
         (if-some [right-value (first right)]
           (if (= 0 ((set-comparator set)
                     left-value
                     right-value))
             (recur (rest left) (rest right))
             false)
           false)
         false)))))

(extend-type btset IEquiv
             (-equiv [set other] (set-equiv set other)))

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
   (-slice set key-from key-to (set-comparator set)))
  ([set key-from key-to cmp]
   (-slice set key-from key-to (as-ordering cmp))))

(defn rslice
  ([set key-from key-to]
   (set-rslice-with set key-from key-to (set-comparator set)))
  ([set key-from key-to cmp]
   (set-rslice-with set key-from key-to (as-ordering cmp))))

(defn seek
  ([values target]
   (seek values target (fn [left right] (stdlib/compare left right))))
  ([values target cmp]
   (let [cmp (as-ordering cmp)]
     (filter
      #(not
        (neg?
         (uncurried-compare cmp % target)))
      values))))

(defn comparator [set]
  (set-comparator set))

(defn- sorted-prefix [values length]
  (if (= length (arrays/alength values))
    values
    (arrays/aslice values 0 length)))

(defn- with-storage [set storage]
  (do
    (vreset!
     (:storage set)
     (Some (storage-with-ref-type storage (set-ref-type set))))
    set))

(defn from-sorted-array
  ([cmp values]
   (from-sorted-array cmp values (arrays/alength values)))
  ([cmp values length]
   (from-sorted-array-base
    (as-ordering cmp) (sorted-prefix values length)))
  ([cmp values length storage]
   (let [set
         (from-sorted-array-base
          (as-ordering cmp) (sorted-prefix values length))]
     (if-some [storage storage]
       (with-storage set storage)
       set)))
  ([cmp values length storage ref-type]
   (with-ref-type
    (from-sorted-array cmp values length storage)
    ref-type)))

(defn from-sorted-indexed-array [cmp values indexes ref-type]
  (with-ref-type
   (from-sorted-partitions-with-storage
    (as-ordering cmp)
    (arrays/alength indexes)
    (partition-indexed-array values indexes)
    None)
   ref-type))

(defn sorted-set-with-comparator [cmp storage]
  (let [set (empty-set cmp)]
    (if-some [storage storage]
      (with-storage set storage)
      set)))

(defn settings [set]
  (record set-settings
          (branching-factor @(:branching-factor set))
          (ref-type (set-ref-type set))))
