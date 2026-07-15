(ns me.tonsky.persistent-sorted-set.set
  (:refer-clojure :exclude [contains?])
  (:require
    [me.tonsky.persistent-sorted-set.arrays :as arrays]
    [me.tonsky.persistent-sorted-set.nodes :as nodes]
    [me.tonsky.persistent-sorted-set.path :as path]
    [me.tonsky.persistent-sorted-set.traversal :as traversal]))

(def average-len 24)

(type-record btset [value]
  (root :ref<option<tree<value>>>)
  (shift :int)
  (cnt :int)
  (comparator :fn<value;value;int>)
  (storage :ref<option<storage<value>>>)
  (_address :ref<option<int64>>))

(defn- make-set [root shift cnt comparator storage]
  (record btset (root (volatile! (Some root))) (shift shift) (cnt cnt) (comparator comparator) (storage (volatile! storage)) (_address (volatile! nil))))

(defn- make-restored-set [shift cnt comparator storage address]
  (record btset (root (volatile! nil)) (shift shift) (cnt cnt) (comparator comparator) (storage (volatile! (Some storage))) (_address (volatile! (Some address)))))

(defn set-root [set]
  (if-some [root (deref (:root set))]
    root
    (match (deref (:storage set))
      (Some storage)
      (match (deref (:_address set))
        (Some address)
        (let [restore (:restore storage)]
          (match
            (restore address)
            (Some root)
            (do
              (vreset! (:root set) (Some root))
              root)
            None
            (Stdlib.failwith "persistent sorted set storage returned no root")))
        None
        (Stdlib.failwith "restored persistent sorted set has no address"))
      None
      (Stdlib.failwith "restored persistent sorted set has no storage"))))

(defn set-shift [set]
  (:shift set))

(defn set-count [set]
  (:cnt set))

(defn set-comparator [set]
  (:comparator set))

(defn set-storage [set]
  (deref (:storage set)))

(defn- partition-array [values]
  (let [length (arrays/alength values)]
    (loop [offset 0
           parts []]
      (if (= offset length)
        (arrays/into-array parts)
        (let [remaining (- length offset)
              size
              (cond
                (<= remaining nodes/max-len) remaining
                (>= remaining (+ average-len nodes/min-len)) average-len
                :else (arrays/half remaining))
              next-offset (+ offset size)]
          (recur
            next-offset
            (conj parts (arrays/aslice values offset next-offset))))))))

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
              (if (= 0 #?(:melange
                           (uncurried-compare cmp value previous)
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
                (if (= 0 #?(:melange
                             (uncurried-compare cmp value previous)
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
          (fn [keys] (nodes/new-leaf keys))
          (partition-array values))]
    (loop [current-level leaves
           shift 0]
      (let [length (arrays/alength current-level)]
        (cond
          (= 0 length)
          (make-set (nodes/new-leaf (arrays/empty-array)) 0 0 cmp storage)

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
                (nodes/new-node
                  (arrays/amap
                    (fn [child] (nodes/node-lim-key child))
                    children)
                  children))
              (partition-array current-level))
            (inc shift)))))))

(defn from-sorted-array [cmp values]
  (from-sorted-array-with-storage cmp values nil))

(defn from-sequential [cmp values]
  (let [sorted (into-array values)]
    (asort! cmp sorted)
    (from-sorted-array cmp (sorted-array-distinct sorted cmp))))

(defn empty-set [cmp]
  (make-set (nodes/new-leaf (arrays/empty-array)) 0 0 cmp nil))

(defn empty-set-with-storage [cmp storage]
  (make-set (nodes/new-leaf (arrays/empty-array)) 0 0 cmp storage))

(defn with-storage [set storage]
  (do
    (vreset! (:storage set) (Some storage))
    set))

(defn set-lookup [set key]
  (nodes/node-lookup
    (set-root set) (set-comparator set) key (set-storage set)))

(defn set-contains? [set key]
  (not (nil? (set-lookup set key))))

(defn set-conj-with [set key cmp]
  (if-some [roots
            (nodes/node-conj
              (set-root set) cmp key (set-storage set))]
    (if (= 1 (arrays/alength roots))
      (make-set
        (arrays/aget roots 0)
        (set-shift set)
        (inc (set-count set))
        (set-comparator set)
        (set-storage set))
      (make-set
        (nodes/new-node
          (arrays/amap
            (fn [root] (nodes/node-lim-key root))
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
            (nodes/node-disj
              (set-root set)
              cmp
              key
              true
              nil
              nil
              (set-storage set))]
    (let [root (arrays/aget roots 0)]
      (if (and (> (set-shift set) 0)
               (= 1 (nodes/node-len root)))
        (make-set
          (nodes/node-child root 0 (set-storage set))
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
  (traversal/node-seq
    (set-root set) (set-shift set) (set-storage set)))

(defn set-rseq [set]
  (traversal/node-rseq
    (set-root set) (set-shift set) (set-storage set)))

(defn set-iter [set]
  (traversal/node-iter
    (set-root set) (set-shift set) (set-storage set)))

(defn set-reduce [set f initial]
  (nodes/node-fold (set-root set) f initial (set-storage set)))

(defn- set-slice-bounds [set key-from key-to cmp]
  (if-some [left
            (traversal/seek-path
              (set-root set)
              key-from
              cmp
              (set-shift set)
              (set-storage set))]
    (let [right
          (traversal/rseek-path
            (set-root set)
            key-to
            cmp
            (set-shift set)
            (set-storage set))]
      (if (path/path-lt left right)
        (Some (tuple left right))
        nil))
    nil))

(defn set-slice-with [set key-from key-to cmp]
  (if-some [bounds (set-slice-bounds set key-from key-to cmp)]
    (match bounds
      (tuple left right)
      (Some
        (traversal/node-seq-between
          (set-root set) left right (set-shift set) (set-storage set))))
    nil))

(defn set-slice [set key-from key-to]
  (set-slice-with set key-from key-to (set-comparator set)))

(defn set-rslice-with [set key-from key-to cmp]
  (if-some [bounds (set-slice-bounds set key-from key-to cmp)]
    (match bounds
      (tuple left right)
      (Some
        (traversal/node-rseq-between
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
        address (nodes/node-store root storage-value)]
    (vreset! (:_address set) (Some address))
    address))

(extend-type btset Seqable
  (-seq [set] (set-seq set)))

(extend-type btset Counted
  (-count [set] (set-count set)))

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
