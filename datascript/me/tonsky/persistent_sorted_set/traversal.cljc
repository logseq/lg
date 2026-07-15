(ns me.tonsky.persistent-sorted-set.traversal
  (:require
    [me.tonsky.persistent-sorted-set.arrays :as arrays]
    [me.tonsky.persistent-sorted-set.nodes :as nodes]
    [me.tonsky.persistent-sorted-set.path :as path]
    [me.tonsky.persistent-sorted-set.search :as search]))

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
        (nodes/node-child node (path/path-get current-path level) storage)
        (dec level))
      (nodes/node-keys node))))

(defn node-value-at-path [root current-path shift storage]
  (arrays/aget
    (node-keys-at-path root current-path shift storage)
    (path/path-get current-path 0)))

(defn rightmost-path [root current-path level storage]
  (loop [node root
         current-path current-path
         level level]
    (if (pos? level)
      (let [last-idx (dec (nodes/node-child-count node))]
        (recur
          (nodes/node-child node last-idx storage)
          (path/path-set current-path level last-idx)
          (dec level)))
      (path/path-set
        current-path
        0
        (dec (arrays/alength (nodes/node-keys node)))))))

(defn- next-path-inner [node current-path level storage]
  (let [idx (path/path-get current-path level)]
    (if (pos? level)
      (let [child (nodes/node-child node idx storage)]
        (if-some [sub-path
                  (next-path-inner child current-path (dec level) storage)]
          (Some (path/path-set sub-path level idx))
          (if (< (inc idx) (nodes/node-child-count node))
            (Some (path/path-set path/empty-path level (inc idx)))
            nil)))
      (if (< (inc idx) (nodes/node-len node))
        (Some (path/path-set path/empty-path 0 (inc idx)))
        nil))))

(defn next-path [root current-path shift storage]
  (if (path/path-lt current-path path/empty-path)
    path/empty-path
    (if-some [result (next-path-inner root current-path shift storage)]
      result
      (path/path-inc
        (rightmost-path root path/empty-path shift storage)))))

(defn- prev-path-inner [node current-path level storage]
  (let [idx (path/path-get current-path level)]
    (cond
      (and (= 0 level) (= 0 idx))
      nil

      (= 0 level)
      (Some (path/path-set path/empty-path 0 (dec idx)))

      (>= idx (nodes/node-len node))
      (Some (rightmost-path node current-path level storage))

      :else
      (let [child (nodes/node-child node idx storage)]
        (if-some [sub-path
                  (prev-path-inner child current-path (dec level) storage)]
          (Some (path/path-set sub-path level idx))
          (if (= 0 idx)
            nil
            (let [previous-idx (dec idx)
                  previous-path
                  (rightmost-path
                    (nodes/node-child node previous-idx storage)
                    current-path
                    (dec level)
                    storage)]
              (Some (path/path-set previous-path level previous-idx)))))))))

(defn prev-path [root current-path shift storage]
  (if (> (path/path-get current-path (inc shift)) 0)
    (rightmost-path root current-path shift storage)
    (if-some [result (prev-path-inner root current-path shift storage)]
      result
      (path/path-dec path/empty-path))))

(defn seek-path [root key cmp shift storage]
  (loop [node root
         current-path path/empty-path
         level shift]
    (let [keys (nodes/node-keys node)
          keys-length (arrays/alength keys)]
      (if (= 0 level)
        (let [idx (search/binary-search-l cmp keys (dec keys-length) key)]
          (if (= idx keys-length)
            nil
            (Some (path/path-set current-path 0 idx))))
        (let [idx (search/binary-search-l cmp keys (- keys-length 2) key)]
          (recur
            (nodes/node-child node idx storage)
            (path/path-set current-path level idx)
            (dec level)))))))

(defn rseek-path [root key cmp shift storage]
  (loop [node root
         current-path path/empty-path
         level shift]
    (let [keys (nodes/node-keys node)
          keys-length (arrays/alength keys)]
      (if (= 0 level)
        (path/path-set
          current-path
          0
          (search/binary-search-r cmp keys (dec keys-length) key))
        (let [idx (search/binary-search-r cmp keys (- keys-length 2) key)]
          (recur
            (nodes/node-child node idx storage)
            (path/path-set current-path level idx)
            (dec level)))))))

(defn node-seq-between [root left right shift storage]
  (seq-unfold
    (fn [current]
      (if (path/path-lt current right)
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
        (if (path/path-lt before-left current)
          (Some
            (tuple
              (node-value-at-path root current shift storage)
              (prev-path root current shift storage)))
          nil))
      (prev-path root right shift storage))))

(defn node-seq [root shift storage]
  (if (= 0 shift)
    (array-to-seq (nodes/node-keys root))
    (seq-flat-map
      (fn [idx]
        (node-seq (nodes/node-child root idx storage) (dec shift) storage))
      (arrays/into-array (range 0 (nodes/node-child-count root))))))

(defn node-rseq [root shift storage]
  (if (= 0 shift)
    (array-to-rseq (nodes/node-keys root))
    (seq-flat-map-rev
      (fn [idx]
        (node-rseq (nodes/node-child root idx storage) (dec shift) storage))
      (arrays/into-array (range 0 (nodes/node-child-count root))))))

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
      (let [next-left (path/path-inc left)]
        (if (path/path-lt next-left right)
          (Some
            (make-iterator
              root shift next-left right keys (inc idx) storage))
          nil))
      (let [next-left (next-path root left shift storage)]
        (if (path/path-lt next-left right)
          (Some
            (make-iterator
              root
              shift
              next-left
              right
              (node-keys-at-path root next-left shift storage)
              (path/path-get next-left 0)
              storage))
          nil)))))

(defn node-iter [root shift storage]
  (if (= 0 (nodes/node-len root))
    nil
    (let [left path/empty-path
          right
          (next-path
            root
            (rightmost-path root path/empty-path shift storage)
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
