(ns me.tonsky.persistent-sorted-set.benchmark
  (:require
    [me.tonsky.persistent-sorted-set.arrays :as arrays]
    [me.tonsky.persistent-sorted-set.array-ops :as ops]
    [me.tonsky.persistent-sorted-set.nodes :as nodes]
    [me.tonsky.persistent-sorted-set.search :as search]
    [me.tonsky.persistent-sorted-set.set :as pset]))

(defn int-compare [left right]
  (- left right))

(defn now []
  (Sys.time))

(defmacro benchmark [name iterations expression]
  `(do
     (loop [warmup# 0]
       (if (< warmup# 10)
         (do
           ~expression
           (recur (inc warmup#)))
         nil))
     (let [started# (now)]
       (loop [iteration# 0]
         (if (< iteration# ~iterations)
           (do
             ~expression
             (recur (inc iteration#)))
           nil))
       (let [elapsed# (Float.sub (now) started#)
             millis#
             (Float.div
               (Float.mul elapsed# 1000.0)
               (double ~iterations))]
         (println (str ~name ":" (Float.to_string millis#)))))))

(def ints-10k
  (arrays/amap
    (fn [idx] (mod (* idx 7919) 10000))
    (arrays/into-array (range 0 10000))))

(def ints-300k
  (arrays/amap
    (fn [idx] (mod (* idx 7919) 300000))
    (arrays/into-array (range 0 300000))))

(defn insert-all [values]
  (loop [set (pset/empty-set int-compare)
         idx 0]
    (if (= idx (arrays/alength values))
      set
      (recur
        (pset/set-conj set (arrays/aget values idx))
        (inc idx)))))

(def set-10k (insert-all ints-10k))
(def set-300k (insert-all ints-300k))

(def ints-32 (arrays/into-array (range 0 32)))
(def leaf-32 (nodes/new-leaf ints-32))
(def leaves-32
  (arrays/amap (fn [idx] (nodes/new-leaf (arrays/array idx))) ints-32))
(def leaves-2 (arrays/aslice leaves-32 0 2))
(def loaded-leaves-32
  (arrays/amap (fn [child] (Some child)) leaves-32))
(def loaded-leaves-2 (arrays/aslice loaded-leaves-32 0 2))
(def empty-addresses-32
  (arrays/amap (fn [child] (nodes/node-address child)) leaves-32))
(def empty-addresses-2 (arrays/aslice empty-addresses-32 0 2))

(defn profile-search-10k []
  (loop [idx 0
         result 0]
    (if (= idx 10000)
      result
      (recur
        (inc idx)
        (+ result
           (search/binary-search-l int-compare ints-32 31 (mod idx 33)))))))

(defn profile-splice-10k []
  (loop [idx 0
         result ints-32]
    (if (= idx 10000)
      result
      (let [position (mod idx 33)]
        (recur
          (inc idx)
          (ops/splice ints-32 position position (arrays/array idx)))))))

(defn profile-make-array-10k []
  (loop [idx 0
         result (Array.make 33 0)]
    (if (= idx 10000)
      result
      (recur (inc idx) (Array.make 33 idx)))))

(defn profile-wrap-children-10k []
  (loop [idx 0
         result (arrays/amap (fn [child] (Some child)) leaves-32)]
    (if (= idx 10000)
      result
      (recur
        (inc idx)
        (arrays/amap (fn [child] (Some child)) leaves-32)))))

(defn profile-wrap-two-children-10k []
  (loop [idx 0
         result loaded-leaves-2]
    (if (= idx 10000)
      result
      (recur
        (inc idx)
        (arrays/amap (fn [child] (Some child)) leaves-2)))))

(defn profile-splice-children-10k []
  (loop [idx 0
         result loaded-leaves-32]
    (if (= idx 10000)
      result
      (let [position (mod idx 32)]
        (recur
          (inc idx)
          (ops/splice
            loaded-leaves-32 position (inc position) loaded-leaves-2))))))

(defn profile-new-leaf-10k []
  (loop [idx 0
         result leaf-32]
    (if (= idx 10000)
      result
      (recur (inc idx) (nodes/new-leaf ints-32)))))

(defn profile-splice-addresses-10k []
  (loop [idx 0
         result empty-addresses-32]
    (if (= idx 10000)
      result
      (let [position (mod idx 32)]
        (recur
          (inc idx)
          (ops/splice
            empty-addresses-32 position (inc position) empty-addresses-2))))))

(defn profile-full-leaf-conj-10k []
  (loop [idx 0
         result nil]
    (if (= idx 10000)
      result
      (recur
        (inc idx)
        (nodes/node-conj leaf-32 int-compare (+ idx 32) nil)))))

(defn conj-10k []
  (insert-all ints-10k))

(defn disj-10k []
  (loop [set set-10k
         idx 0]
    (if (= idx (arrays/alength ints-10k))
      set
      (recur
        (pset/set-disj set (arrays/aget ints-10k idx))
        (inc idx)))))

(defn contains-10k []
  (loop [idx 0
         found 0]
    (if (= idx (arrays/alength ints-10k))
      found
      (recur
        (inc idx)
        (if (pset/set-contains? set-10k (arrays/aget ints-10k idx))
          (inc found)
          found)))))

(defn sum-iterator [iterator result]
  (if-some [next-iterator
            (me.tonsky.persistent-sorted-set.traversal/iter-next iterator)]
    (sum-iterator
      next-iterator
      (+ result
         (me.tonsky.persistent-sorted-set.traversal/iter-first iterator)))
    (+ result
       (me.tonsky.persistent-sorted-set.traversal/iter-first iterator))))

(defn next-300k []
  (if-some [iterator (pset/set-iter set-300k)]
    (sum-iterator iterator 0)
    0))

(defn doseq-300k []
  (let [result (atom 0)]
    (doseq [value set-300k]
      (reset! result (+ (deref result) value)))
    (deref result)))

(defn reduce-300k []
  (pset/set-reduce set-300k (fn [left right] (+ left right)) 0))

(benchmark "conj-10K" 100 (conj-10k))
(benchmark "disj-10K" 50 (disj-10k))
(benchmark "contains-10K" 200 (contains-10k))
(benchmark "doseq-300K" 100 (doseq-300k))
(benchmark "next-300K" 100 (next-300k))
(benchmark "reduce-300K" 100 (reduce-300k))
(benchmark "profile/search-32x10K" 100 (profile-search-10k))
(benchmark "profile/splice-32x10K" 50 (profile-splice-10k))
(benchmark "profile/make-33x10K" 100 (profile-make-array-10k))
(benchmark "profile/wrap-children-32x10K" 50 (profile-wrap-children-10k))
(benchmark "profile/wrap-children-2x10K" 100 (profile-wrap-two-children-10k))
(benchmark "profile/splice-children-10K" 50 (profile-splice-children-10k))
(benchmark "profile/splice-addresses-10K" 50 (profile-splice-addresses-10k))
(benchmark "profile/new-leaf-10K" 100 (profile-new-leaf-10k))
(benchmark "profile/full-leaf-conj-10K" 50 (profile-full-leaf-conj-10k))
