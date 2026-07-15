(ns lg.benchmark.persistent-sorted-set
  (:require
    [cljs.nodejs :as nodejs]
    [me.tonsky.persistent-sorted-set :as pset]))

(nodejs/enable-util-print!)

(defn int-compare [left right]
  (- left right))

(defn now-millis []
  (.now js/performance))

(defn benchmark [name iterations f]
  (loop [warmup 0]
    (when (< warmup 10)
      (f)
      (recur (inc warmup))))
  (let [started (now-millis)]
    (loop [iteration 0]
      (when (< iteration iterations)
        (f)
        (recur (inc iteration))))
    (println
      (str name ":" (/ (- (now-millis) started) iterations)))))

(def ints-10k
  (into-array (map #(mod (* % 7919) 10000) (range 0 10000))))

(def ints-300k
  (into-array (map #(mod (* % 7919) 300000) (range 0 300000))))

(defn insert-all [values]
  (loop [set (pset/sorted-set-by int-compare)
         idx 0]
    (if (= idx (alength values))
      set
      (recur
        (pset/conj set (aget values idx) int-compare)
        (inc idx)))))

(def set-10k (insert-all ints-10k))
(def set-300k (insert-all ints-300k))

(defn conj-10k []
  (insert-all ints-10k))

(defn disj-10k []
  (loop [set set-10k
         idx 0]
    (if (= idx (alength ints-10k))
      set
      (recur
        (pset/disj set (aget ints-10k idx) int-compare)
        (inc idx)))))

(defn contains-10k []
  (loop [idx 0
         found 0]
    (if (= idx (alength ints-10k))
      found
      (recur
        (inc idx)
        (if (contains? set-10k (aget ints-10k idx))
          (inc found)
          found)))))

(defn doseq-300k []
  (let [result (volatile! 0)]
    (doseq [value set-300k]
      (vswap! result + value))
    @result))

(defn next-300k []
  (loop [values (seq set-300k)
         result 0]
    (if (nil? values)
      result
      (recur (next values) (+ result (first values))))))

(defn reduce-300k []
  (reduce + 0 set-300k))

(defn -main []
  (benchmark "conj-10K" 100 conj-10k)
  (benchmark "disj-10K" 50 disj-10k)
  (benchmark "contains-10K" 200 contains-10k)
  (benchmark "doseq-300K" 100 doseq-300k)
  (benchmark "next-300K" 100 next-300k)
  (benchmark "reduce-300K" 100 reduce-300k))

(-main)
