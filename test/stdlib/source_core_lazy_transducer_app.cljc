(ns source-core-lazy-transducer-app
  (:require [cljs.math :as math]
            [clojure.core :as core]
            [clojure.core.protocols :as protocols]
            [ocaml.Buffer :as buffer]
            [ocaml.Lg_runtime.Runtime_reduced :as runtime-reduced]))

(def lazy-realizations (atom 0))
(def delayed-values
  (lazy-seq
    (swap! lazy-realizations inc)
    (list 1 2 3)))

(signature source-core-lazy-transducer-app/append-int
  :overload<fn<vector<int>>;fn<vector<int>;vector<int>>;fn<vector<int>;int;vector<int>>>)
(defn append-int
  ([] [])
  ([result] result)
  ([result input] (conj result input)))

(signature source-core-lazy-transducer-app/append-reduced
  :overload<fn<vector<int>>;fn<vector<int>;vector<int>>;fn<vector<int>;int;__lg_maybe_reduced_callback_result<vector<int>>>>)
(defn append-reduced
  ([] [])
  ([result] result)
  ([result input]
   (runtime-reduced/continue (conj result input))))

(signature source-core-lazy-transducer-app/sum-int
  :overload<fn<int>;fn<int;int>;fn<int;int;int>>)
(defn sum-int
  ([] 0)
  ([result] result)
  ([result input] (+ result input)))

(signature source-core-lazy-transducer-app/increment-even
  :fn<overload<fn<int>;fn<int;int>;fn<int;int;__lg_maybe_reduced_callback_result<int>>>;overload<fn<int>;fn<int;int>;fn<int;int;__lg_maybe_reduced_callback_result<int>>>>)
(defn increment-even [rf]
  ((map inc) ((filter even?) rf)))

(println (= 0 @lazy-realizations))
(println (= 1 (first delayed-values)))
(println (= 1 (first delayed-values)))
(println (= 1 @lazy-realizations))

(println (= [2 4] (vec (filter even? [1 2 3 4]))))
(println (= [1 3] (vec (remove even? [1 2 3 4]))))
(println (= [1 2] (vec (take 2 [1 2 3 4]))))
(println (= [3 4] (vec (drop 2 [1 2 3 4]))))
(println (= [1 2] (vec (take-while #(< % 3) [1 2 3 1]))))
(println (= [3 1] (vec (drop-while #(< % 3) [1 2 3 1]))))
(println (= [10 21 32] (vec (map-indexed #(+ (* %1 10) %2) [10 11 12]))))
(println (= [2 4] (vec (keep #(if (even? %) % nil) [1 2 3 4]))))
(println (= [1 -1 2 -2] (vec (mapcat #(list % (- %)) [1 2]))))

(signature source-core-lazy-transducer-app/cat-reducer
  :overload<fn<vector<int>>;fn<vector<int>;vector<int>>;fn<vector<int>;seqable<int>;__lg_maybe_reduced_callback_result<vector<int>>>>)
(def cat-reducer (cat append-reduced))
(def cat-first (cat-reducer [] [1 2]))
(def cat-second
  (cat-reducer (runtime-reduced/unreduced cat-first) [3 4]))
(println (= [1 2 3 4] (runtime-reduced/unreduced cat-second)))
(println (= 6 (transduce (map inc) sum-int [0 1 2])))
(println (= 6 (transduce increment-even sum-int 0 [0 1 2 3 4])))
(println (= 3 (transduce (halt-when #(= % 3)) sum-int 0 [1 2 3 4])))
(println
  (= 9
     (transduce
       (halt-when #(= % 3) (fn [result input] (* result input)))
       sum-int
       0
       [1 2 3 4])))

(def sequence-realizations (atom 0))
(def transformed-values
  (sequence
    (map (fn [value]
           (swap! sequence-realizations inc)
           (inc value)))
    [1 2 3]))
(println (= 0 @sequence-realizations))
(println (= 2 (first transformed-values)))
(println (= 1 @sequence-realizations))
(println (= [2 3 4] (vec transformed-values)))
(println (= 3 @sequence-realizations))

(println (= [1 2 3] (vec (lazy-cat [1] (list 2 3)))))
(println (= [7 7 7] (vec (repeatedly 3 (fn [] 7)))))
(println
  (= [10 32]
     (vec (keep-indexed
            (fn [index value]
              (if (even? index) (Some (+ index value)) None))
            [10 20 30 40]))))
(println
  (= [10 32]
     (vec (sequence
            (keep-indexed
              (fn [index value]
                (if (even? index) (Some (+ index value)) None)))
            [10 20 30 40]))))
(println (= [0 2 4] (vec (take-nth 2 [0 1 2 3 4 5]))))
(println (= [0 2 4] (vec (sequence (take-nth 2) [0 1 2 3 4 5]))))
(println (= [1 2 3] (vec (random-sample 1.0 [1 2 3]))))
(println (= [] (vec (sequence (random-sample 0.0) [1 2 3]))))
(def lazy-partitions (partition-all 2 [1 2 3]))
(println
  (and (= [1 2] (vec (first lazy-partitions)))
       (= [3] (vec (second lazy-partitions)))))
(println (= [[1 2] [3]] (vec (sequence (partition-all 2) [1 2 3]))))
(println (= [[1 2] [3]] (vec (partitionv-all 2 [1 2 3]))))
(println (= [[1 2] [3]] (vec (sequence (partitionv-all 2) [1 2 3]))))
(println (= [9 9 9 9] (vec (take 4 (repeat 9)))))
(println (= [8 8 8] (vec (repeat 3 8))))
(println (= [1 2 1 2 1] (vec (take 5 (cycle [1 2])))))
(println (= [2 4] (filterv even? [1 2 3 4])))
(def filterv-predicate-calls (atom 0))
(def filterv-once
  (filterv
    (fn [value]
      (swap! filterv-predicate-calls inc)
      (even? value))
    [1 2 3]))
(println (= [2] filterv-once))
(println (= 3 @filterv-predicate-calls))
(def default-partitions (partition 2 [1 2 3 4 5]))
(println
  (and (= [1 2] (vec (first default-partitions)))
       (= [3 4] (vec (second default-partitions)))
       (= 2 (count default-partitions))))
(def stepped-partitions (partition 2 2 [1 2 3 4 5]))
(println
  (and (= [1 2] (vec (first stepped-partitions)))
       (= [3 4] (vec (second stepped-partitions)))
       (= 2 (count stepped-partitions))))
(def padded-partitions (partition 2 2 [0] [1 2 3]))
(println
  (and (= [1 2] (vec (first padded-partitions)))
       (= [3 0] (vec (second padded-partitions)))))
(def dorun-calls (atom 0))
(def dorun-values
  (map
    (fn [value]
      (swap! dorun-calls inc)
      value)
    [1 2 3]))
(dorun 2 dorun-values)
(println (= 3 @dorun-calls))
(dorun dorun-values)
(println (= 3 @dorun-calls))
(def run-values (atom []))
(def run-result
  (run!
    (fn [value]
      (swap! run-values conj value))
    [1 2 3]))
(println (nil? run-result))
(println (= [1 2 3] @run-values))
(println
  (= {1 ["a" "c"] 2 ["bb"]}
     (group-by (fn [value] (count value)) ["a" "bb" "c"])))
(println
  (= [[1 3] [2 4] [5]]
     (mapv vec (partition-by odd? [1 3 2 4 5]))))
(println
  (= [[1 3] [2 4] [5]]
     (into [] (partition-by odd?) [1 3 2 4 5])))
(println (= [1 2 3] (vec (sort [3 1 2]))))
(println
  (= [3 2 1]
     (vec (sort (comparator (fn [left right] (> left right))) [1 3 2]))))
(println
  (= ["a" "bb" "ccc"]
     (vec (sort-by (fn [value] (count value)) ["ccc" "a" "bb"]))))
(println
  (= ["ccc" "bb" "a"]
     (vec
      (sort-by (fn [value] (count value))
               (comparator (fn [left right] (> left right)))
               ["a" "ccc" "bb"]))))
(defrecord SortEntry [^:int rank])
(println
  (= [1 2 3]
     (mapv (fn [^SortEntry entry] (:rank entry))
           (sort-by (fn [^SortEntry entry] (:rank entry))
                    [(SortEntry. 3) (SortEntry. 1) (SortEntry. 2)]))))
(println
  (= [1 3 6]
     (vec (reductions (fn [left right] (+ left right)) [1 2 3]))))
(println
  (= [10 11 13]
     (vec (reductions (fn [left right] (+ left right)) 10 [1 2]))))
(println
  (= 31
     (reduce-kv (fn [result index value]
                  (+ result (+ index value)))
                0
                [10 20])))
(println
  (= 6
     (reduce-kv (fn [result _key value] (+ result value))
                0
                {:a 1 :b 2 :c 3})))
(println (= 6 (reduce (fn [left right] (+ left right)) [1 2 3])))
(println (= 16 (reduce (fn [result value] (+ result value)) 10 [1 2 3])))
(println
  (= 3
     (reduce (fn [result value]
               (if (= value 3) (reduced result) (+ result value)))
             0
             [1 2 3 4])))
(println (= 0.0 (math/sin 0.0)))
(println (= 1.0 (math/cos 0.0)))
(println (= 0.0 (math/tan 0.0)))
(println (= 0.0 (math/asin 0.0)))
(println (= 0.0 (math/acos 1.0)))
(println (= 0.0 (math/atan 0.0)))
(println (= math/PI (math/to-radians 180.0)))
(println (= 180.0 (math/to-degrees math/PI)))
(println (= 1.0 (math/exp 0.0)))
(println (= 0.0 (math/log 1.0)))
(println (= 2.0 (math/log10 100.0)))
(println (= 3.0 (math/sqrt 9.0)))
(println (= 2.0 (math/ceil 1.5)))
(println (= 1.0 (math/floor 1.5)))
(println (= 0.0 (math/atan2 0.0 1.0)))
(println (= 0.0 (math/sinh 0.0)))
(println (= 1.0 (math/cosh 0.0)))
(println (= 0.0 (math/tanh 0.0)))
(println (= 5.0 (math/hypot 3.0 4.0)))
(println (= 0.0 (math/expm1 0.0)))
(println (= 0.0 (math/log1p 0.0)))
(println (= 3.0 (math/cbrt 27.0)))
(println (= 8.0 (math/pow 2.0 3.0)))
(println (= 1.0 (math/IEEE-fmod 7.0 3.0)))
(println (let [value (math/random)]
           (and (<= 0.0 value) (< value 1.0))))
(defrecord ProtocolBox [value])
(extend-type ProtocolBox
  protocols/Datafiable
  (datafy [box] (:value box)))
(println (= "plain" (protocols/datafy "plain")))
(println (satisfies? protocols/Datafiable "plain"))
(println (= 42 (protocols/datafy (ProtocolBox. 42))))
(println (= 7 (protocols/nav {:answer 7} :answer 7)))
(defrecord SourceNamed [name-value ^:option<string> namespace-value])
(extend-type SourceNamed
  clojure.core/INamed
  (-name [value] (:name-value value))
  (-namespace [value] (:namespace-value value)))
(println (= "entry"
            (core/INamed/-name
              (SourceNamed. "entry" (Some "scope")))))
(println (= (Some "scope")
            (core/INamed/-namespace
              (SourceNamed. "entry" (Some "scope")))))
(println (= "entry" (core/INamed/-name :scope/entry)))
(println (= (Some "scope")
            (core/INamed/-namespace :scope/entry)))
(def source-writer (buffer/create 16))
(core/IWriter/-write source-writer "ab")
(println (= "ab" (buffer/contents source-writer)))
(println (= nil (core/IWriter/-flush source-writer)))
