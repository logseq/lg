(ns source-core-lazy-transducer-app
  (:require [ocaml.Lg_runtime.Runtime_reduced :as runtime-reduced]))

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
