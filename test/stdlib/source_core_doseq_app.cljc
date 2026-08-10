(ns source-core-doseq-app
  (:require [cljs.core :as core :refer [doseq]]))

(def automatic-total (atom 0))
(println
 (nil?
  (doseq [value [1 2 3]]
    (swap! automatic-total + value))))
(println (= 6 @automatic-total))

(def while-values (atom [0]))
(doseq [value [1 2 3 4]
        :let [doubled (* value 2)]
        :while (< doubled 7)]
  (swap! while-values (fn [^:vector<int> values] (conj values doubled))))
(println (= [0 2 4 6] @while-values))

(def filtered-values (atom [0]))
(doseq [value [1 2 3 4]
        :when (even? value)]
  (swap! filtered-values (fn [^:vector<int> values] (conj values value))))
(println (= [0 2 4] @filtered-values))

(def nested-values (atom [0]))
(doseq [outer [1 2]
        inner [1 2 3]
        :while (< inner 3)]
  (swap! nested-values
         (fn [^:vector<int> values]
           (conj values (+ (* outer 10) inner)))))
(println (= [0 11 12 21 22] @nested-values))

(def collection-evaluations (atom 0))
(def evaluated-total (atom 0))
(doseq [value (do (swap! collection-evaluations inc) [4 5])]
  (swap! evaluated-total + value))
(println
 (and (= 1 @collection-evaluations)
      (= 9 @evaluated-total)))

(def destructured-total (atom 0))
(doseq [[_key value] (hash-map 'first 1 'second 2)]
  (swap! destructured-total + value))
(println (= 3 @destructured-total))

(def alias-total (atom 0))
(core/doseq [value [2 3]]
  (swap! alias-total + value))
(println (= 5 @alias-total))

(def qualified-total (atom 0))
(clojure.core/doseq [value [6 7]]
  (swap! qualified-total + value))
(println (= 13 @qualified-total))

(println (nil? (doseq [value [1 2]])))
