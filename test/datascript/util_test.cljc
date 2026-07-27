(ns datascript.test.util
  (:require [datascript.inline :as inline]
            [datascript.util :as util]))

(def environment #?(:native "native" :melange "melange"))

(def find-calls (volatile! 0))
(def found
  (util/find
    #(do
       (vswap! find-calls inc)
       (> % 2))
    [1 2 3 4]))

(println
  (str environment ":find:"
       (= 3 found) ":"
       (= 3 (deref find-calls)) ":"
       (nil? (util/find #(> % 9) [1 2 3]))))

(println
  (str environment ":collections:"
       (= [1 2 3] (util/concatv [1] [] [2 3])) ":"
       (= [[1 4] [2 5]] (util/zip [1 2 3] [4 5])) ":"
       (= [[1 4 7] [2 5 8]] (util/zip [1 2] [4 5] [7 8])) ":"
       (= [1 2 3] (util/distinct-by #(mod % 3) [1 4 2 5 3 6])) ":"
       (= {:b 2} (util/removem #(= % :a) {:a 1 :b 2})) ":"
       (= [1] (util/conjv nil 1)) ":"
       (= #{1} (util/conjs nil 1))))

(println
  (str environment ":reduce:"
       (= 11 (util/reduce-indexed
               (fn [acc el idx] (+ acc (* el idx)))
               0
               [2 3 4])) ":"
       (reduced? (util/reduce-indexed
                   (fn [acc el idx]
                     (if (= idx 1) (reduced acc) (+ acc el)))
                   0
                   [2 3 4])) ":"
       (= 42 (util/single [42]))))

(def macro-if
  (util/if+
    (and true :let [x 4] (> x 3))
    (+ x 1)
    0))
(def macro-cond
  (util/cond+
    :let [x 2]
    (> x 1) (+ x 3)
    :else 0))

(println
  (str environment ":macros:"
       (= 5 macro-if) ":"
       (= 5 macro-cond) ":"
       (= 3 (util/some-of nil nil 3 4))))

(def debug-before util/*debug*)
(def debug-during
  (binding [util/*debug* true]
    util/*debug*))
(def debug-after util/*debug*)
(println
  (str environment ":debug:"
       (= false debug-before) ":"
       (= true debug-during) ":"
       (= false debug-after)))

(def assoc-map-calls (volatile! 0))
(def assoc-key-calls (volatile! 0))
(def ^:map<keyword;int> inline-empty-map {})
(defn ^:map<keyword;int> inline-int-map []
  (assoc inline-empty-map :a 1))
(def assoc-single
  (inline/assoc
   (do
     (vswap! assoc-map-calls inc)
     (inline-int-map))
   (do
     (vswap! assoc-key-calls inc)
     :b)
   2))
(def assoc-many
  (inline/assoc (inline-int-map) :b 2 :c 3 :d 4))

(println
  (str environment ":inline-assoc:"
       (and
        (= 1 (get assoc-single :a))
        (= 2 (get assoc-single :b))) ":"
       (= 1 @assoc-map-calls) ":"
       (= 1 @assoc-key-calls) ":"
       (and
        (= 1 (get assoc-many :a))
        (= 2 (get assoc-many :b))
        (= 3 (get assoc-many :c))
        (= 4 (get assoc-many :d)))))

(def update-map-calls (volatile! 0))
(def update-key-calls (volatile! 0))
(defn ^:map<keyword;int> inline-counter-map []
  (assoc inline-empty-map :n 1))
(def update-once
  (inline/update
   (do
     (vswap! update-map-calls inc)
     (inline-counter-map))
   (do
     (vswap! update-key-calls inc)
     :n)
   inc))

(println
  (str environment ":inline-update:"
       (= 2 (get update-once :n)) ":"
       (= 1 @update-map-calls) ":"
       (= 1 @update-key-calls) ":"
       (= 3
          (get
           (inline/update (inline-counter-map) :n + 2)
           :n)) ":"
       (= 6
          (get
           (inline/update (inline-counter-map) :n + 2 3)
           :n)) ":"
       (= 10
          (get
           (inline/update (inline-counter-map) :n + 2 3 4)
           :n)) ":"
       (= 15
          (get
           (inline/update (inline-counter-map) :n + 2 3 4 5)
           :n))))

(def timestamp 1700000000)
(def id (util/squuid timestamp))
(println
  (str environment ":squuid:"
       (= timestamp (util/squuid-time-millis id))))
