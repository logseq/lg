(ns stdlib.clojure-set-app
  (:require
   [clojure.core :as core :refer [odd?]]
   [clojure.edn :as edn]
   [clojure.set :as set :refer [difference]]
   [clojure.string :as string :refer [upper-case]]
   [cljs.reader :as reader]
   [ocaml.Lg_runtime.Runtime_edn :as runtime-edn]))

(println (count (set/union)))
(println (= #{1 2} (set/union #{1 2})))
(println (pr-str (set/union #{1 2} #{2 3} #{3 4})))
(println (pr-str (set/intersection #{1 2 3 4} #{2 3 4} #{3 4 5})))
(println (pr-str (difference #{1 2 3 4} #{2} #{4})))
(println (set/subset? #{1 2} #{1 2 3}))
(println (set/subset? #{1 4} #{1 2 3}))
(println (pr-str (set/union #{"a"} #{"b"})))
(println (string/join "," ["a" "b"]))
(println (string/index-of "banana" "na" 3))
(println (string/last-index-of "banana" "na" 3))
(println (upper-case "logseq"))
(println (identity 42))
(println (clojure.core/identity "core"))
(println ((core/complement (fn [x] (> x 0))) -1))
(println (core/even? 8))
(println (odd? 9))
(println (core/not-every? (fn [x] (> x 0)) [1 -1]))
(println (core/not-any? (fn [x] (< x 0)) [1 2]))
(def split-at-result (core/split-at 2 [1 2 3 4]))
(println (= 3 (first (second split-at-result))))
(def split-with-result (core/split-with (fn [x] (< x 3)) [1 2 3 4]))
(println (= 2 (count (first split-with-result))))
(println (= 3 (first (core/nthnext [1 2 3 4] 2))))
(println (= 4 (first (core/nthrest [1 2 3 4] 3))))
(println (= 2 (core/bounded-count 2 [1 2 3 4])))
(println (= 3 (count (core/butlast [1 2 3 4]))))
(println (= 3 (first (core/take-last 2 [1 2 3 4]))))
(println (= 3 (count (core/drop-last [1 2 3 4]))))
(println (= 2 (count (core/drop-last 2 [1 2 3 4]))))
(println (= 4 (first (core/reverse [1 2 3 4]))))
(println (= 2 (count (core/reverse (hash-set 1 2)))))
(println (= (Some 2) (get (core/zipmap [:a :b] [1 2]) :b)))
(println (= 4 (core/bit-clear 5 0)))
(println (= 7 (core/bit-set 5 1)))
(println (= 7 (core/bit-flip 5 1)))
(println (core/bit-test 4 2))
(println (= "{:answer 42}"
            (runtime-edn/write-string
             (edn/read-string "{:answer 42}"))))
(println (= "[1 2]"
            (runtime-edn/write-string
             (reader/read-string "[1 2]"))))
