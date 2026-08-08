(ns stdlib.clojure-set-app
  (:require
   [clojure.core :as core :refer [odd?]]
   [clojure.set :as set :refer [difference]]
   [clojure.string :as string :refer [upper-case]]))

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
