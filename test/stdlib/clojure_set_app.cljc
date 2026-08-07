(ns stdlib.clojure-set-app
  (:require [clojure.set :as set :refer [difference]]))

(println (count (set/union)))
(println (= #{1 2} (set/union #{1 2})))
(println (pr-str (set/union #{1 2} #{2 3} #{3 4})))
(println (pr-str (set/intersection #{1 2 3 4} #{2 3 4} #{3 4 5})))
(println (pr-str (difference #{1 2 3 4} #{2} #{4})))
(println (set/subset? #{1 2} #{1 2 3}))
(println (set/subset? #{1 4} #{1 2 3}))
(println (pr-str (set/union #{"a"} #{"b"})))
