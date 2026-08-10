(ns stdlib.clojure-set-app
  (:require
   [clojure.core :as core :refer [odd?]]
   [clojure.edn :as edn]
   [clojure.set :as set :refer [difference project]]
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
(println (set/superset? #{1 2 3} #{1 2}))
(println (not (set/superset? #{1 2} #{1 2 3})))
(println (= #{1 3} (set/select odd? #{1 2 3 4})))
(println (= {"a" :left "b" :right}
            (set/map-invert {:left "a" :right "b"})))
(println (= {1 "one" 2 "two"}
            (set/map-invert {"one" 1 "two" 2})))
(def renamed-collision
  (set/rename-keys {:a 1 :b 2} {:a :b}))
(println (and (= 1 (count renamed-collision))
              (= (Some 1) (get renamed-collision :b))))
(def renamed-missing
  (set/rename-keys {:a 1 :b 2} {:a :c :missing :x}))
(println (and (= 2 (count renamed-missing))
              (= (Some 2) (get renamed-missing :b))
              (= (Some 1) (get renamed-missing :c))))
(def relation
  #{{:a 1 :b 2}
    {:a 1 :b 3}
    {:a 2 :b 4}})
(def forward-map (zipmap [:a :b] [1 2]))
(def reverse-map (zipmap [:b :a] [2 1]))
(println (= 1 (count (hash-set forward-map reverse-map))))
(def projected (project relation [:a]))
(println (= #{{:a 1} {:a 2}} projected))
(println (= #{} (set/project (empty relation) [:a])))
(def projected-empty-keys (set/project relation []))
(println (and (= 1 (count projected-empty-keys))
              (= 0 (count (first projected-empty-keys)))))
(def project-relation project)
(println (= #{{:b 2} {:b 3} {:b 4}}
            (project-relation relation [:b])))
(def renamed-relation (set/rename relation {:a :x}))
(println (= #{{:x 1 :b 2} {:x 1 :b 3} {:x 2 :b 4}}
            renamed-relation))
(println (= #{{:b 1}}
            (clojure.set/rename #{{:a 1 :b 2} {:a 1 :b 3}} {:a :b})))
(println (= #{} (set/rename (empty relation) {:a :x})))
(println (= relation (set/rename relation {})))
(println (= relation (set/rename relation {:missing :x})))
(def relation-evaluations (atom 0))
(def key-evaluations (atom 0))
(defn evaluated-relation []
  (do (swap! relation-evaluations inc) relation))
(defn evaluated-keys []
  (do (swap! key-evaluations inc) [:a]))
(def evaluated-project
  (set/project (evaluated-relation) (evaluated-keys)))
(println (and (= #{{:a 1} {:a 2}} evaluated-project)
              (= 1 (deref relation-evaluations))
              (= 1 (deref key-evaluations))))
(println (pr-str (set/union #{"a"} #{"b"})))
(println (string/join "," ["a" "b"]))
(println (string/index-of "banana" "na" 3))
(println (string/last-index-of "banana" "na" 3))
(println (upper-case "logseq"))
(println (identity 42))
(println (clojure.core/identity "core"))
(println ((core/complement (fn [x] (> x 0))) -1))
(println (not ((core/complement (fn [x] (+ x 1))) 1)))
(println (core/boolean 0))
(println (not (core/boolean nil)))
(println (core/every? (fn [x] (+ x 1)) [1 2]))
(println (core/every? (fn [x] (> x 0)) (hash-set 1 2)))
(println (not (core/every? (fn [x] (> x 1)) [1 2])))
(println (= [3 4] (core/fnext [[1 2] [3 4]])))
(println (= [[3 4]] (vec (core/nnext [[1 2] [2 3] [3 4]]))))
(println (= 1 (core/ffirst [[1 2] [3 4]])))
(println (= [2] (vec (core/nfirst [[1 2] [3 4]]))))
(println (core/even? 8))
(println (odd? 9))
(println (core/not-every? (fn [x] (> x 0)) [1 -1]))
(println (not (core/not-every? (fn [x] (+ x 1)) [1 2])))
(println (core/not-any? (fn [x] (< x 0)) [1 2]))
(println (not (core/not-any? (fn [x] (+ x 1)) [1 2])))
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
(println (= [1 0 2 0 3] (vec (core/interpose 0 [1 2 3]))))
(println (= [1 2 1] (vec (core/dedupe [1 1 2 2 1]))))
(println (= [1 2 3] (vec (core/distinct [1 2 1 3 2]))))
(println (= ["a" "b"] (vec (core/distinct ["a" "a" "b"]))))
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
