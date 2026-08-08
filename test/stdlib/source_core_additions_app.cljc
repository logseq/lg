(ns source-core-additions-app
  (:require [cljs.core :as core]))

(println (= 4 (bit-and-not 7 3)))
(println (= 8 (bit-and-not 15 3 4)))
(println (= 4 (unsigned-bit-shift-right 8 1)))
(println (= 0 (bit-count 0)))
(println (= 4 (bit-count 15)))

(def compare-less (comparator <))
(println (= -1 (compare-less 1 2)))
(println (= 1 (compare-less 2 1)))
(println (= 0 (compare-less 2 2)))

(def counts (frequencies [:left :right :left]))
(println (= 2 (get counts :left 0)))
(println (= 1 (get counts :right 0)))

(def incremented (update-vals {:left 1 :right 2} inc))
(println (= 2 (get incremented :left 0)))
(println (= 3 (get incremented :right 0)))

(def named (update-keys {:left 1 :right 2} (fn [key] (name key))))
(println (= 1 (get named "left" 0)))
(println (= 2 (get named "right" 0)))
(println (= 3 (core/bit-count 7)))
(println (= 2 (get (clojure.core/frequencies ["x" "x"]) "x" 0)))

(def annotated (with-meta {:left 1} {:source "upstream"}))
(println (= "upstream" (:source (meta (update-vals annotated inc)))))
(println (= "upstream"
            (:source (meta (update-keys annotated (fn [key] (name key)))))))

(println (= "zebra"
            (max-key (fn [value]
                       (if (= value "zebra") 3
                           (if (= value "middle") 2 1)))
                     "apple" "zebra" "middle")))
(println (= "second"
            (max-key (fn [value] (if (= value "same") 1 1))
                     "same" "second")))
(println (= "apple"
            (min-key (fn [value]
                       (if (= value "zebra") 3
                           (if (= value "middle") 2 1)))
                     "apple" "zebra" "middle")))
(println (= "second"
            (min-key (fn [value] (if (= value "same") 1 1))
                     "same" "second")))
(println (= 7 (core/max-key identity 7)))
(println (= 2 (clojure.core/min-key identity 9 4 2 8)))

(def always-ready (constantly :ready))
(println (= :ready (always-ready)))
(println (= :ready (always-ready 1)))
(println (= :ready (always-ready "left" true)))
(println (= :ready (always-ready 1 2 3 4)))
(println (= "core" ((core/constantly "core") 1 2 3)))
(println (= [1 2] ((clojure.core/constantly [1 2]) :ignored)))

(println (= [1 2 3] (vec (list 1 2 3))))
(println (= [1 2 3] (vec [1 2 3])))
(println (= ["a" "b"] (core/vec (seq ["a" "b"]))))
(println (= [] (clojure.core/vec (list))))
(defn realize-strings [values]
  (vec values))
(println (= ["left" "right"] (realize-strings ["left" "right"])))

(println (= ["x" "x" "x"] (replicate 3 "x")))
(println (= [] (replicate 0 :ignored)))
(println (= [] (core/replicate -2 :ignored)))
(println (= [7 7] (clojure.core/replicate 2 7)))

(defn last-keyword-key [^:map<keyword;int> entries]
  (reduce (fn [_result entry] (key entry)) :missing entries))
(def qualified-cljs-key cljs.core/key)
(def qualified-clojure-val clojure.core/val)
(defn last-string-key [^:map<string;bool> entries]
  (reduce (fn [_result entry] (qualified-cljs-key entry)) "missing" entries))
(defn last-int-value [^:map<keyword;int> entries]
  (reduce (fn [_result entry] (val entry)) 0 entries))
(defn last-bool-value [^:map<string;bool> entries]
  (reduce (fn [_result entry] (qualified-clojure-val entry)) false entries))
(println (= :left (last-keyword-key {:left 7})))
(println (= 7 (last-int-value {:left 7})))
(println (= "name" (last-string-key {"name" true})))
(println (= true (last-bool-value {"name" true})))

(println (= true (parse-boolean "true")))
(println (= false (parse-boolean "false")))
(println (nil? (parse-boolean "TRUE")))
(println (nil? (parse-boolean " false")))
(println (= true (core/parse-boolean "true")))
(println (= false (clojure.core/parse-boolean "false")))
