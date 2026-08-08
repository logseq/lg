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
