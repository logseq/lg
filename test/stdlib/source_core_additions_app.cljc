(ns source-core-additions-app
  (:require [cljs.core :as core :refer [NaN? bit-not dec hash-long inc infinite? keyword-identical? merge-with parse-double parse-long parse-uuid special-symbol? symbol-identical?]]))

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

(def splitv core/splitv-at)
(let [[prefix suffix] (splitv 2 [1 2 3 4])]
  (println (= [1 2] prefix))
  (println (= [3 4] (vec suffix))))
(let [[prefix suffix] (splitv-at 0 [1 2])]
  (println (= [] prefix))
  (println (= [1 2] (vec suffix))))
(let [[prefix suffix] (cljs.core/splitv-at -2 [1 2])]
  (println (= [] prefix))
  (println (= [1 2] (vec suffix))))
(let [[prefix suffix] (clojure.core/splitv-at 5 [1 2])]
  (println (= [1 2] prefix))
  (println (= [] (vec suffix))))
(let [[prefix suffix] (splitv-at 1 ["left" "right"])]
  (println (= ["left"] prefix))
  (println (= ["right"] (vec suffix))))

(println (= [true false] (booleans [true false])))
(println (= [1 2] (bytes [1 2])))
(println (= ["a" "b"] (chars ["a" "b"])))
(println (= [1 2] (shorts [1 2])))
(println (= [1 2] (ints [1 2])))
(println (= [1.5 2.5] (floats [1.5 2.5])))
(println (= [1.5 2.5] (doubles [1.5 2.5])))
(println (= [1 2] (longs [1 2])))
(println (= "unchanged" (core/ints "unchanged")))
(println (= [1 2] (into [] (take 2) [1 2 3])))
(println (= [3] (into [] (drop 2) [1 2 3])))

(def uuid-pattern
  #"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
(println (boolean (re-matches uuid-pattern (str (random-uuid)))))
(println (boolean (re-matches uuid-pattern (str (core/random-uuid)))))
(println (= "550e8400-e29b-41d4-a716-446655440000"
            (when-some [parsed (parse-uuid "550e8400-e29b-41d4-a716-446655440000")]
              (str parsed))))
(println (= "550e8400-e29b-41d4-a716-446655440000"
            (when-some [parsed (cljs.core/parse-uuid "550E8400-E29B-41D4-A716-446655440000")]
              (str parsed))))
(println (nil? (clojure.core/parse-uuid "not-a-uuid")))
(println (<= 0.0 (system-time)))
(println (<= 0.0 (core/system-time)))
(println (<= 0.0 (clojure.core/system-time)))

(println (= 0 (parse-long "0")))
(println (= 42 (parse-long "+42")))
(println (= -42 (parse-long "-42")))
(println (= 12 (parse-long "0012")))
(println (= 9007199254740991 (parse-long "9007199254740991")))
(println (= -9007199254740991 (parse-long "-9007199254740991")))
(println (nil? (parse-long "9007199254740992")))
(println (nil? (parse-long "-9007199254740992")))
(println (nil? (parse-long "18446744073709551616")))
(println (nil? (parse-long "")))
(println (nil? (parse-long "+")))
(println (nil? (parse-long " 42")))
(println (nil? (parse-long "42 ")))
(println (nil? (parse-long "1.0")))
(println (nil? (parse-long "12a")))
(println (= 7 (core/parse-long "7")))
(println (= 8 (clojure.core/parse-long "8")))

(println (= 0.0 (parse-double "0")))
(println (= 1.5 (parse-double "+1.5")))
(println (= 0.5 (parse-double ".5")))
(println (= 1.0 (parse-double "1.")))
(println (= 1000.0 (parse-double "1e3")))
(println (= -0.01 (parse-double "-1E-2")))
(println (= 1.0 (parse-double "1d")))
(println (= 1.5 (parse-double "  1.5  ")))
(println (= ##Inf (parse-double "Infinity")))
(println (= ##-Inf (parse-double "-Infinity")))
(println (when-some [value (parse-double "NaN")] (js/isNaN value)))
(println (when-some [value (parse-double "+NaN")] (js/isNaN value)))
(println (nil? (parse-double "")))
(println (nil? (parse-double ".")))
(println (nil? (parse-double "1e")))
(println (nil? (parse-double "1 2")))
(println (nil? (parse-double "infinity")))
(println (nil? (parse-double "nan")))
(println (= 2.5 (core/parse-double "2.5")))
(println (= 3.5 (clojure.core/parse-double "3.5")))

(defn add-values [left right]
  (+ left right))

(def merged-basic
  (merge-with add-values {:left 1 :right 2} {:left 4 :extra 3}))
(println (= 5 (get merged-basic :left 0)))
(println (= 2 (get merged-basic :right 0)))
(println (= 3 (get merged-basic :extra 0)))

(def merged-left-to-right
  (merge-with (fn [left right] (- left right))
              {:value 20}
              {:value 3}
              {:value 2}))
(println (= 15 (get merged-left-to-right :value 0)))
(println (= 9 (get (merge-with add-values {:value 9}) :value 0)))
(println (nil? (merge-with add-values)))
(println (= 7 (get (merge-with add-values nil {:value 7} nil) :value 0)))
(println (= 0 (count (merge-with add-values nil nil))))

(def merged-disjoint
  (merge-with (fn [_left _right] (/ 1 0)) {:left 1} {:right 2}))
(println (= 1 (get merged-disjoint :left 0)))
(println (= 2 (get merged-disjoint :right 0)))

(def merged-strings
  (core/merge-with (fn [left right]
                     (if (= right "right") "left:right" left))
                   {1 "left"}
                   {1 "right" 2 "other"}))
(println (= "left:right" (get merged-strings 1 "")))
(println (= "other" (get merged-strings 2 "")))
(println (= 6 (get (clojure.core/merge-with add-values
                                           {:value 1}
                                           {:value 2}
                                           {:value 3})
                   :value
                   0)))

(println (NaN? ##NaN))
(println (not (NaN? 1.5)))
(println (core/NaN? ##NaN))
(println (infinite? ##Inf))
(println (infinite? ##-Inf))
(println (not (infinite? 1.5)))
(println (not (core/infinite? ##NaN)))

(println (keyword-identical? :block/uuid :block/uuid))
(println (not (keyword-identical? :block/uuid :block/title)))
(println (core/keyword-identical? :db/id :db/id))
(println (clojure.core/keyword-identical? :db/id :db/id))

(println (symbol-identical? (symbol "user" "value") (symbol "user" "value")))
(println (not (symbol-identical? (symbol "left") (symbol "right"))))
(println (core/symbol-identical? (symbol "ready") (symbol "ready")))

(println (= 4 (hash-long 7 3)))
(println (= -1 (core/hash-long 0 -1)))
(println (special-symbol? (symbol "if")))
(println (special-symbol? (symbol "set!")))
(println (not (clojure.core/special-symbol? (symbol "ordinary"))))

(def original-array (array 1 2 3))
(def cloned-array (aclone original-array))
(aset cloned-array 0 9)
(println (= 1 (aget original-array 0)))
(println (= 9 (aget cloned-array 0)))
(println (not (identical? original-array cloned-array)))

(println (distinct? 1))
(println (distinct? 1 2))
(println (not (distinct? 1 1)))
(println (distinct? 1 2 3 4 5))
(println (not (core/distinct? "a" "b" "a")))

(println (not (not= 1)))
(println (not= 1 2))
(println (not (not= 1 1)))
(println (not= 1 1 2 1))
(println (not (clojure.core/not= "same" "same" "same")))
(let [same-fn (fn [value] value)]
  (println (not (not= same-fn same-fn))))
(println (not= (fn [value] value) (fn [value] value)))

(def conversion-vector [10 20 30])
(def converted-array (to-array conversion-vector))
(aset converted-array 0 99)
(println (= 10 (first conversion-vector)))
(println (= 99 (aget converted-array 0)))

(def conversion-source-array (array 4 5 6))
(def copied-source-array (to-array conversion-source-array))
(aset copied-source-array 0 8)
(println (= 4 (aget conversion-source-array 0)))
(println (= 8 (aget copied-source-array 0)))
(println (= (list 7 8) (array-seq (into-array (list 7 8)))))
(println (= (list 4 5 6) (array-seq conversion-source-array)))
(println (= (list 5 6) (array-seq conversion-source-array 1)))
(println (empty? (array-seq conversion-source-array 3)))
(println (= 2 (alength (core/to-array [1 2]))))
(println (= 9 (first (core/array-seq (core/into-array [9])))))

(def source-increment inc)
(def source-decrement dec)
(def source-bit-not bit-not)
(println (= 0 (source-increment -1)))
(println (= -1 (source-decrement 0)))
(println (= -1 (source-bit-not 0)))
(println (= 0 (bit-not -1)))
(println (= -6 (bit-not 5)))
(println (= 42 (core/inc 41)))
(println (= 40 (clojure.core/dec 41)))
(println (= -43 (core/bit-not 42)))
(println (= [2 3 4] (vec (map inc [1 2 3]))))
