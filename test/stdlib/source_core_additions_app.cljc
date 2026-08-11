(ns source-core-additions-app
  (:require [cljs.core :as core :refer [INext ISeq NaN? add-to-string-hash-cache areduce array-binary-search-left array-binary-search-right array-from array-index-of array-values bit-and bit-not bit-or bit-shift-left bit-shift-right bit-xor concat dec decimal? divide equiv-map flush hash-double hash-keyword hash-long hash-map-lite hash-string ifind? inc infinite? into iterate key-test keyword-identical? locking map-entry? mapv merge-with parse-double parse-long parse-uuid partitionv ratio? realized? reduceable? regexp? set-lite special-symbol? symbol-identical? tree-seq vector-lite volatile?]]
            [cljs.reader :as reader :refer [deregister-default-tag-parser! deregister-tag-parser! parse-and-validate-timestamp]]
            [clojure.data :as data :refer [diff]]
            [clojure.string :as string :refer [split]]
            [clojure.walk :as walk :refer [keywordize-keys postwalk-replace prewalk-replace stringify-keys]]
            [clojure.zip :as zip :refer [node]]
            [ocaml.Lg_runtime.Runtime_edn :as runtime-edn]))

(println (= 4 (bit-and-not 7 3)))
(println (= 8 (bit-and-not 15 3 4)))
(def source-enable-console-print! enable-console-print!)
(source-enable-console-print!)
(println true)
(defrecord WeakSourceBox [^int value])
(def retained-weak-source-box (WeakSourceBox. 7))
(def weak-source-reference (weak-ref retained-weak-source-box))
(def source-weak-deref weak-deref)
(def source-weak-clear! weak-clear!)
(println
 (if-some [box (source-weak-deref weak-source-reference)]
   (= 7 (:value box))
   false))
(source-weak-clear! weak-source-reference)
(println (nil? (source-weak-deref weak-source-reference)))
(def source-subvec subvec)
(def source-array array)
(println (= [2 3] (source-subvec [1 2 3 4] 1 3)))
(println (= 3 (aget (source-array 1 2 3) 2)))
(defrecord AssocState [^:map<keyword;int> properties])
(def assoc-updated
  (update (AssocState. {}) :properties assoc :answer 42))
(println (get (:properties assoc-updated) :answer))
(def transducing-into into)
(println (if (= [1 2] (transducing-into [] (take 2) [1 2 3]))
           "into-first-class-transducer"
           "into-first-class-transducer-failed"))
(println (if (= [2 3 4] (vec (map inc (list 1 2 3))))
           "map-unary"
           "map-unary-failed"))
(println (if (= [11] (vec (core/map (fn [left right] (+ left right))
                                    [1 2] [10])))
           "map-binary-shortest"
           "map-binary-shortest-failed"))
(println (if (= [111 222]
                (vec (clojure.core/map (fn [a b c] (+ a b c))
                                       [1 2] [10 20] [100 200])))
           "map-ternary"
           "map-ternary-failed"))
(println (if (= [1111]
                (vec (map (fn [a b c d] (+ a b c d))
                          [1 2] [10] [100 200] [1000 2000])))
           "map-variadic"
           "map-variadic-failed"))
(println (if (= [] (vec (map (fn [a b c d] (+ a b c d))
                             [1] [10] [] [1000])))
           "map-empty-shortest"
           "map-empty-shortest-failed"))
(def collect-map map)
(println (if (= [false true]
                (vec (collect-map (fn [value] (if value false true))
                                  [true false])))
           "map-first-class"
           "map-first-class-failed"))
(println (if (= [1111]
                (vec (collect-map (fn [a b c d] (+ a b c d))
                                  [1] [10] [100] [1000])))
           "map-first-class-variadic"
           "map-first-class-variadic-failed"))
(println (if (= [[1 10 100 1000]]
                (vec (apply map vector [[1] [10] [100] [1000]])))
           "map-apply"
           "map-apply-failed"))
(println (if (= [2 3]
                (transducing-into [] (map inc) [1 2]))
           "map-transducer"
           "map-transducer-failed"))
(def map-realizations (atom 0))
(def map-lazy-source
  (map (fn [value]
         (swap! map-realizations inc)
         (inc value))
       [1 2 3]))
(println (if (= 0 @map-realizations) "map-lazy-before" "map-lazy-before-failed"))
(println (if (= 2 (first map-lazy-source)) "map-lazy-first" "map-lazy-first-failed"))
(println (if (and (= 2 (first map-lazy-source)) (= 1 @map-realizations))
           "map-lazy-once"
           "map-lazy-once-failed"))
(println (if (= [1 2 3 4 5] (vec (take 5 (map inc (range)))))
           "map-infinite-prefix"
           "map-infinite-prefix-failed"))
(def map-argument-order (atom []))
(def map-eager-source
  (map (do (swap! map-argument-order conj 1)
           (fn [left right] (+ left right)))
       (do (swap! map-argument-order conj 2) [1])
       (do (swap! map-argument-order conj 3) [10])))
(println (if (= [1 2 3] @map-argument-order)
           "map-arguments-eager-once"
           "map-arguments-eager-once-failed"))
(println (if (= [11] (vec map-eager-source))
           "map-arguments-result"
           "map-arguments-result-failed"))
(println (if (= [2 3 4] (mapv inc (list 1 2 3)))
           "mapv-unary"
           "mapv-unary-failed"))
(println (if (= [11] (core/mapv (fn [left right] (+ left right)) [1 2] [10]))
           "mapv-binary-shortest"
           "mapv-binary-shortest-failed"))
(println (if (= [111 222]
                (clojure.core/mapv (fn [a b c] (+ a b c))
                                   [1 2] [10 20] [100 200]))
           "mapv-ternary"
           "mapv-ternary-failed"))
(println (if (= [1111]
                (mapv (fn [a b c d] (+ a b c d))
                      [1 2] [10] [100 200] [1000 2000]))
           "mapv-variadic"
           "mapv-variadic-failed"))
(println (if (= [] (mapv (fn [left right] (+ left right)) [1] []))
           "mapv-empty-shortest"
           "mapv-empty-shortest-failed"))
(def collect-mapv mapv)
(println (if (= [false true]
                (collect-mapv (fn [value] (if value false true))
                              [true false]))
           "mapv-first-class"
           "mapv-first-class-failed"))
(println (if (= [11]
                (collect-mapv (fn [left right] (+ left right))
                              [1 2] [10]))
           "mapv-first-class-binary"
           "mapv-first-class-binary-failed"))
(println (if (= [1111]
                (collect-mapv (fn [a b c d] (+ a b c d))
                              [1] [10] [100] [1000]))
           "mapv-first-class-variadic"
           "mapv-first-class-variadic-failed"))
(println (if (= [[1 10] [2 20]]
                (apply mapv vector [[1 2] [10 20]]))
           "mapv-apply"
           "mapv-apply-failed"))
(def mapv-evaluations (atom 0))
(println
 (if (= [11]
        (mapv (do (swap! mapv-evaluations inc)
                  (fn [left right] (+ left right)))
              (do (swap! mapv-evaluations inc) [1])
              (do (swap! mapv-evaluations inc) [10])))
   "mapv-single-evaluation-result"
   "mapv-single-evaluation-result-failed"))
(println (if (= 3 @mapv-evaluations)
           "mapv-single-evaluation-count"
           "mapv-single-evaluation-count-failed"))
(println (if (= [] (vec (concat)))
           "concat-zero"
           "concat-zero-failed"))
(println (if (= [1 2] (vec (core/concat [1 2])))
           "concat-one"
           "concat-one-failed"))
(println (if (= [1 2 3 4]
                (vec (clojure.core/concat [1 2] (list 3 4))))
           "concat-two-storage-types"
           "concat-two-storage-types-failed"))
(println (if (= [1 2 3 4]
                (vec (concat [1] [] (list 2 3) [4])))
           "concat-variadic-empty-middle"
           "concat-variadic-empty-middle-failed"))
(println (if (= [1 2] (vec (concat nil [1 2])))
           "concat-nil"
           "concat-nil-failed"))
(println (if (= [0 1 2 3 4] (vec (take 5 (concat (range) [99]))))
           "concat-infinite-prefix"
           "concat-infinite-prefix-failed"))
(def concat-realizations (atom 0))
(def concat-lazy-source
  (lazy-seq
   (do (swap! concat-realizations inc)
       (seq [1 2]))))
(def concat-lazy-result (concat concat-lazy-source [3]))
(println (if (= 0 @concat-realizations)
           "concat-lazy-before"
           "concat-lazy-before-failed"))
(println (if (= 1 (first concat-lazy-result))
           "concat-lazy-first"
           "concat-lazy-first-failed"))
(println (if (= 1 @concat-realizations)
           "concat-lazy-once"
           "concat-lazy-once-failed"))
(def concat-argument-evaluations (atom 0))
(def concat-evaluated-arguments
  (concat (do (swap! concat-argument-evaluations inc) [1])
          (do (swap! concat-argument-evaluations inc) [2])))
(println (if (= 2 @concat-argument-evaluations)
           "concat-arguments-eager-once"
           "concat-arguments-eager-once-failed"))
(println (if (= [1 2] (vec concat-evaluated-arguments))
           "concat-arguments-result"
           "concat-arguments-result-failed"))
(def join-concat concat)
(println (if (= [] (vec (join-concat)))
           "concat-first-class-zero"
           "concat-first-class-zero-failed"))
(println (if (= [1 2] (vec (join-concat [1] (list 2))))
           "concat-first-class-two"
           "concat-first-class-two-failed"))
(println (if (= [1 2 3 4]
                (vec (join-concat [1] [2] [3] [4])))
           "concat-first-class-variadic"
           "concat-first-class-variadic-failed"))
(println (if (= [1 2] (vec (apply concat [[1] [2]])))
           "concat-apply"
           "concat-apply-failed"))
(println (if (= [] (vec (interleave)))
           "interleave-zero"
           "interleave-zero-failed"))
(println (if (= [1 2] (vec (core/interleave (list 1 2))))
           "interleave-one"
           "interleave-one-failed"))
(println (if (= [1 10]
                (vec (clojure.core/interleave [1 2] (list 10))))
           "interleave-two-shortest"
           "interleave-two-shortest-failed"))
(println (if (= [1 10 100 2 20 200]
                (vec (interleave [1 2 3]
                                 (list 10 20)
                                 [100 200 300])))
           "interleave-variadic"
           "interleave-variadic-failed"))
(println (if (= [] (vec (interleave [1] [] [100])))
           "interleave-empty-shortest"
           "interleave-empty-shortest-failed"))
(def weave interleave)
(println (if (= [] (vec (weave)))
           "interleave-first-class-zero"
           "interleave-first-class-zero-failed"))
(println (if (= [1 2] (vec (weave [1 2])))
           "interleave-first-class-one"
           "interleave-first-class-one-failed"))
(println (if (= [1 10 100]
                (vec (weave [1] [10] [100])))
           "interleave-first-class-variadic"
           "interleave-first-class-variadic-failed"))
(println (if (= [1 10 100 2 20 200]
                (vec (apply interleave [[1 2] [10 20] [100 200]])))
           "interleave-apply"
           "interleave-apply-failed"))
(def interleave-realizations (atom 0))
(def interleave-lazy-result
  (interleave
   (map (fn [value] (swap! interleave-realizations inc) value) [1 2])
   (map (fn [value] (swap! interleave-realizations inc) value) [10 20])))
(println (if (= 0 @interleave-realizations)
           "interleave-lazy-before"
           "interleave-lazy-before-failed"))
(println (if (= 1 (first interleave-lazy-result))
           "interleave-lazy-first"
           "interleave-lazy-first-failed"))
(println (if (and (= 1 (first interleave-lazy-result))
                  (= 2 @interleave-realizations))
           "interleave-lazy-once"
           "interleave-lazy-once-failed"))
(println (if (= [0 9 1 9 2 9]
                (vec (take 6 (interleave (range) (repeat 9)))))
           "interleave-infinite-prefix"
           "interleave-infinite-prefix-failed"))
(def interleave-argument-order (atom []))
(def interleave-eager-result
  (interleave (do (swap! interleave-argument-order conj 1) [1])
              (do (swap! interleave-argument-order conj 2) [10])
              (do (swap! interleave-argument-order conj 3) [100])))
(println (if (= [1 2 3] @interleave-argument-order)
           "interleave-arguments-eager-once"
           "interleave-arguments-eager-once-failed"))
(println (if (= [1 10 100] (vec interleave-eager-result))
           "interleave-arguments-result"
           "interleave-arguments-result-failed"))
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

(def source-meta meta)
(def source-with-meta with-meta)
(def cljs-meta core/meta)
(def clojure-with-meta clojure.core/with-meta)
(def ^:map<keyword;int> source-annotated
  (source-with-meta {:left 1} (meta annotated)))
(println (= {:left 1} source-annotated))
(println (= "upstream" (:source (source-meta source-annotated))))
(println (= "upstream" (:source (cljs-meta source-annotated))))
(def ^:map<keyword;int> replaced-metadata
  (clojure-with-meta source-annotated {:source "replacement"}))
(println (= "replacement" (:source (core/meta replaced-metadata))))
(println (= "upstream" (:source (clojure.core/meta source-annotated))))
(println (= {:left 1} replaced-metadata))

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

(def copy-array array-from)
(def copied-integers (copy-array [1 3 5 7]))
(def original-copy-source (array 4 5 6))
(def copied-source (core/array-from original-copy-source))
(aset copied-source 0 9)
(println (= 4 (aget original-copy-source 0)))
(println (= 9 (aget copied-source 0)))
(def copied-strings (clojure.core/array-from ["a" "b"]))
(println (= "b" (aget copied-strings 1)))

(defn compare-integers [left right]
  (compare left right))
(defn compare-strings [left right]
  (compare left right))
(def source-search-left array-binary-search-left)
(println (= 2.0 (source-search-left compare-integers copied-integers 3 4)))
(println (= 3.0 (core/array-binary-search-right compare-integers copied-integers 3 5)))
(println (= 1.0 (clojure.core/array-binary-search-left compare-strings copied-strings 1 "b")))

(def constructed-integers (array-values 1 2 3))
(println (= 3 (alength constructed-integers)))
(println (= 2 (aget constructed-integers 1)))
(println (= :only (aget (core/array-values :only) 0)))
(def construction-order (atom []))
(array-values
  (do (swap! construction-order conj 1) 10)
  (do (swap! construction-order conj 2) 20)
  (do (swap! construction-order conj 3) 30))
(println (= [1 2 3] @construction-order))

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

(def source-bit-or bit-or)
(println (= 3 (bit-and 15 7 3)))
(println (= 15 (bit-or 1 2 4 8)))
(println (= 9 (bit-xor 15 3 5)))
(println (= 10 (source-bit-or 8 2)))
(println (= 0 (reduce bit-xor 0 [1 2 3])))
(println (= 48 (bit-shift-left 3 4)))
(println (= -4 (bit-shift-right -16 2)))
(println (= 10 (core/bit-shift-left 5 1)))
(println (= 6 (clojure.core/bit-and 7 6)))

(def source-ratio? ratio?)
(def source-decimal? decimal?)
(println (not (source-ratio? 1)))
(println (not (source-ratio? "1")))
(println (not (source-decimal? 1)))
(println (not (source-decimal? :value)))
(def predicate-calls (atom 0))
(println
 (and (not (ratio? (do (swap! predicate-calls inc) 1)))
      (= 1 @predicate-calls)))
(println (not (core/ratio? 1)))
(println (not (clojure.core/decimal? "1")))

(def source-future-call future-call)
(def completed-future (source-future-call (fn [] 42)))
(def source-realized? realized?)
(println (source-realized? completed-future))
(println (core/realized? completed-future))

(def source-imul core/imul)
(def source-hash-int clojure.core/m3-hash-int)
(println (= 878082066 (core/int-rotate-left 305419896 8)))
(println (= 2147483643 (source-imul 2147483647 5)))
(println (= -1017931171 (core/m3-mix-K1 1)))
(println (= 651101558 (core/m3-mix-H1 0 (core/m3-mix-K1 1))))
(println (= -68075478 (core/m3-fmix 651101558 4)))
(println (= 1982413648 (source-hash-int -1)))
(println (= -196466786 (core/mix-collection-hash -1 3)))
(def source-string-hash core/hash-string*)
(def source-unencoded-hash core/m3-hash-unencoded-chars)
(println (= 1772899 (source-string-hash "😀")))
(println (= 2147505144 (source-string-hash "墀㺙眧悱崯뒛")))
(println (= 1118836419 (source-unencoded-hash "abc")))
(println (= 1443257913 (source-unencoded-hash "😀")))

(deftype SourceFindable [^:int value])
(extend-type SourceFindable
  IFind
  (-find [this key]
    (if (= key :value)
      (Some (tuple key (.-value this)))
      None)))

(def source-ifind? ifind?)
(def source-map-entry? map-entry?)
(def source-regexp? regexp?)
(def source-volatile? volatile?)
(def ^:map<keyword;int> predicate-map {:value 1})
(println (source-ifind? predicate-map))
(println (not (ifind? [1 2])))
(println (core/ifind? (SourceFindable. 7)))
(println
  (if-let [entry (first (seq predicate-map))]
    (and (source-map-entry? entry) (= 1 (val entry)))
    false))
(println (not (map-entry? [1 2])))
(println (regexp? #"value"))
(println (not (cljs.core/regexp? "value")))
(println (source-regexp? #"source"))
(println (volatile? (volatile! 1)))
(println (not (core/volatile? 1)))
(println (source-volatile? (volatile! :ready)))
(println (clojure.core/ifind? predicate-map))
(println
  (if-let [entry (first (seq predicate-map))]
    (and (clojure.core/map-entry? entry) (= 1 (val entry)))
    false))
(println (clojure.core/regexp? #"qualified"))
(println (cljs.core/volatile? (volatile! 1)))

(def source-hash-double hash-double)
(def source-hash-keyword hash-keyword)
(def source-hash-string hash-string)
(def source-array-index-of array-index-of)
(println (= (hash 1.0) (source-hash-double 1.0)))
(println (= 0 (hash-double 0.0)))
(println (= (hash :block/uuid) (source-hash-keyword :block/uuid)))
(println (= (hash :db/id) (core/hash-keyword :db/id)))
(println (= 96354 (source-hash-string "abc")))
(println (= 0 (hash-string nil)))
(println (= (hash-string* "😀") (cljs.core/hash-string "😀")))
(def indexed-words (array-values "a" "value-a" "b" "value-b"))
(println (= 2 (source-array-index-of indexed-words "b")))
(println (= -1 (array-index-of indexed-words "value-a")))
(println (= -1 (core/array-index-of indexed-words "missing")))
(def indexed-integers (array-values 10 100 20 200))
(println (= 2 (cljs.core/array-index-of indexed-integers 20)))
(println (= -1 (clojure.core/array-index-of indexed-integers 100)))
(println (= (hash 2.5) (clojure.core/hash-double 2.5)))
(println (= (hash :source/value) (cljs.core/hash-keyword :source/value)))
(println (= 120 (clojure.core/hash-string "x")))

(def source-iterate iterate)
(println (= [1 2 3 4 5] (vec (take 5 (source-iterate inc 1)))))
(println (= ["a" "a!" "a!!"]
            (vec (take 3 (core/iterate (fn [value] (str value "!")) "a")))))
(println (= [2 4 8 16]
            (vec (take 4 (clojure.core/iterate (fn [value] (* value 2)) 2)))))

(defn source-tree-branch? [node]
  (< node 4))

(defn source-tree-children [node]
  (cond
    (= node 1) [2 3]
    (= node 2) [4 5]
    (= node 3) [6]
    :else []))

(def source-tree-seq tree-seq)
(println (= [1 2 4 5 3 6]
            (vec (source-tree-seq source-tree-branch? source-tree-children 1))))
(println (= [1 2 4 5 3 6]
            (vec (core/tree-seq source-tree-branch? source-tree-children 1))))
(println (= [1 2 4 5 3 6]
            (vec (clojure.core/tree-seq
                  source-tree-branch? source-tree-children 1))))
(def source-tree-visits (atom []))
(def source-lazy-tree
  (tree-seq
   (fn [node]
     (do
       (swap! source-tree-visits conj node)
       (source-tree-branch? node)))
   source-tree-children
   1))
(println (empty? @source-tree-visits))
(println (= [1 2] (vec (take 2 source-lazy-tree))))
(println (= [1 2] @source-tree-visits))

(def source-partitionv partitionv)
(println (= [[1 2] [3 4]] (vec (source-partitionv 2 [1 2 3 4 5]))))
(println (= [[1 2] [2 3]] (vec (partitionv 2 1 [1 2 3]))))
(println (= [[1 2 3] [4 0 0]]
            (vec (core/partitionv 3 3 [0 0] [1 2 3 4]))))
(println (= [[1 2 3] [4 0]]
            (vec (clojure.core/partitionv 3 3 [0] [1 2 3 4]))))
(println (= [] (vec (partitionv 3 [1 2]))))

(def source-add-to-string-hash-cache add-to-string-hash-cache)
(println (= 96354 (source-add-to-string-hash-cache "abc")))
(println (= 0 (add-to-string-hash-cache nil)))
(println (= (hash-string* "source")
            (core/add-to-string-hash-cache "source")))
(println (= (hash-string* "qualified")
            (clojure.core/add-to-string-hash-cache "qualified")))

(def source-flush flush)
(println (nil? (source-flush)))
(println (nil? (flush)))
(println (nil? (core/flush)))
(println (nil? (clojure.core/flush)))

(def source-reduce-values (array-values 1 2 3 4))
(println (= 10
            (areduce source-reduce-values index result 0
              (+ result (aget source-reduce-values index)))))
(println (= 24
            (core/areduce source-reduce-values index result 1
              (* result (aget source-reduce-values index)))))
(println (= "1234"
            (clojure.core/areduce source-reduce-values index result ""
              (str result (aget source-reduce-values index)))))
(def source-array-evaluations (atom 0))
(println (= 10
            (areduce
             (do (swap! source-array-evaluations inc) source-reduce-values)
             index result 0
             (+ result (aget source-reduce-values index)))))
(println (= 1 @source-array-evaluations))

(def source-lock-evaluations (atom 0))
(def source-lock-body (atom []))
(println (= :done
            (locking (swap! source-lock-evaluations inc)
              (swap! source-lock-body conj :first)
              (swap! source-lock-body conj :second)
              :done)))
(println (= 0 @source-lock-evaluations))
(println (= [:first :second] @source-lock-body))
(println (nil? (core/locking :unused)))
(println (= 42 (clojure.core/locking :ignored 42)))

(println (key-test :item :item))
(println (key-test "item" "item"))
(println (not (key-test 1 2)))
(println (core/key-test :item :item))
(println (clojure.core/key-test "item" "item"))
(def source-key-test key-test)
(println (source-key-test 7 7))

(println (reduceable? [1 2]))
(println (reduceable? (list 1 2)))
(println (reduceable? #{1 2}))
(println (reduceable? (array-values 1 2)))
(println (reduceable? "ab"))
(println (reduceable? {:left 1}))
(println (not (reduceable? 1)))
(println (core/reduceable? [1]))
(println (clojure.core/reduceable? {:left 1}))
(def source-reduceable? reduceable?)
(println (source-reduceable? [1]))

(type-record source-reducible (value :int))
(extend-type source-reducible ISeqable
  (-seq [_source] (list 99)))
(extend-type source-reducible IReduce
  (-reduce [source reducer initial]
    (reducer initial (:value source))))
(def source-reducible-value
  (record source-reducible (value 7)))
(println (reduceable? source-reducible-value))
(println (= 7 (reduce + 0 source-reducible-value)))

(println (= [] (vector-lite)))
(println (= [1 2 3] (vector-lite 1 2 3)))
(println (= ["left" "right"] (core/vector-lite "left" "right")))
(def source-vector-lite vector-lite)
(println (= [:left :right] (source-vector-lite :left :right)))

(println (= {} (hash-map-lite)))
(println (= 1 (get (hash-map-lite :left 1 :right 2) :left 0)))
(println (= 3 (get (hash-map-lite :left 1 :left 3) :left 0)))
(println (= true (get (core/hash-map-lite "ready" true) "ready" false)))
(def source-hash-map-lite hash-map-lite)
(println (= 9 (get (source-hash-map-lite :value 9) :value 0)))

(println (= #{1 2} (set-lite [1 2 1])))
(println (= #{:left :right} (core/set-lite (list :left :right :left))))
(println (= #{"x"} (clojure.core/set-lite ["x" "x"])))
(def source-set-lite set-lite)
(println (= #{7 8} (source-set-lite [7 8 7])))

(println (equiv-map {} {}))
(println (equiv-map {:left 1 :right 2} {:right 2 :left 1}))
(println (not (equiv-map {:left 1} {:left 1 :right 2})))
(println (not (equiv-map {:left 1} {:right 1})))
(println (not (equiv-map {:left 1} {:left 2})))
(println (core/equiv-map {"left" true} {"left" true}))
(println (clojure.core/equiv-map {:left "value"} {:left "value"}))
(def source-equiv-map equiv-map)
(println (source-equiv-map {7 :seven} {7 :seven}))

(println (= (divide 2) (/ 1 2)))
(println (= (divide 8 2) (/ 8 2)))
(println (= (divide 64 4 2) (/ (/ 64 4) 2)))
(println (= (core/divide 27 3 3) (/ (/ 27 3) 3)))
(println (= (clojure.core/divide -12 3) (/ -12 3)))
(def source-divide-order (atom []))
(println (= (divide (do (swap! source-divide-order conj :left) 64)
                    (do (swap! source-divide-order conj :middle) 4)
                    (do (swap! source-divide-order conj :right) 2))
            (/ (/ 64 4) 2)))
(println (= [:left :middle :right] @source-divide-order))

(def reader-parser-a (fn [value] value))
(def reader-first-old
  (reader/register-tag-parser! 'lg/point reader-parser-a))
(println (nil? reader-first-old))
(def source-register-tag-parser! reader/register-tag-parser!)
(def reader-second-old
  (source-register-tag-parser! 'lg/point (fn [value] value)))
(println
 (if-some [old reader-second-old]
   (= "[1 2]" (runtime-edn/write-string (old (reader/read-string "[1 2]"))))
   false))
(println
 (= "[3 4]"
    (runtime-edn/write-string (reader/read-string "#lg/point [3 4]"))))
(def reader-removed (deregister-tag-parser! 'lg/point))
(println
 (if-some [old reader-removed]
   (= "9" (runtime-edn/write-string (old (reader/read-string "9"))))
   false))
(println
 (= "#lg/point [3 4]"
    (runtime-edn/write-string (reader/read-string "#lg/point [3 4]"))))

(def reader-default-a (fn [_tag value] value))
(def reader-default-first-old
  (cljs.reader/register-default-tag-parser! reader-default-a))
(println (nil? reader-default-first-old))
(def reader-default-second-old
  (cljs.reader/register-default-tag-parser! (fn [_tag value] value)))
(println
 (if-some [old reader-default-second-old]
   (= "7" (runtime-edn/write-string
            (old 'lg/unknown (reader/read-string "7"))))
   false))
(println
 (= "[5 6]"
    (runtime-edn/write-string (reader/read-string "#lg/unknown [5 6]"))))
(def reader-default-removed (deregister-default-tag-parser!))
(println
 (if-some [old reader-default-removed]
   (= "8" (runtime-edn/write-string
            (old 'lg/unknown (reader/read-string "8"))))
   false))
(println
 (= "#lg/unknown [5 6]"
    (runtime-edn/write-string (reader/read-string "#lg/unknown [5 6]"))))

(def walk-one (reader/read-string "1"))
(def walk-two (reader/read-string "2"))
(def walk-twenty (reader/read-string "20"))
(def walk-vector-two (reader/read-string "[2]"))
(def walk-vector-twenty (reader/read-string "[20]"))
(defn walk-replace-two [^:Lg_edn_backend.t value]
  (if (= value walk-two) walk-twenty value))
(defn walk-expand [^:Lg_edn_backend.t value]
  (if (= value walk-one)
    walk-vector-two
    (if (= value walk-two) walk-twenty value)))

(println
 (= (reader/read-string "[1 20]")
    (walk/walk walk-replace-two identity (reader/read-string "[1 2]"))))
(println
 (= (reader/read-string "[[2]]")
    (walk/postwalk walk-expand (reader/read-string "[1]"))))
(println
 (= (reader/read-string "[[20]]")
    (walk/prewalk walk-expand (reader/read-string "[1]"))))
(println
 (= (reader/read-string "{:a {:b 1}}")
    (keywordize-keys (reader/read-string "{\"a\" {\"b\" 1}}"))))
(println
 (= (reader/read-string "{\"a\" {\"b\" 1}}")
    (stringify-keys (reader/read-string "{:a {:b 1}}"))))

(def walk-replacements (reader/read-string "{1 [2], 2 20}"))
(println
 (= (reader/read-string "[[20]]")
    (prewalk-replace walk-replacements (reader/read-string "[1]"))))
(println
 (= (reader/read-string "[[2]]")
    (postwalk-replace walk-replacements (reader/read-string "[1]"))))
(def source-postwalk walk/postwalk)
(println
 (= (reader/read-string "[1 20]")
    (source-postwalk walk-replace-two (reader/read-string "[1 2]"))))
(println
 (= (reader/read-string "[1 20]")
    (clojure.walk/postwalk walk-replace-two (reader/read-string "[1 2]"))))

(def walk-zero (reader/read-string "0"))
(defn walk-collapse-numbers [^:Lg_edn_backend.t value]
  (if (= value walk-one)
    walk-zero
    (if (= value walk-two) walk-zero value)))
(println
 (= (reader/read-string "#{0}")
    (walk/walk walk-collapse-numbers identity (reader/read-string "#{1 2}"))))
(println
 (= (reader/read-string "{:a 2}")
    (keywordize-keys (reader/read-string "{\"a\" 1, :a 2}"))))

(println (= ["a" "b" "" "c"] (string/split "a,b,,c," #",")))
(println (= ["a" "b" "c,d"] (split "a,b,c,d" #"," 3)))
(println (= ["a" "1" "b" "2"]
            (clojure.string/split "a1b2" #"([0-9])")))
(println (= ["" "a" "b" "c"] (string/split "abc" #"")))
(println (= ["a" "b"] (string/split "a,b," #"," 0)))
(println (= ["a" "b" ""] (string/split "a,b," #"," -1)))
(println (= ["a,b,c"] (string/split "a,b,c" #"," 1)))
(println (= ["a" "b" "c"] (string/split "a,b,c" ",")))

(def data-one (reader/read-string "1"))
(def data-two (reader/read-string "2"))
(def source-diff diff)
(println (= (reader/read-string "[nil nil 1]")
            (source-diff data-one data-one)))
(println (= (reader/read-string "[1 2 nil]")
            (data/diff data-one data-two)))
(println
 (= (reader/read-string "[{:b 2} {:b 3, :c 4} {:a 1}]")
    (clojure.data/diff
     (reader/read-string "{:a 1, :b 2}")
     (reader/read-string "{:a 1, :b 3, :c 4}"))))
(println
 (= (reader/read-string "[[nil 2] [nil 3 4] [1]]")
    (data/diff (reader/read-string "[1 2]")
               (reader/read-string "[1 3 4]"))))
(println
 (= (reader/read-string "[#{1} #{3} #{2}]")
    (data/diff (reader/read-string "#{1 2}")
               (reader/read-string "#{2 3}"))))
(println
 (= (reader/read-string "[[1] {:a 1} nil]")
    (data/diff (reader/read-string "[1]")
               (reader/read-string "{:a 1}"))))
(println
 (= (reader/read-string "[{:a nil} nil nil]")
    (data/diff (reader/read-string "{:a nil}")
               (reader/read-string "{}"))))

(defn zip-branch? [value] (< value 4))
(defn zip-children [value] [(* value 2) (inc (* value 2))])
(defn zip-make-node [value children]
  (reduce + value children))
(def zip-root (zip/zipper zip-branch? zip-children zip-make-node 1))
(def zip-down (zip/down zip-root))
(def zip-right (zip/right zip-down))

(println (= 1 (node zip-root)))
(println (= 2 (zip/node zip-down)))
(println (= 3 (zip/node zip-right)))
(println (= [1] (zip/path zip-down)))
(println (= [] (zip/lefts zip-down)))
(println (= [3] (zip/rights zip-down)))
(println (= 2 (zip/node (zip/left zip-right))))
(println (= 3 (zip/node (zip/rightmost zip-down))))
(println (= 2 (zip/node (zip/leftmost zip-right))))
(println (= 1 (zip/node (zip/up zip-down))))
(println (= 4 (zip/node (zip/down (zip/down zip-root)))))
(println (= 2 (zip/node (zip/prev (zip/next (zip/next zip-root))))))
(println (= 5 (zip/node (zip/next (zip/next (zip/next zip-root))))))
(println (= 24 (zip/root (zip/replace zip-down 20))))
(println (= 15 (zip/root (zip/insert-right zip-down 9))))
(println (= 15 (zip/root (zip/insert-left zip-right 9))))
(println (= 15 (zip/root (zip/insert-child zip-root 9))))
(println (= 15 (zip/root (zip/append-child zip-root 9))))
(println (= 24 (zip/root (zip/edit zip-down (fn [value] (* value 10))))))
(println (= 4 (zip/root (zip/remove zip-down))))
(println (= 3 (zip/root (zip/remove zip-right))))
(def zip-end
  (zip/next
   (zip/next
    (zip/next
     (zip/next
      (zip/next
       (zip/next
        (zip/next zip-root))))))))
(println (zip/end? zip-end))
(println (= 1 (zip/root zip-end)))
(def source-zip-root zip/root)
(println (= 1 (source-zip-root zip-root)))
(println (= 1 (clojure.zip/root zip-root)))
(println (nil? (zip/up zip-root)))
(println (nil? (zip/left zip-root)))
(println (nil? (zip/down (zip/zipper (fn [_] false) zip-children zip-make-node 1))))
(println
 (try
   (do (zip/insert-left zip-root 9) false)
   (catch _ true)))
(println
 (try
   (do (zip/insert-right zip-root 9) false)
   (catch _ true)))
(println
 (try
   (do (zip/remove zip-root) false)
   (catch _ true)))

(def vector-zipper (zip/vector-zip (reader/read-string "[1 [2 3]]")))
(println (= (reader/read-string "1") (zip/node (zip/down vector-zipper))))
(println (= (reader/read-string "[2 3]")
            (zip/node (zip/right (zip/down vector-zipper)))))
(println (= (reader/read-string "[1 [20 3]]")
            (zip/root
             (zip/replace
              (zip/down (zip/right (zip/down vector-zipper)))
              (reader/read-string "20")))))
(def sequence-zipper (zip/seq-zip (reader/read-string "(1 (2 3))")))
(println (= (reader/read-string "(1 (20 3))")
            (zip/root
             (zip/replace
              (zip/down (zip/right (zip/down sequence-zipper)))
              (reader/read-string "20")))))
(def xml-zipper
  (zip/xml-zip
   (reader/read-string "{:tag :root, :content [\"a\" {:tag :child, :content [\"b\"]}]}")))
(println (= (reader/read-string "\"b\"")
            (zip/node
             (zip/down
              (zip/right
               (zip/down xml-zipper))))))

(def hierarchy-child (reader/read-string ":app/child"))
(def hierarchy-parent (reader/read-string ":app/parent"))
(def hierarchy-root (reader/read-string ":app/root"))
(def hierarchy-peer (reader/read-string ":app/peer"))
(def hierarchy-other (reader/read-string ":app/other"))
(def hierarchy-empty (make-hierarchy))
(def hierarchy-one (derive hierarchy-empty hierarchy-child hierarchy-parent))
(def hierarchy-two (derive hierarchy-one hierarchy-parent hierarchy-root))

(println (= (reader/read-string "{:parents {}, :descendants {}, :ancestors {}}")
            hierarchy-empty))
(println (= (reader/read-string "#{:app/parent}")
            (parents hierarchy-one hierarchy-child)))
(println (= (reader/read-string "#{:app/parent :app/root}")
            (ancestors hierarchy-two hierarchy-child)))
(println (= (reader/read-string "#{:app/child :app/parent}")
            (descendants hierarchy-two hierarchy-root)))
(println (isa? hierarchy-two hierarchy-child hierarchy-root))
(println (isa? hierarchy-two hierarchy-child hierarchy-child))
(println (not (isa? hierarchy-two hierarchy-child hierarchy-other)))
(println
 (isa? hierarchy-two
       (reader/read-string "[:app/child :app/parent]")
       (reader/read-string "[:app/root :app/root]")))
(println (= hierarchy-two (derive hierarchy-two hierarchy-child hierarchy-parent)))
(println (nil? (parents hierarchy-two hierarchy-other)))
(println (nil? (ancestors hierarchy-two hierarchy-other)))
(println (nil? (descendants hierarchy-two hierarchy-other)))
(println
 (try
   (do (derive hierarchy-two hierarchy-child hierarchy-root) false)
   (catch _ true)))
(println
 (try
   (do (derive hierarchy-two hierarchy-root hierarchy-child) false)
   (catch _ true)))
(println
 (try
   (do (derive hierarchy-two hierarchy-child hierarchy-child) false)
   (catch _ true)))

(def hierarchy-three (derive hierarchy-two hierarchy-child hierarchy-peer))
(def hierarchy-underived
  (underive hierarchy-three hierarchy-child hierarchy-parent))
(println (= (reader/read-string "#{:app/peer}")
            (parents hierarchy-underived hierarchy-child)))
(println (= (reader/read-string "#{:app/peer}")
            (ancestors hierarchy-underived hierarchy-child)))
(println (= (reader/read-string "#{:app/parent}")
            (descendants hierarchy-underived hierarchy-root)))
(println (= hierarchy-underived
            (underive hierarchy-underived hierarchy-child hierarchy-parent)))

(println (nil? (derive hierarchy-child hierarchy-parent)))
(println (isa? hierarchy-child hierarchy-parent))
(println (= (reader/read-string "#{:app/parent}") (parents hierarchy-child)))
(println (nil? (underive hierarchy-child hierarchy-parent)))
(println (not (isa? hierarchy-child hierarchy-parent)))

(def source-isa cljs.core/isa?)
(println (source-isa hierarchy-two hierarchy-child hierarchy-root))
(println (clojure.core/isa? hierarchy-two hierarchy-child hierarchy-root))

(def source-make-array make-array)
(def source-aget-int aget)
(def source-aget-float aget)
(def source-aset aset)
(def source-atom atom)
(def source-volatile volatile!)
(def source-array-values (source-make-array 2 0))
(def source-written-value (source-aset source-array-values 1 42))
(def source-atom-value (source-atom 7))
(def source-volatile-value (source-volatile "before"))
(reset! source-atom-value 8)
(vreset! source-volatile-value "after")
(println
 (and (= 42 source-written-value)
      (= 42 (source-aget-int source-array-values 1))
      (= 42 (source-aget-float source-array-values 1.0))
      (= 8 (deref source-atom-value))
      (= "after" (deref source-volatile-value))))

(println (= [] (into)))
(println (= [1] (into [1])))
(println (= [1 2] (core/into [1] [2])))
(println (= #{1 2} (clojure.core/into #{1} [2])))
(def collect-into into)
(println (= [1 2 3] (collect-into [1] [2 3])))

(defn invalid-reader-timestamp? [source]
  (try
    (do (reader/parse-and-validate-timestamp source) false)
    (catch _ true)))

(println (= [2020 1 1 0 0 0 0 0]
            (reader/parse-and-validate-timestamp "2020")))
(println (= [2020 2 29 23 59 60 123 0]
            (parse-and-validate-timestamp "2020-02-29T23:59:60.1234Z")))
(println (= [2019 12 31 4 5 6 100 150]
            (cljs.reader/parse-and-validate-timestamp
             "2019-12-31T04:05:06.1+02:30")))
(println (= [2019 12 31 4 5 6 7 -195]
            (reader/parse-and-validate-timestamp
             "2019-12-31T04:05:06.007-03:15")))
(println (= [2020 1 1 0 0 0 0 6039]
            (reader/parse-and-validate-timestamp "2020+99:99")))
(println (invalid-reader-timestamp? "not-a-timestamp"))
(println (invalid-reader-timestamp? "2020-13"))
(println (invalid-reader-timestamp? "2019-02-29"))
(println (invalid-reader-timestamp? "2020-01-01T24"))
(println (invalid-reader-timestamp? "2020-01-01T23:60"))
(println (invalid-reader-timestamp? "2020-01-01T23:58:60"))

(def source-sequence (seq [1 2 3]))
(println
 (and (= 1 (core/ISeq/-first source-sequence))
      (= [2 3] (vec (ISeq/-rest source-sequence)))
      (= [2 3] (vec (INext/-next source-sequence)))
      (empty? (INext/-next (seq [1])))))

(deftype SourceSequence [^:seq<int> values]
  ISeq
  (-first [source] (first (.-values source)))
  (-rest [source] (rest (.-values source)))

  INext
  (-next [source] (next (.-values source))))

(def custom-source-sequence (SourceSequence. (seq [4 5 6])))
(println
 (and (= 4 (core/ISeq/-first custom-source-sequence))
      (= [5 6] (vec (ISeq/-rest custom-source-sequence)))
      (= [5 6] (vec (INext/-next custom-source-sequence)))))
