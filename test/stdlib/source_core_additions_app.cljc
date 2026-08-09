(ns source-core-additions-app
  (:require [cljs.core :as core :refer [NaN? add-to-string-hash-cache areduce array-binary-search-left array-binary-search-right array-from array-index-of array-values bit-and bit-not bit-or bit-shift-left bit-shift-right bit-xor dec decimal? divide equiv-map flush hash-double hash-keyword hash-long hash-map-lite hash-string ifind? inc infinite? iterate key-test keyword-identical? locking map-entry? merge-with parse-double parse-long parse-uuid partitionv ratio? realized? reduceable? regexp? set-lite special-symbol? symbol-identical? tree-seq vector-lite volatile?]]
            [cljs.reader :as reader :refer [deregister-default-tag-parser! deregister-tag-parser!]]
            [clojure.data :as data :refer [diff]]
            [clojure.string :as string :refer [split]]
            [clojure.walk :as walk :refer [keywordize-keys postwalk-replace prewalk-replace stringify-keys]]
            [clojure.zip :as zip :refer [node]]
            [ocaml.Lg_runtime.Runtime_edn :as runtime-edn]))

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

(def completed-future (future-call (fn [] 42)))
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
