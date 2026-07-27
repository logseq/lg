(ns datascript.test.query
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.lg.query :as query]
    [datascript.lg.query-types :as query-types]
    [datascript.lru :as lru]
    [datascript.parser :as parser]
    [datascript.query-v3 :as query-v3]
    [datascript.test.core :as tdc]))

(defn ^:Datascript_runtime.Data_value.t query-form-vector
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:Datascript_runtime.Data_value.t query-form-list
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.List
   (into (list) (reverse values))))

(defn ^:Datascript_runtime.Data_value.t query-rule-branch
  [^:string rule-name
   ^:vector<Datascript_runtime.Data_value.t> parameters
   ^:vector<Datascript_runtime.Data_value.t> clauses]
  (query-form-vector
   (vec
    (concat
     [(query-form-vector
       (vec
        (concat
         [(Datascript_runtime.Data_value.Symbol rule-name)]
         parameters)))]
     clauses))))

(defn ^datascript.lg.query-types/result query-int-result [^:int value]
  (query-types/value-result
   (Datascript_runtime.Data_value.Int value)))

(defn ^:int query-result-int
  [^datascript.lg.query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Entity value) value
    (Datascript_runtime.Query_value.Value
     (Datascript_runtime.Data_value.Int value))
    value
    _ (Stdlib.invalid_arg "Expected an integer query result")))

(defn ^:vector<vector<int>> relation-int-rows
  [^datascript.lg.query-types/relation relation
   ^:vector<string> variables]
  (mapv
   (fn [row]
     (mapv
      (fn [^:string variable]
        (if-some [result
                  (query-types/relation-result
                   relation variable row)]
          (query-result-int result)
          (Stdlib.invalid_arg
           (str "Missing relation variable " variable))))
      variables))
   (query-types/relation-rows relation)))

(defn ^:vector<vector<Datascript_runtime.Data_value.t>>
  relation-data-rows
  [^datascript.lg.query-types/relation relation
   ^:vector<string> variables]
  (mapv
   (fn [row]
     (mapv
      (fn [^:string variable]
        (if-some [result
                  (query-types/relation-result
                   relation variable row)]
          (query-types/result-pattern-value result)
          (Stdlib.invalid_arg
           (str "Missing relation variable " variable))))
      variables))
   (query-types/relation-rows relation)))

(defn ^:vector<string> query-form-strings
  [^:vector<Datascript_runtime.Data_value.t> values]
  (mapv
   (fn [^:Datascript_runtime.Data_value.t value]
     (Datascript_runtime.Data_value.to_edn_string value))
   values))

(defn ^:vector<string> clause-variable-names
  [^:vector<datascript.parser/clause> clauses]
  (vec
   (mapcat
    (fn [^datascript.parser/clause clause]
      (mapv
       (fn [^datascript.parser/Variable variable]
         (str (.-symbol variable)))
       (datascript.parser/clause-vars clause)))
    clauses)))

(defn ^:array<datascript.lg.query-types/result> query-int-row
  [^:vector<int> values]
  (to-array (mapv query-int-result values)))

(defn ^:vector<vector<int>> query-int-rows
  [^:vector<array<datascript.lg.query-types/result>> rows]
  (mapv
   (fn [^:array<datascript.lg.query-types/result> row]
     (mapv query-result-int row))
   rows))

(defn ^:vector<vector<string>> query-output-edn-rows
  [^:vector<array<datascript.lg.query-types/result>> rows]
  (mapv
   (fn [^:array<datascript.lg.query-types/result> row]
     (mapv
      (fn [^datascript.lg.query-types/result result]
        (Datascript_runtime.Data_value.to_edn_string
         (query-types/result-pattern-value result)))
      row))
   rows))

(defn ^:option<Datascript_runtime.Data_value.t> sum-query-arguments
  [^:vector<datascript.lg.query-types/result> arguments]
  (Some
   (Datascript_runtime.Data_value.Int
    (reduce
     (fn [^:int total result]
       (+ total (query-result-int result)))
     0
     arguments))))

(defn ^:option<Datascript_runtime.Data_value.t> even-query-argument
  [^:vector<datascript.lg.query-types/result> arguments]
  (if-some [argument (first arguments)]
    (Some
     (Datascript_runtime.Data_value.Bool
      (= 0 (mod (query-result-int argument) 2))))
    None))

(defn ^:int query-call-int
  [^:option<Datascript_runtime.Data_value.t> value]
  (match value
    (Some (Datascript_runtime.Data_value.Int value)) value
    _ (Stdlib.invalid_arg "Expected an integer query call result")))

(defn ^:option<Datascript_runtime.Data_value.t> database-marker
  [^:vector<datascript.lg.query-types/result> arguments]
  (if-some [argument (first arguments)]
    (match argument
      (Datascript_runtime.Query_value.Database _database)
      (Some (Datascript_runtime.Data_value.Int 1))
      _ (Stdlib.invalid_arg "Expected a database query argument"))
    None))

(defn ^:option<Datascript_runtime.Data_value.t>
  relation-query-result
  [^:vector<datascript.lg.query-types/result> arguments]
  (if-some [argument (first arguments)]
    (let [value (query-result-int argument)]
      (Some
       (query-form-vector
        [(query-form-vector
          [(Datascript_runtime.Data_value.Int value)
           (Datascript_runtime.Data_value.Int 10)])
         (query-form-vector
          [(Datascript_runtime.Data_value.Int value)
           (Datascript_runtime.Data_value.Int 20)])])))
    None))

(defn ^:option<Datascript_runtime.Data_value.t>
  nil-query-result
  [^:vector<datascript.lg.query-types/result> _arguments]
  (Some (Datascript_runtime.Data_value.Nil)))

(defn ^:option<Datascript_runtime.Data_value.t>
  absent-query-value
  [^:vector<datascript.lg.query-types/result> _arguments]
  None)

(defn ^:vector<vector<int>> mapped-int-rows
  [^:vector<map<string;datascript.lg.query-types/result>> rows
   ^:vector<string> keys]
  (mapv
   (fn [^:map<string;datascript.lg.query-types/result> row]
     (mapv
      (fn [^:string key]
        (if-some [value (get row key)]
          (query-result-int value)
          (Stdlib.invalid_arg
           (str "Missing mapped result key " key))))
      keys))
   rows))

(defn ^:option<int> optional-query-int
  [^:option<datascript.lg.query-types/result> value]
  (match value
    None None
    (Some result) (Some (query-result-int result))))

(defn ^:option<int> absent-int []
  None)

(defn ^:option<datascript.lg.query-types/result>
  absent-query-result
  []
  None)

(defn ^:vector<vector<option<int>>> optional-int-rows
  [^:vector<array<option<datascript.lg.query-types/result>>> rows]
  (mapv
   (fn [^:array<option<datascript.lg.query-types/result>> row]
     (mapv optional-query-int row))
   rows))

(deftest test-public-map-star
  (let [increment (fn [^:int value] (+ value 1))]
    (is (= [2 3 4] (query/map* increment [1 2 3])))
    (is (= (list 4 3 2) (query/map* increment (list 1 2 3))))))

(deftest test-public-group-by-helpers
  (let [collection-calls (volatile! 0)
        grouped
        (query/-group-by
         (fn [^:int value] (mod value 2))
         []
         (do
           (vswap! collection-calls inc)
           [1 2 3 4]))
        hashed
        (query/hash-attrs
         (fn [^:int value] (mod value 2))
         [1 2 3 4])]
    (is (= 1 @collection-calls))
    (is (= [1 3] (get grouped 1)))
    (is (= [2 4] (get grouped 0)))
    (is (= (list 3 1) (get hashed 1)))
    (is (= (list 4 2) (get hashed 0)))))

(deftest test-public-query-cache
  (let [query-form
        (query-form-vector
         [(Datascript_runtime.Data_value.Keyword ":find")
          (Datascript_runtime.Data_value.Symbol "?e")
          (Datascript_runtime.Data_value.Keyword ":where")
          (query-form-vector
           [(Datascript_runtime.Data_value.Symbol "?e")
            (Datascript_runtime.Data_value.Keyword ":age")
            (Datascript_runtime.Data_value.Int 18)])])
        parse-calls (atom 0)
        parse-query
        (fn []
          (swap! parse-calls inc)
          (parser/parse-query query-form))]
    (binding [query/*query-cache* (lru/cache 2)]
      (lru/-get
       query/*query-cache* query-form parse-query)
      (lru/-get
       query/*query-cache* query-form parse-query)
      (is (= 1 @parse-calls))
      (binding [query/*query-cache* (lru/cache 2)]
        (lru/-get
         query/*query-cache* query-form parse-query)
        (is (= 2 @parse-calls)))
      (lru/-get
       query/*query-cache* query-form parse-query)
      (is (= 2 @parse-calls)))))

(deftest test-query-v3-public-collection-helpers
  (is (= [2 4 6] (vec (query-v3/mapa #(* 2 %) [1 2 3]))))
  (is (= [] (vec (query-v3/mapa inc []))))
  (is (= [2 3 4] (vec (query-v3/arange 2 5))))
  (is (= [] (vec (query-v3/arange 3 3))))
  (let [values (to-array [1 2 3 4])]
    (is (= [2 3] (vec (query-v3/subarr values 1 3))))
    (is (= [] (vec (query-v3/subarr values 2 2)))))
  (let [one (Datascript_runtime.Data_value.Int 1)
        two (Datascript_runtime.Data_value.Int 2)
        three (Datascript_runtime.Data_value.Int 3)
        four (Datascript_runtime.Data_value.Int 4)
        five (Datascript_runtime.Data_value.Int 5)
        six (Datascript_runtime.Data_value.Int 6)]
    (is (= [] (query-v3/concatv)))
    (is
     (=
      [one two three four]
      (query-v3/concatv [one two] [three] [four])))
    (is
     (=
      [[one four] [two five]]
      (vec (query-v3/zip [one two three] [four five]))))
    (is
     (=
      [[one three five] [two four six]]
      (vec (query-v3/zip [one two] [three four] [five six])))))
  (is (= (Some true) (query-v3/has? [1 2 3] 2)))
  (is (= nil (query-v3/has? [1 2 3] 4))))

(defn ^:vector<vector<int>> query-v3-int-rows
  [^datascript.query-v3/relation-v3 relation]
  (query-v3/-fold
   relation
   (fn [^:vector<vector<int>> rows
        ^:array<datascript.lg.query-types/result> row]
     (conj rows (mapv query-result-int row)))
   []))

(defn ^:vector<vector<string>> query-v3-edn-rows
  [^datascript.query-v3/relation-v3 relation]
  (query-v3/-fold
   relation
   (fn [^:vector<vector<string>> rows
        ^:array<datascript.lg.query-types/result> row]
     (conj
      rows
      (mapv
       (fn [^datascript.lg.query-types/result result]
         (Datascript_runtime.Data_value.to_edn_string
          (query-types/result-pattern-value result)))
       row)))
   []))

(defn ^:vector<array<datascript.lg.query-types/result>>
  query-v3-rows
  [^datascript.query-v3/relation-v3 relation]
  (query-v3/-fold
   relation
   (fn [^:vector<array<datascript.lg.query-types/result>> rows
        ^:array<datascript.lg.query-types/result> row]
     (conj rows row))
   []))

(deftest test-query-v3-array-relation
  (let [relation
        (query-v3/array-rel
         ["?x" "?y"]
         [(query-int-row [1 2])
          (query-int-row [3 4])])
        first-row (query-int-row [1 2])]
    (is (= ["?x" "?y"] (vec (query-v3/-symbols relation))))
    (is (= 2 (query-v3/-arity relation)))
    (is (= 2 (query-v3/-size relation)))
    (is (= 2 (query-result-int
              ((query-v3/-getter relation "?y") first-row))))
    (is (= [1 0]
           (vec (query-v3/-indexes relation ["?y" "?x"]))))
    (is (= 4
           (query-v3/-fold
            relation
            (fn [total row]
              (+ total
                 (query-result-int (aget row 0))))
            0)))
    (let [projected (query-v3/-project relation ["?y"])]
      (is (= ["?y"] (vec (query-v3/-symbols projected))))
      (is (= 1 (query-v3/-arity projected)))
      (is (= [[1 2] [3 4]] (query-v3-int-rows projected)))
      (is (= 2
             (query-result-int
              ((query-v3/-getter projected "?y") first-row)))))
    (let [altered
          (query-v3/-alter-coll
           relation
           (fn [rows]
             (subvec rows 1)))]
      (is (= [[3 4]] (query-v3-int-rows altered))))
    (let [target (query-int-row [0 0])]
      (query-v3/-copy-tuple
       relation
       first-row
       (to-array [0 1])
       target
       (to-array [1 0]))
      (is (= [2 1] (mapv query-result-int target))))
    (let [united
          (query-v3/-union
           relation
           (query-v3/array-rel
            ["?x" "?y"]
            [(query-int-row [5 6])]))]
      (is (= [[1 2] [3 4] [5 6]]
             (query-v3-int-rows united))))))

(deftest test-query-v3-coll-relation-query-rows
  (let [pattern
        [(parser/pattern-variable "?x")
         (parser/pattern-placeholder)
         (parser/pattern-variable "?y")
         (parser/pattern-constant
          (Datascript_runtime.Data_value.Int 200))]
        relation
        (query-v3/coll-rel
         pattern
         [(query-v3/CollQueryRowV3
           (query-int-row [1 100 2 200]))
          (query-v3/CollQueryRowV3
           (query-int-row [3 300 4 200]))])
        first-row (query-int-row [1 100 2 200])]
    (is (= ["?x" "?y"] (vec (query-v3/-symbols relation))))
    (is (= 2 (query-v3/-arity relation)))
    (is (= 2 (query-v3/-size relation)))
    (is (= [2 0]
           (vec (query-v3/-indexes relation ["?y" "?x"]))))
    (is (= 2
           (query-result-int
            ((query-v3/-getter relation "?y") first-row))))
    (is (= [[1 100 2 200] [3 300 4 200]]
           (query-v3-int-rows relation)))
    (let [target (query-int-row [0 0])]
      (query-v3/-copy-tuple
       relation
       first-row
       (to-array [2 0])
       target
       (to-array [0 1]))
      (is (= [2 1] (mapv query-result-int target))))
    (let [projected
          (query-v3/-project relation ["?y"])
          altered
          (query-v3/-alter-coll
           projected
           (fn [rows]
             (subvec rows 1)))
          other
          (query-v3/-project
           (query-v3/coll-rel
            pattern
            [(query-v3/CollQueryRowV3
              (query-int-row [5 500 6 200]))])
           ["?y"])
          united (query-v3/-union altered other)]
      (is (= ["?y"] (vec (query-v3/-symbols united))))
      (is (= 1 (query-v3/-arity united)))
      (is (= [[3 300 4 200] [5 500 6 200]]
             (query-v3-int-rows united))))
    (let [repeated
          (query-v3/coll-rel
           [(parser/pattern-variable "?x")
            (parser/pattern-variable "?x")]
           [(query-v3/CollQueryRowV3
             (query-int-row [10 20]))])]
      (is (= ["?x"] (vec (query-v3/-symbols repeated))))
      (is (= 20
             (query-result-int
              ((query-v3/-getter repeated "?x")
               (query-int-row [10 20]))))))
    (is
     (=
      "Cannot union relations with different kinds"
      (try
        (let [_united
              (query-v3/-union
               relation
               (query-v3/array-rel
                ["?x" "?y"]
                [(query-int-row [5 6])]))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is (= 0
           (query-v3/-size
            (query-v3/coll-rel pattern []))))))

(deftest test-query-v3-coll-relation-datom-rows
  (let [pattern
        [(parser/pattern-variable "?e")
         (parser/pattern-variable "?a")
         (parser/pattern-variable "?v")
         (parser/pattern-placeholder)
         (parser/pattern-variable "?added")]
        datom (d/datom 7 :name "Ada" 99 true)
        relation
        (query-v3/coll-rel
         pattern
         [(query-v3/CollDatomRowV3 datom)])]
    (is (= ["?e" "?a" "?v" "?added"]
           (vec (query-v3/-symbols relation))))
    (is (= 4 (query-v3/-arity relation)))
    (is (= [["7" ":name" "\"Ada\"" "99" ":db/add"]]
           (query-v3-edn-rows relation)))
    (is (= ":name"
           (Datascript_runtime.Data_value.to_edn_string
            (query-types/result-pattern-value
             ((query-v3/-getter relation "?a")
              (query-types/datom-row datom))))))
    (let [target (query-int-row [0 0 0])]
      (query-v3/-copy-tuple
       relation
       (query-types/datom-row datom)
       (to-array [0 2 4])
       target
       (to-array [0 1 2]))
      (is (= ["7" "\"Ada\"" ":db/add"]
             (mapv
              (fn [result]
                (Datascript_runtime.Data_value.to_edn_string
                 (query-types/result-pattern-value result)))
              target))))
    (let [projected (query-v3/-project relation ["?v"])]
      (is (= ["?v"] (vec (query-v3/-symbols projected))))
      (is (= [["7" ":name" "\"Ada\"" "99" ":db/add"]]
             (query-v3-edn-rows projected))))))

(deftest test-query-v3-singleton-relation
  (let [relation (query-v3/singleton-rel)
        target (query-int-row [7])]
    (is (= [] (vec (query-v3/-symbols relation))))
    (is (= 0 (query-v3/-arity relation)))
    (is (= 1 (query-v3/-size relation)))
    (is (= [[]] (query-v3-int-rows relation)))
    (is (= [] (vec (query-v3/-indexes relation []))))
    (query-v3/-copy-tuple
     relation
     (to-array [])
     (to-array [])
     target
     (to-array []))
    (is (= [7] (mapv query-result-int target)))))

(deftest test-query-v3-relation-products
  (let [left
        (query-v3/array-rel
         ["?x" "?ignored"]
         [(query-int-row [1 100])
          (query-int-row [2 200])])
        projected-left (query-v3/-project left ["?x"])
        right
        (query-v3/array-rel
         ["?y"]
         [(query-int-row [10])
          (query-int-row [20])])
        third
        (query-v3/array-rel
         ["?z"]
         [(query-int-row [30])])
        product (query-v3/product projected-left right)
        product-all
        (query-v3/product-all [projected-left right third])]
    (is (= ["?x" "?y"] (vec (query-v3/-symbols product))))
    (is (= 2 (query-v3/-arity product)))
    (is
     (=
      [[1 10] [1 20] [2 10] [2 20]]
      (query-v3-int-rows product)))
    (is (= ["?x" "?y" "?z"]
           (vec (query-v3/-symbols product-all))))
    (is
     (=
      [[1 10 30] [1 20 30] [2 10 30] [2 20 30]]
      (query-v3-int-rows product-all)))
    (is
     (=
      [[1] [2]]
      (query-v3-int-rows
       (query-v3/product
        projected-left
        (query-v3/singleton-rel)))))))

(deftest test-query-v3-hash-join-single-key
  (let [left
        (query-v3/array-rel
         ["?x" "?left"]
         [(query-int-row [1 10])
          (query-int-row [1 11])
          (query-int-row [2 20])])
        left-hash (query-v3/hash-map-rel left ["?x"])
        right
        (query-v3/array-rel
         ["?x" "?right"]
         [(query-int-row [1 100])
          (query-int-row [2 200])
          (query-int-row [1 101])
          (query-int-row [3 300])])
        joined
        (query-v3/hash-join left left-hash ["?x"] right)]
    (is (= ["?x" "?left" "?right"]
           (vec (query-v3/-symbols joined))))
    (is
     (=
      [[1 10 100]
       [1 11 100]
       [2 20 200]
       [1 10 101]
       [1 11 101]]
      (query-v3-int-rows joined)))))

(deftest test-query-v3-hash-join-composite-key
  (let [left
        (query-v3/array-rel
         ["?x" "?y" "?left"]
         [(query-int-row [1 2 10])
          (query-int-row [1 3 11])
          (query-int-row [1 2 12])])
        left-hash
        (query-v3/hash-map-rel left ["?x" "?y"])
        right
        (query-v3/array-rel
         ["?y" "?x" "?right"]
         [(query-int-row [2 1 100])
          (query-int-row [3 1 101])
          (query-int-row [2 9 999])])
        joined
        (query-v3/hash-join
         left left-hash ["?x" "?y"] right)]
    (is (= ["?x" "?y" "?left" "?right"]
           (vec (query-v3/-symbols joined))))
    (is (= [[1 2 10 100]
            [1 2 12 100]
            [1 3 11 101]]
           (query-v3-int-rows joined)))))

(deftest test-query-v3-hash-join-empty-key
  (let [left
        (query-v3/array-rel
         ["?left"]
         [(query-int-row [1])
          (query-int-row [2])])
        left-hash (query-v3/hash-map-rel left [])
        right
        (query-v3/array-rel
         ["?right"]
         [(query-int-row [10])
          (query-int-row [20])])
        joined (query-v3/hash-join left left-hash [] right)]
    (is (= ["?left" "?right"]
           (vec (query-v3/-symbols joined))))
    (is (= [[1 10] [2 10] [1 20] [2 20]]
           (query-v3-int-rows joined)))))

(deftest test-query-v3-hash-join-normalizes-datom-keys
  (let [pattern
        [(parser/pattern-variable "?e")
         (parser/pattern-variable "?a")
         (parser/pattern-variable "?v")]
        left
        (query-v3/coll-rel
         pattern
         [(query-v3/CollDatomRowV3
           (d/datom 7 :name "Ada" 99 true))])
        left-hash
        (query-v3/hash-map-rel left ["?e" "?a"])
        right-row
        (to-array
         [(query-int-result 7)
          (query-types/value-result
           (Datascript_runtime.Data_value.Keyword ":name"))
          (query-types/value-result
           (Datascript_runtime.Data_value.String "match"))])
        right
        (query-v3/array-rel
         ["?e" "?a" "?tag"]
         [right-row])
        joined
        (query-v3/hash-join
         left left-hash ["?e" "?a"] right)]
    (is (= ["?e" "?a" "?v" "?tag"]
           (vec (query-v3/-symbols joined))))
    (is (= [["7" ":name" "\"Ada\"" "\"match\""]]
           (query-v3-edn-rows joined)))))

(defn ^:bool query-v3-empty-context?
  [^datascript.query-v3/query-context-v3 context]
  (query-v3/context-empty? context))

(defn ^:vector<datascript.query-v3/relation-v3>
  query-v3-context-relations
  [^datascript.query-v3/query-context-v3 context]
  (query-v3/context-relations context))

(defn ^:map<string;datascript.lg.query-types/result>
  query-v3-context-constants
  [^datascript.query-v3/query-context-v3 context]
  (query-v3/context-constants context))

(deftest test-query-v3-join-unrelated-context
  (let [context (query-v3/context-v3 [] {})
        empty-relation (query-v3/array-rel ["?x"] [])
        singleton
        (query-v3/array-rel
         ["?x" "?y"]
         [(query-int-row [1 2])])
        multiple
        (query-v3/array-rel
         ["?z"]
         [(query-int-row [10])
          (query-int-row [20])])]
    (is (query-v3-empty-context?
         (query-v3/join-unrelated context empty-relation)))
    (let [with-constants
          (query-v3/join-unrelated context singleton)
          constants (query-v3-context-constants with-constants)]
      (is (= 0 (count (query-v3-context-relations with-constants))))
      (is (= 1
             (query-result-int
              (get constants "?x" (query-int-result 0)))))
      (is (= 2
             (query-result-int
              (get constants "?y" (query-int-result 0))))))
    (let [with-relation
          (query-v3/join-unrelated context multiple)]
      (is (= 1 (count (query-v3-context-relations with-relation))))
      (is (= [[10] [20]]
             (query-v3-int-rows
              (nth
               (query-v3-context-relations with-relation)
               0)))))))

(deftest test-query-v3-context-hash-join
  (let [related
        (query-v3/array-rel
         ["?x" "?left"]
         [(query-int-row [1 10])
          (query-int-row [2 20])])
        unrelated
        (query-v3/array-rel
         ["?z"]
         [(query-int-row [7])
          (query-int-row [8])])
        context
        (query-v3/context-v3 [related unrelated] {})
        incoming
        (query-v3/array-rel
         ["?x" "?right"]
         [(query-int-row [2 200])
          (query-int-row [1 100])])
        joined-context
        (query-v3/hash-join-rel context incoming)
        relations (query-v3-context-relations joined-context)]
    (is (= 2 (count relations)))
    (is (= ["?z"] (vec (query-v3/-symbols (nth relations 0)))))
    (is (= [[7] [8]] (query-v3-int-rows (nth relations 0))))
    (is (= ["?x" "?left" "?right"]
           (vec (query-v3/-symbols (nth relations 1)))))
    (is (= [[2 20 200] [1 10 100]]
           (query-v3-int-rows (nth relations 1))))
    (is (= 1
           (count
            (query-v3/related-rels
             joined-context ["?right"]))))
    (match
     (query-v3/extract-rels joined-context ["?right"])
     (tuple extracted remaining)
     (do
       (match extracted
         None (is false)
         (Some extracted)
         (is (= 1 (count extracted))))
       (is (= 1
              (count
               (query-v3-context-relations remaining))))))))

(deftest test-query-v3-context-hash-join-multiple-relations
  (let [left
        (query-v3/array-rel
         ["?x" "?left"]
         [(query-int-row [1 10])
          (query-int-row [2 20])])
        middle
        (query-v3/array-rel
         ["?y" "?middle"]
         [(query-int-row [3 30])
          (query-int-row [4 40])])
        unrelated
        (query-v3/array-rel
         ["?z"]
         [(query-int-row [9])
          (query-int-row [10])])
        context
        (query-v3/context-v3 [left unrelated middle] {})
        incoming
        (query-v3/array-rel
         ["?x" "?y" "?right"]
         [(query-int-row [2 3 200])
          (query-int-row [1 4 100])])
        joined-context
        (query-v3/hash-join-rel context incoming)
        relations (query-v3-context-relations joined-context)]
    (is (= 2 (count relations)))
    (is (= ["?z"] (vec (query-v3/-symbols (nth relations 0)))))
    (is (= ["?x" "?left" "?y" "?middle" "?right"]
           (vec (query-v3/-symbols (nth relations 1)))))
    (is (= [[2 20 3 30 200]
            [1 10 4 40 100]]
           (query-v3-int-rows (nth relations 1))))
    (is
     (query-v3-empty-context?
      (query-v3/hash-join-rel
       context
       (query-v3/array-rel
        ["?x"]
        [(query-int-row [99])]))))))

(deftest test-query-v3-context-sources
  (let [database-value
        (d/db-with
         (d/empty-db)
         [[:db/add 1 :name "Ada"]])
        database (db/database-view database-value)
        rows [(query-int-row [1 20])]
        context
        (query-v3/context-v3
         []
         {}
         {"$" (query-types/database-source database)
          "$rows" (query-types/relation-source rows)})]
    (is
     (some?
      (query-types/source-database
       (query-v3/get-source
        context parser/DefaultSource))))
    (is
     (=
      1
      (match
       (query-types/source-rows
        (query-v3/get-source
         context
         (parser/ExplicitSource
          (datascript.parser/SrcVar. (symbol "$rows")))))
       None 0
       (Some source-rows) (count source-rows))))
    (is
     (=
      "Source $missing is not defined"
      (try
        (let [_source
              (query-v3/get-source
               context
               (parser/ExplicitSource
                (datascript.parser/SrcVar.
                 (symbol "$missing"))))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-resolve-pattern-db
  (let [database-value
        (d/db-with
         (d/empty-db)
         [[:db/add 1 :name "Ada"]
          [:db/add 2 :name "Grace"]
          [:db/add 3 :age 42]])
        database (db/database-view database-value)
        clause
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :name)
          (parser/pattern-variable "?name")])
        relation (query-v3/resolve-pattern-db database clause)]
    (is (= ["?e" "?name"] (vec (query-v3/-symbols relation))))
    (is (= 2 (query-v3/-size relation)))
    (is (= ["1" "2"]
           (mapv
            (fn [row]
              (Datascript_runtime.Data_value.to_edn_string
               (query-types/result-pattern-value
                ((query-v3/-getter relation "?e") row))))
            (query-v3-rows relation))))
    (is (= ["\"Ada\"" "\"Grace\""]
           (mapv
            (fn [row]
              (Datascript_runtime.Data_value.to_edn_string
               (query-types/result-pattern-value
                ((query-v3/-getter relation "?name") row))))
            (query-v3-rows relation))))
    (is (= 0
           (query-v3/-size
            (query-v3/resolve-pattern-db
             database
             (parser/pattern-clause
              [(parser/pattern-variable "?e")
               (parser/pattern-attribute :name)
               (parser/pattern-constant
                (Datascript_runtime.Data_value.String
                 "Missing"))])))))))

(deftest test-query-v3-resolve-pattern-relation-source
  (let [rows
        [(query-int-row [1 20 100])
         (query-int-row [2 30 200])
         (query-int-row [3 20 300])]
        source (query-types/relation-source rows)
        clause
        (parser/explicit-pattern-clause
         "$rows"
         [(parser/pattern-variable "?x")
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Int 20))
          (parser/pattern-placeholder)])
        relation
        (query-v3/resolve-pattern-coll source clause)]
    (is (= ["?x"] (vec (query-v3/-symbols relation))))
    (is (= [[1 20 100] [3 20 300]]
           (query-v3-int-rows relation)))
    (let [repeated
          (query-v3/resolve-pattern-coll
           source
           (parser/explicit-pattern-clause
            "$rows"
            [(parser/pattern-variable "?x")
             (parser/pattern-variable "?x")]))]
      (is (= [[1 20 100] [2 30 200] [3 20 300]]
             (query-v3-int-rows repeated)))
      (is (= 20
             (query-result-int
              ((query-v3/-getter repeated "?x")
               (nth rows 0))))))))

(deftest test-query-v3-resolve-pattern-context
  (let [rows
        [(query-int-row [1 10])
         (query-int-row [2 20])
         (query-int-row [3 30])]
        existing
        (query-v3/array-rel
         ["?x" "?left"]
         [(query-int-row [1 100])
          (query-int-row [3 300])
          (query-int-row [3 301])])
        context
        (query-v3/context-v3
         [existing]
         {"?wanted" (query-int-result 30)}
         {"$rows" (query-types/relation-source rows)})
        clause
        (parser/explicit-pattern-clause
         "$rows"
         [(parser/pattern-variable "?x")
          (parser/pattern-variable "?wanted")])
        resolved (query-v3/resolve-pattern context clause)
        relations (query-v3-context-relations resolved)]
    (is (= 1 (count relations)))
    (is (= ["?x" "?left"]
           (vec (query-v3/-symbols (nth relations 0)))))
    (is (= [[3 300] [3 301]]
           (query-v3-int-rows (nth relations 0))))))

(deftest test-query-v3-resolve-predicate-constant-only
  (let [context
        (query-v3/context-v3
         []
         {"?limit" (query-int-result 2)})
        true-clause
        (parser/static-predicate-clause
         ">"
         [(parser/variable-argument "?limit")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 1))])
        false-clause
        (parser/static-predicate-clause
         "<"
         [(parser/variable-argument "?limit")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 1))])]
    (is (not
         (query-v3-empty-context?
          (query-v3/resolve-predicate context true-clause))))
    (is
     (query-v3-empty-context?
      (query-v3/resolve-predicate context false-clause)))))

(deftest test-query-v3-resolve-predicate-single-relation
  (let [related
        (query-v3/array-rel
         ["?x"]
         [(query-int-row [1])
          (query-int-row [2])
          (query-int-row [3])])
        unrelated
        (query-v3/array-rel
         ["?tag"]
         [(query-int-row [10])
          (query-int-row [20])])
        context (query-v3/context-v3 [related unrelated] {})
        clause
        (parser/static-predicate-clause
         ">"
         [(parser/variable-argument "?x")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 1))])
        resolved (query-v3/resolve-predicate context clause)
        relations (query-v3-context-relations resolved)]
    (is (= 2 (count relations)))
    (is (= ["?tag"] (vec (query-v3/-symbols (nth relations 0)))))
    (is (= [[10] [20]] (query-v3-int-rows (nth relations 0))))
    (is (= ["?x"] (vec (query-v3/-symbols (nth relations 1)))))
    (is (= [[2] [3]] (query-v3-int-rows (nth relations 1))))))

(deftest test-query-v3-resolve-predicate-multiple-relations
  (let [left
        (query-v3/array-rel
         ["?x"]
         [(query-int-row [1])
          (query-int-row [3])])
        right
        (query-v3/array-rel
         ["?y"]
         [(query-int-row [2])
          (query-int-row [4])])
        context (query-v3/context-v3 [left right] {})
        clause
        (parser/static-predicate-clause
         "<"
         [(parser/variable-argument "?x")
          (parser/variable-argument "?y")])
        resolved (query-v3/resolve-predicate context clause)
        relations (query-v3-context-relations resolved)]
    (is (= 1 (count relations)))
    (is (= ["?x" "?y"]
           (vec (query-v3/-symbols (nth relations 0)))))
    (is (= [[1 2] [1 4] [3 4]]
           (query-v3-int-rows (nth relations 0))))))

(deftest test-query-v3-resolve-predicate-database-source
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 2 :age 42]]))
        entities
        (query-v3/array-rel
         ["?e"]
         [(query-int-row [1])
          (query-int-row [2])
          (query-int-row [3])])
        context
        (query-v3/context-v3
         [entities]
         {}
         {"$" (query-types/database-source database)})
        clause
        (parser/static-predicate-clause
         "missing?"
         [(parser/source-argument "$")
          (parser/variable-argument "?e")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Keyword ":age"))])
        resolved (query-v3/resolve-predicate context clause)
        relations (query-v3-context-relations resolved)]
    (is (= 1 (count relations)))
    (is (= [[1] [3]]
           (query-v3-int-rows (nth relations 0))))))

(deftest test-query-v3-resolve-predicate-variable-callable
  (let [callable
        (query-types/callable even-query-argument)
        values
        (query-v3/array-rel
         ["?x"]
         [(query-int-row [1])
          (query-int-row [2])
          (query-int-row [3])
          (query-int-row [4])])
        context
        (query-v3/context-v3
         [values]
         {"?predicate" (query-types/callable-result callable)})
        clause
        (parser/variable-predicate-clause
         "?predicate"
         [(parser/variable-argument "?x")])
        resolved (query-v3/resolve-predicate context clause)
        relations (query-v3-context-relations resolved)]
    (is (= 1 (count relations)))
    (is (= [[2] [4]]
           (query-v3-int-rows (nth relations 0))))))

(deftest test-query-v3-resolve-predicate-errors
  (let [context (query-v3/context-v3 [] {})]
    (is
     (=
      "Unknown built-in unknown-predicate"
      (try
        (let [_resolved
              (query-v3/resolve-predicate
               context
               (parser/static-predicate-clause
                "unknown-predicate"
                []))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Unknown function ?predicate"
      (try
        (let [_resolved
              (query-v3/resolve-predicate
               context
               (parser/variable-predicate-clause
                "?predicate"
                []))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Unbound source variable: $missing"
      (try
        (let [_resolved
              (query-v3/resolve-predicate
               context
               (parser/static-predicate-clause
                "missing?"
                [(parser/source-argument "$missing")
                 (parser/constant-argument
                  (Datascript_runtime.Data_value.Int 1))
                 (parser/constant-argument
                  (Datascript_runtime.Data_value.Keyword ":age"))]))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Insufficient bindings: #{?missing}"
      (try
        (let [_resolved
              (query-v3/resolve-predicate
               context
               (parser/static-predicate-clause
                ">"
                [(parser/variable-argument "?missing")
                 (parser/constant-argument
                  (Datascript_runtime.Data_value.Int 1))]))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-project-relation-and-context
  (let [xy
        (query-v3/array-rel
         ["?x" "?y"]
         [(query-int-row [1 10])
          (query-int-row [2 20])])
        z
        (query-v3/array-rel
         ["?z"]
         [(query-int-row [30])
          (query-int-row [40])])
        context
        (query-v3/context-v3
         [xy z]
         {"?limit" (query-int-result 2)
          "?drop" (query-int-result 99)}
         {"$rows"
          (query-types/relation-source
           [(query-int-row [1])])}
         "$rows")]
    (match (query-v3/project-rel xy ["?x" "?y" "?extra"])
      None (is false)
      (Some unchanged)
      (is (= ["?x" "?y"] (vec (query-v3/-symbols unchanged)))))
    (match (query-v3/project-rel xy ["?z"])
      None (is true)
      (Some _) (is false))
    (match (query-v3/project-rel xy ["?x"])
      None (is false)
      (Some projected)
      (do
       (is (= ["?x"] (vec (query-v3/-symbols projected))))
       (is (= [1 2]
              (mapv
               (fn [row]
                 (query-result-int
                  ((query-v3/-getter projected "?x") row)))
               (query-v3-rows projected))))))
    (let [projected-context
          (query-v3/project-context context ["?x" "?limit"])
          relations
          (query-v3-context-relations projected-context)
          constants
          (query-v3-context-constants projected-context)]
      (is (= 1 (count relations)))
      (is (= ["?x"] (vec (query-v3/-symbols (nth relations 0)))))
      (is (= 1 (count constants)))
      (is (= 2
             (query-result-int
              (get constants "?limit" (query-int-result 0)))))
      (is (= "$rows"
             (query-v3/context-default-source-symbol
              projected-context)))
      (is (= 1
             (count
              (query-v3/context-sources projected-context)))))
    (is
     (query-v3-empty-context?
      (query-v3/project-context query-v3/empty-context ["?x"])))))

(deftest test-query-v3-check-bound
  (let [context
        (query-v3/context-v3
         [(query-v3/array-rel
           ["?x"]
           [(query-int-row [1])
            (query-int-row [2])])]
         {"?limit" (query-int-result 2)}
         {"$rows"
          (query-types/relation-source
           [(query-int-row [1])])})]
    (is (= (Stdlib.ignore 0)
           (query-v3/check-bound
            context
            ["?x" "?limit" "$rows"]
            "test form")))
    (is
     (=
      "Insufficient bindings: #{?missing $missing} not bound in test form"
      (try
        (let [_result
              (query-v3/check-bound
               context
               ["?missing" "$missing" "?missing"]
               "test form")]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-update-default-source
  (let [context
        (query-v3/context-v3
         []
         {}
         {"$" (query-types/relation-source [])
          "$rows" (query-types/relation-source [])})
        explicit
        (parser/explicit-pattern-clause
         "$rows"
         [(parser/pattern-variable "?x")])
        default
        (parser/pattern-clause
         [(parser/pattern-variable "?x")])
        predicate
        (parser/static-predicate-clause
         ">"
         [(parser/constant-argument
           (Datascript_runtime.Data_value.Int 2))
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 1))])]
    (is (= "$rows"
           (query-v3/context-default-source-symbol
            (query-v3/upd-default-source context explicit))))
    (is (= "$"
           (query-v3/context-default-source-symbol
            (query-v3/upd-default-source context default))))
    (is (= "$"
           (query-v3/context-default-source-symbol
            (query-v3/upd-default-source context predicate))))))

(deftest test-query-v3-collect-opt
  (let [single-context
        (query-v3/context-v3
         [(query-v3/array-rel
           ["?x"]
           [(query-int-row [1])
            (query-int-row [1])
            (query-int-row [2])])]
         {})
        constant-context
        (query-v3/context-v3
         []
         {"?x" (query-int-result 7)})
        composite-context
        (query-v3/context-v3
         [(query-v3/array-rel
           ["?x"]
           [(query-int-row [1])
            (query-int-row [2])])
          (query-v3/array-rel
           ["?y"]
           [(query-int-row [10])
            (query-int-row [20])])]
         {"?z" (query-int-result 5)})
        single-keys (query-v3/collect-opt single-context ["?x"])
        constant-keys (query-v3/collect-opt constant-context ["?x"])
        composite-keys
        (query-v3/collect-opt
         composite-context
         ["?x" "?y" "?z"])]
    (is (= 2 (count single-keys)))
    (is (= [[3]]
           (query-v3-int-rows
            (query-v3/subtract-from-rel
             (query-v3/array-rel
              ["?x"]
              [(query-int-row [1])
               (query-int-row [2])
               (query-int-row [3])])
             ["?x"]
             single-keys))))
    (is (= 1 (count constant-keys)))
    (is (= [[6] [8]]
           (query-v3-int-rows
            (query-v3/subtract-from-rel
             (query-v3/array-rel
              ["?x"]
              [(query-int-row [6])
               (query-int-row [7])
               (query-int-row [8])])
             ["?x"]
             constant-keys))))
    (is (= 4 (count composite-keys)))
    (is (= [[1 99 5]]
           (query-v3-int-rows
            (query-v3/subtract-from-rel
             (query-v3/array-rel
              ["?x" "?y" "?z"]
              [(query-int-row [1 10 5])
               (query-int-row [1 99 5])
               (query-int-row [2 20 5])])
             ["?x" "?y" "?z"]
             composite-keys))))
    (is
     (=
      "Insufficient bindings: #{?x} not bound in collect-opt"
      (try
        (let [_keys
              (query-v3/collect-opt
               query-v3/empty-context
               ["?x"])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-subtract-from-relation
  (let [relation
        (query-v3/array-rel
         ["?x" "?value"]
         [(query-int-row [1 10])
          (query-int-row [2 20])
          (query-int-row [3 30])])
        exclude-context
        (query-v3/context-v3
         [(query-v3/array-rel
           ["?x"]
           [(query-int-row [2])])]
         {})
        excluded (query-v3/collect-opt exclude-context ["?x"])
        remaining
        (query-v3/subtract-from-rel relation ["?x"] excluded)]
    (is (= ["?x" "?value"]
           (vec (query-v3/-symbols remaining))))
    (is (= [[1 10] [3 30]]
           (query-v3-int-rows remaining)))))

(deftest test-query-v3-subtract-contexts
  (let [entities
        (query-v3/array-rel
         ["?e"]
         [(query-int-row [1])
          (query-int-row [2])
          (query-int-row [3])])
        tags
        (query-v3/array-rel
         ["?tag"]
         [(query-int-row [10])
          (query-int-row [20])])
        context
        (query-v3/context-v3 [entities tags] {})
        excluded
        (query-v3/context-v3
         [(query-v3/array-rel
           ["?e"]
           [(query-int-row [2])
            (query-int-row [3])])]
         {})
        remaining
        (query-v3/subtract-contexts
         context excluded ["?e"])
        constants (query-v3-context-constants remaining)
        relations (query-v3-context-relations remaining)]
    (is (= 1
           (query-result-int
            (get constants "?e" (query-int-result 0)))))
    (is (= 1 (count relations)))
    (is (= [[10] [20]]
           (query-v3-int-rows (nth relations 0))))
    (is (= [[1] [2] [3]]
           (query-v3-int-rows
            (nth
             (query-v3-context-relations
              (query-v3/subtract-contexts
               context query-v3/empty-context ["?e"]))
             0))))
    (is
     (query-v3-empty-context?
      (query-v3/subtract-contexts
       (query-v3/context-v3
        []
        {"?constant" (query-int-result 1)})
       (query-v3/context-v3
        []
        {"?constant" (query-int-result 1)})
       ["?constant"])))
    (is
     (query-v3-empty-context?
      (query-v3/subtract-contexts
       context
       (query-v3/context-v3
        [(query-v3/array-rel
          ["?e"]
          [(query-int-row [1])
           (query-int-row [2])
           (query-int-row [3])])]
        {})
       ["?e"])))))

(deftest test-query-v3-resolve-clauses-sequential-and-short-circuit
  (let [rows
        [(query-int-row [1])
         (query-int-row [2])
         (query-int-row [3])]
        pattern
        (parser/explicit-pattern-clause
         "$rows"
         [(parser/pattern-variable "?x")])
        predicate
        (parser/static-predicate-clause
         ">"
         [(parser/variable-argument "?x")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 1))])
        context
        (query-v3/context-v3
         []
         {}
         {"$rows" (query-types/relation-source rows)})
        resolved
        (query-v3/resolve-clauses
         context
         [pattern predicate])
        nested
        (query-v3/resolve-clauses
         context
         [(parser/static-and-clause [pattern predicate])])]
    (is (= [[2] [3]]
           (query-v3-int-rows
            (nth (query-v3-context-relations resolved) 0))))
    (is (= [[2] [3]]
           (query-v3-int-rows
            (nth (query-v3-context-relations nested) 0)))))
  (let [false-predicate
        (parser/static-predicate-clause
         ">"
         [(parser/constant-argument
           (Datascript_runtime.Data_value.Int 1))
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 2))])
        unresolved-pattern
        (parser/pattern-clause
         [(parser/pattern-variable "?never")])]
    (is
     (query-v3-empty-context?
      (query-v3/resolve-clauses
       (query-v3/context-v3 [] {})
       [false-predicate unresolved-pattern])))))

(deftest test-query-v3-resolve-not
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 2 :blocked true]]))
        entities
        (query-v3/array-rel
         ["?e"]
         [(query-int-row [1])
          (query-int-row [2])
          (query-int-row [3])])
        context
        (query-v3/context-v3
         [entities]
         {}
         {"$" (query-types/database-source database)})
        blocked-pattern
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :blocked)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Bool true))])
        clause
        (parser/static-not-clause
         [blocked-pattern]
         "(not [?e :blocked true])")
        resolved (query-v3/resolve-not context clause)]
    (is (= [[1] [3]]
           (query-v3-int-rows
            (nth (query-v3-context-relations resolved) 0))))
    (is
     (query-v3-empty-context?
      (query-v3/resolve-not
       (query-v3/context-v3
        []
        {"?e" (query-int-result 2)}
        {"$" (query-types/database-source database)})
       clause)))
    (let [unblocked
          (query-v3/resolve-not
           (query-v3/context-v3
            []
            {"?e" (query-int-result 1)}
            {"$" (query-types/database-source database)})
           clause)]
      (is (not (query-v3-empty-context? unblocked)))
      (is (= 1
             (query-result-int
              (get
               (query-v3-context-constants unblocked)
               "?e"
               (query-int-result 0))))))))

(deftest test-query-v3-resolve-not-explicit-source
  (let [blocked-rows
        [(to-array
          [(query-int-result 2)
           (query-types/value-result
            (Datascript_runtime.Data_value.Bool true))])]
        entities
        (query-v3/array-rel
         ["?e"]
         [(query-int-row [1])
          (query-int-row [2])
          (query-int-row [3])])
        context
        (query-v3/context-v3
         [entities]
         {}
         {"$blocked"
          (query-types/relation-source blocked-rows)})
        nested-pattern
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Bool true))])
        clause
        (parser/static-source-not-clause
         "$blocked"
         [nested-pattern]
         "(not $blocked [?e true])")
        resolved
        (query-v3/resolve-not context clause)]
    (is (= [[1] [3]]
           (query-v3-int-rows
            (nth (query-v3-context-relations resolved) 0))))))

(deftest test-query-v3-resolve-not-errors
  (let [clause
        (parser/static-not-clause
         [(parser/pattern-clause
           [(parser/pattern-variable "?missing")])]
         "(not [?missing])")]
    (is
     (=
      "Insufficient bindings: #{?missing} not bound in (not [?missing])"
      (try
        (let [_resolved
              (query-v3/resolve-not
               (query-v3/context-v3 [] {})
               clause)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-resolve-or
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :kind "a"]
           [:db/add 2 :kind "b"]
           [:db/add 1 :also true]]))
        context
        (query-v3/context-v3
         []
         {}
         {"$" (query-types/database-source database)})
        kind-a
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :kind)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.String "a"))])
        kind-b
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :kind)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.String "b"))])
        also
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :also)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Bool true))])
        clause
        (parser/static-or-clause
         [kind-a kind-b]
         "(or [?e :kind \"a\"] [?e :kind \"b\"])")
        resolved (query-v3/resolve-or context clause)]
    (is (= [[1] [2]]
           (query-v3-int-rows
            (nth (query-v3-context-relations resolved) 0))))
    (is (= [[1] [2]]
           (query-v3-int-rows
            (nth
             (query-v3-context-relations
              (query-v3/-resolve-clause clause context))
             0))))
    (let [duplicates
          (query-v3/resolve-or
           context
           (parser/static-or-clause
            [kind-a also]
            "(or [?e :kind \"a\"] [?e :also true])"))]
      (is (= [[1] [1]]
             (query-v3-int-rows
              (nth
               (query-v3-context-relations duplicates)
               0)))))
    (is
     (query-v3-empty-context?
      (query-v3/resolve-or
       context
       (parser/static-or-clause
        [(parser/pattern-clause
          [(parser/pattern-variable "?e")
           (parser/pattern-attribute :missing-a)])
         (parser/pattern-clause
          [(parser/pattern-variable "?e")
           (parser/pattern-attribute :missing-b)])]
        "(or [?e :missing-a] [?e :missing-b])"))))
    (let [constant-context
          (query-v3/context-v3
           []
           {"?e" (query-int-result 1)}
           {"$" (query-types/database-source database)})
          constant-result
          (query-v3/resolve-or constant-context clause)]
      (is (not (query-v3-empty-context? constant-result)))
      (is (= 1
             (query-result-int
              (get
               (query-v3-context-constants constant-result)
               "?e"
               (query-int-result 0))))))))

(deftest test-query-v3-resolve-or-join
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :a 10]
           [:db/add 2 :b 20]]))
        entities
        (query-v3/array-rel
         ["?e"]
         [(query-int-row [1])
          (query-int-row [2])])
        context
        (query-v3/context-v3
         [entities]
         {}
         {"$" (query-types/database-source database)})
        branch-a
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :a)
          (parser/pattern-variable "?value")])
        branch-b
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :b)
          (parser/pattern-variable "?value")])
        clause
        (parser/static-or-join-clause
         ["?e"]
         ["?value"]
         [branch-a branch-b]
         "(or-join [?e ?value] [?e :a ?value] [?e :b ?value])")
        resolved
        (query-v3/resolve-or context clause)]
    (is (= [[1 10] [2 20]]
           (query-v3-int-rows
            (nth (query-v3-context-relations resolved) 0)))))
  (let [clause
        (parser/static-or-join-clause
         ["?required"]
         ["?value"]
         [(parser/pattern-clause
           [(parser/pattern-variable "?required")
            (parser/pattern-attribute :value)
            (parser/pattern-variable "?value")])]
         "(or-join [?required ?value] ...)")]
    (is
     (=
      "Insufficient bindings: #{?required} not bound in (or-join [?required ?value] ...)"
      (try
        (let [_resolved
              (query-v3/resolve-or
               (query-v3/context-v3 [] {})
               clause)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-resolve-or-explicit-source
  (let [rows
        [(query-int-row [1 10])
         (query-int-row [2 20])
         (query-int-row [3 30])]
        context
        (query-v3/context-v3
         []
         {}
         {"$rows" (query-types/relation-source rows)})
        branch-10
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Int 10))])
        branch-20
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Int 20))])
        clause
        (parser/static-source-or-clause
         "$rows"
         [branch-10 branch-20]
         "(or $rows [?e 10] [?e 20])")
        resolved (query-v3/resolve-or context clause)]
    (is (= [[1] [2]]
           (query-v3-int-rows
            (nth (query-v3-context-relations resolved) 0))))))

(deftest test-query-v3-bind-input-values
  (let [scalar
        (query-v3/bind
         (parser/scalar-input "?x")
         (query-types/scalar-binding
          (query-int-result 7)))
        ignored
        (query-v3/bind
         (parser/ignore-input)
         (query-types/scalar-binding
          (query-int-result 99)))
        tuple
        (query-v3/bind
         (parser/tuple-input
          [(parser/scalar-input "?x")
           (parser/scalar-input "?y")])
         (query-types/collection-binding
          [(query-types/scalar-binding
            (query-int-result 1))
           (query-types/scalar-binding
            (query-int-result 2))]))
        collection
        (query-v3/bind
         (parser/collection-input
          (parser/scalar-input "?x"))
         (query-types/collection-binding
          [(query-types/scalar-binding
            (query-int-result 1))
           (query-types/scalar-binding
            (query-int-result 2))
           (query-types/scalar-binding
            (query-int-result 3))]))
        empty-collection
        (query-v3/bind
         (parser/collection-input
          (parser/scalar-input "?x"))
         (query-types/collection-binding []))]
    (is (= ["?x"] (vec (query-v3/-symbols scalar))))
    (is (= [[7]] (query-v3-int-rows scalar)))
    (is (= [] (vec (query-v3/-symbols ignored))))
    (is (= [[]] (query-v3-int-rows ignored)))
    (is (= ["?x" "?y"] (vec (query-v3/-symbols tuple))))
    (is (= [[1 2]] (query-v3-int-rows tuple)))
    (is (= [[1] [2] [3]]
           (query-v3-int-rows collection)))
    (is (= ["?x"]
           (vec (query-v3/-symbols empty-collection))))
    (is (= 0 (query-v3/-size empty-collection)))))

(deftest test-query-v3-bind-input-errors
  (is
   (=
    "Scalar query input requires a Scalar_binding"
    (try
      (let [_relation
            (query-v3/bind
             (parser/scalar-input "?x")
             (query-types/collection-binding []))]
        "no error")
      (catch (Invalid_argument message)
        (str message)))))
  (is
   (=
    "Tuple query input has too few values"
    (try
      (let [_relation
            (query-v3/bind
             (parser/tuple-input
              [(parser/scalar-input "?x")
               (parser/scalar-input "?y")])
             (query-types/collection-binding
              [(query-types/scalar-binding
                (query-int-result 1))]))]
        "no error")
      (catch (Invalid_argument message)
        (str message))))))

(deftest test-query-v3-resolve-inputs
  (let [source
        (query-types/relation-source
         [(query-int-row [10])
          (query-int-row [20])])
        descriptors
        [(parser/make-static-source-input "$rows")
         (parser/make-static-value-input
          (parser/scalar-input "?limit"))
         (parser/make-static-value-input
          (parser/collection-input
           (parser/scalar-input "?x")))]
        inputs
        [(query-types/source-input source)
         (query-types/binding-input
          (query-types/scalar-binding
           (query-int-result 2)))
         (query-types/binding-input
          (query-types/collection-binding
           [(query-types/scalar-binding
             (query-int-result 1))
            (query-types/scalar-binding
             (query-int-result 2))
            (query-types/scalar-binding
             (query-int-result 3))]))]
        resolved
        (query-v3/resolve-ins
         (query-v3/context-v3 [] {})
         descriptors
         inputs)
        constants (query-v3-context-constants resolved)
        relations (query-v3-context-relations resolved)]
    (is (= 1 (count (query-v3/context-sources resolved))))
    (is (= 2
           (query-result-int
            (get constants "?limit" (query-int-result 0)))))
    (is (= 1 (count relations)))
    (is (= [[1] [2] [3]]
           (query-v3-int-rows (nth relations 0))))))

(deftest test-query-v3-resolve-input-errors
  (let [context (query-v3/context-v3 [] {})]
    (is
     (=
      "Wrong number of query inputs: 1 required, 0 provided"
      (try
        (let [_resolved
              (query-v3/resolve-ins
               context
               [(parser/make-static-source-input "$")]
               [])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Source query input requires a Source_input"
      (try
        (let [_resolved
              (query-v3/resolve-ins
               context
               [(parser/make-static-source-input "$")]
               [(query-types/binding-input
                 (query-types/scalar-binding
                  (query-int-result 1)))])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Rules query input requires a Rules_input"
      (try
        (let [_resolved
              (query-v3/resolve-ins
               context
               [(parser/make-static-rules-input)]
               [(query-types/binding-input
                 (query-types/scalar-binding
                  (query-int-result 1)))])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(defn ^datascript.parser/Query query-v3-collection-input-query
  [^datascript.parser/find-spec find
   ^:string variable]
  (parser/static-query-clauses-with-inputs
   find
   []
   [(parser/make-static-value-input
     (parser/collection-input
      (parser/scalar-input variable)))]))

(defn ^datascript.lg.query-types/input query-v3-int-collection-input
  [^:vector<int> values]
  (query-types/binding-input
   (query-types/collection-binding
    (mapv
     (fn [^:int value]
       (query-types/scalar-binding
        (query-int-result value)))
     values))))

(defn ^datascript.lg.query-types/input query-v3-data-collection-input
  [^:vector<Datascript_runtime.Data_value.t> values]
  (query-types/binding-input
   (query-types/collection-binding
    (mapv
     (fn [^:Datascript_runtime.Data_value.t value]
       (query-types/scalar-binding
        (query-types/value-result value)))
     values))))

(defn ^datascript.lg.query-types/input
  query-v3-data-tuple-collection-input
  [^:vector<vector<Datascript_runtime.Data_value.t>> rows]
  (query-types/binding-input
   (query-types/collection-binding
    (mapv
     (fn [^:vector<Datascript_runtime.Data_value.t> row]
       (query-types/collection-binding
        (mapv
         (fn [^:Datascript_runtime.Data_value.t value]
           (query-types/scalar-binding
            (query-types/value-result value)))
         row)))
     rows))))

(defn ^:Datascript_runtime.Data_value.t query-v3-lookup-ref
  [^:keyword attr ^:string value]
  (query-form-vector
   [(Datascript_runtime.Data_value.Keyword (str attr))
    (Datascript_runtime.Data_value.String value)]))

(defn ^:vector<array<datascript.lg.query-types/result>>
  require-query-v3-relation-output
  [^datascript.lg.query-types/output output]
  (if-some [rows (query-types/output-relation output)]
    rows
    (Stdlib.invalid_arg "Expected query-v3 relation output")))

(defn ^:vector<datascript.lg.query-types/result>
  require-query-v3-collection-output
  [^datascript.lg.query-types/output output]
  (if-some [values (query-types/output-collection output)]
    values
    (Stdlib.invalid_arg "Expected query-v3 collection output")))

(deftest test-query-v3-collect-to
  (let [left
        (query-v3/array-rel
         ["?x"]
         [(query-int-row [1])
          (query-int-row [2])])
        right
        (query-v3/array-rel
         ["?y"]
         [(query-int-row [10])
          (query-int-row [20])])
        context
        (query-v3/context-v3
         [left right]
         {"?constant" (query-int-result 7)})
        symbols ["?x" "?y" "?constant"]
        expected
        [[1 10 7]
         [1 20 7]
         [2 10 7]
         [2 20 7]]]
    (is (= expected
           (query-int-rows
            (query-v3/collect-to context symbols []))))
    (is (= expected
           (query-int-rows
            (query-v3/collect-to context symbols [] []))))
    (is
     (=
      expected
      (query-int-rows
       (query-v3/collect-to
        context
        symbols
        []
        []
        (to-array [None None None])))))
    (is (= [[99]]
           (query-int-rows
            (query-v3/collect-to
             query-v3/empty-context
             ["?x"]
             [(query-int-row [99])]))))))

(deftest test-query-v3-q-find-shapes
  (let [input (query-v3-int-collection-input [2 1 2])
        relation-output
        (query-v3/q
         (query-v3-collection-input-query
          (parser/relation-find ["?x"])
          "?x")
         input)
        collection-output
        (query-v3/q
         (query-v3-collection-input-query
          (parser/collection-find "?x")
          "?x")
         input)
        scalar-output
        (query-v3/q
         (query-v3-collection-input-query
          (parser/single-find "?x")
          "?x")
         input)
        tuple-query
        (parser/static-query-clauses-with-inputs
         (parser/tuple-find ["?x" "?y"])
         []
         [(parser/make-static-value-input
           (parser/scalar-input "?x"))
          (parser/make-static-value-input
           (parser/scalar-input "?y"))])
        tuple-output
        (query-v3/q
         tuple-query
         (query-types/binding-input
          (query-types/scalar-binding
           (query-int-result 3)))
         (query-types/binding-input
          (query-types/scalar-binding
           (query-int-result 4))))]
    (is (= [[2] [1]]
           (query-int-rows
            (require-query-v3-relation-output
             relation-output))))
    (is (= [2 1]
           (mapv
            query-result-int
            (require-query-v3-collection-output
             collection-output))))
    (is
     (match (query-types/output-scalar scalar-output)
       (Some (Some result)) (= 2 (query-result-int result))
       _ false))
    (is
     (match (query-types/output-tuple tuple-output)
       (Some (Some row))
       (= [3 4]
          (mapv query-result-int (vec row)))
       _ false))))

(deftest test-query-v3-q-empty-find-shapes
  (let [input (query-v3-int-collection-input [])
        relation-output
        (query-v3/q
         (query-v3-collection-input-query
          (parser/relation-find ["?x"])
          "?x")
         input)
        collection-output
        (query-v3/q
         (query-v3-collection-input-query
          (parser/collection-find "?x")
          "?x")
         input)
        scalar-output
        (query-v3/q
         (query-v3-collection-input-query
          (parser/single-find "?x")
          "?x")
         input)
        tuple-output
        (query-v3/q
         (query-v3-collection-input-query
          (parser/tuple-find ["?x"])
          "?x")
         input)]
    (is (= []
           (query-int-rows
            (require-query-v3-relation-output
             relation-output))))
    (is (= []
           (require-query-v3-collection-output
            collection-output)))
    (is (= (Some None)
           (query-types/output-scalar scalar-output)))
    (is (= (Some None)
           (query-types/output-tuple tuple-output)))))

(deftest test-query-v3-q-with-preserves-upstream-multiplicity
  (let [binding
        (parser/collection-input
         (parser/tuple-input
          [(parser/scalar-input "?x")
           (parser/scalar-input "?tag")]))
        base-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find ["?x"])
         []
         [(parser/make-static-value-input binding)])
        with-query (parser/query-with base-query ["?tag"])
        input
        (query-types/binding-input
         (query-types/collection-binding
          [(query-types/collection-binding
            [(query-types/scalar-binding
              (query-int-result 1))
             (query-types/scalar-binding
              (query-int-result 10))])
           (query-types/collection-binding
            [(query-types/scalar-binding
              (query-int-result 1))
             (query-types/scalar-binding
              (query-int-result 20))])
           (query-types/collection-binding
            [(query-types/scalar-binding
              (query-int-result 1))
             (query-types/scalar-binding
              (query-int-result 20))])]))
        without-with
        (require-query-v3-relation-output
         (query-v3/q base-query input))
        with-result
        (require-query-v3-relation-output
         (query-v3/q with-query input))]
    (is (= [[1]]
           (query-int-rows without-with)))
    (is (= [[1] [1]]
           (query-int-rows with-result)))))

(deftest test-query-v3-q-rejects-unbound-find-variable
  (is
   (=
    "Query find variable is not bound: ?missing"
    (try
      (let [_output
            (query-v3/q
             (parser/static-query-clauses-with-inputs
              (parser/relation-find ["?missing"])
              []
              []))]
        "no error")
      (catch (Invalid_argument message)
        (str message))))))

(defn ^datascript.parser/clause query-v3-substitution-clause []
  (parser/static-and-clause
   [(parser/pattern-clause
     [(parser/pattern-variable "?entity")
      (parser/pattern-attribute :score)
      (parser/pattern-variable "?score")
      (parser/pattern-variable "?entity")])
    (parser/static-predicate-clause
     ">"
     [(parser/variable-argument "?score")
      (parser/variable-argument "?limit")])]))

(deftest test-query-v3-clause-symbols
  (let [clause (query-v3-substitution-clause)]
    (is (= #{"?entity" "?score" "?limit"}
           (query-v3/clause-syms clause)))
    (is
     (=
      #{"?entity" "?score" "?limit"}
      (query-v3/clause-syms
       (parser/static-not-clause
        [clause]
        "(not (and ...))"))))))

(deftest test-query-v3-substitute-constants
  (let [clause (query-v3-substitution-clause)
        context
        (query-v3/context-v3
         []
         {"?entity" (query-int-result 1)
          "?limit" (query-int-result 10)})
        substituted
        (query-v3/substitute-constants clause context)
        unchanged
        (query-v3/substitute-constants
         clause
         (query-v3/context-v3 [] {}))]
    (is (= #{"?score"}
           (query-v3/clause-syms substituted)))
    (is (= clause unchanged))
    (is (= #{"?entity" "?score" "?limit"}
           (query-v3/clause-syms clause)))))

(deftest test-query-v3-substitute-constants-in-pattern-resolution
  (let [rows
        [(query-int-row [1 10])
         (query-int-row [2 20])]
        context
        (query-v3/context-v3
         []
         {"?entity" (query-int-result 1)}
         {"$rows" (query-types/relation-source rows)})
        clause
        (parser/explicit-pattern-clause
         "$rows"
         [(parser/pattern-variable "?entity")
          (parser/pattern-variable "?value")])
        resolved (query-v3/resolve-pattern context clause)
        constants (query-v3-context-constants resolved)]
    (is (= 0
           (count (query-v3-context-relations resolved))))
    (is (= 10
           (query-result-int
            (get constants "?value" (query-int-result 0)))))
    (is (= 1
           (query-result-int
            (get constants "?entity" (query-int-result 0)))))))

(defn ^datascript.parser/Query query-v3-function-query
  [^datascript.parser/find-spec find
   ^:vector<datascript.parser/static-query-input> inputs
   ^datascript.parser/clause clause]
  (parser/static-query-clauses-with-inputs
   find [clause] inputs))

(defn ^:vector<vector<string>> query-v3-static-predicate-output-edn
  [^:string function-name
   ^:vector<Datascript_runtime.Data_value.t> values]
  (let [query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find ["?value"])
         [(parser/static-predicate-clause
           function-name
           [(parser/variable-argument "?value")])]
         [(parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?value")))])
        output
        (query-v3/q
         query
         (query-v3-data-collection-input values))]
    (query-output-edn-rows
     (require-query-v3-relation-output output))))

(defn ^:vector<vector<string>> query-v3-static-function-output-edn
  [^:string function-name
   ^:vector<datascript.parser/fn-arg> arguments]
  (let [query
        (query-v3-function-query
         (parser/relation-find ["?result"])
         []
         (parser/static-function-clause
          function-name
          arguments
          (parser/scalar-input "?result")))
        output (query-v3/q query)]
    (query-output-edn-rows
     (require-query-v3-relation-output output))))

(deftest test-query-v3-core-truthiness-predicates
  (let [values
        [(Datascript_runtime.Data_value.Nil)
         (Datascript_runtime.Data_value.Bool false)
         (Datascript_runtime.Data_value.Bool true)
         (Datascript_runtime.Data_value.Int 0)]]
    (is (= [["true"]]
           (query-v3-static-predicate-output-edn
            "true?" values)))
    (is (= [["false"]]
           (query-v3-static-predicate-output-edn
            "false?" values)))
    (is (= [["nil"]]
           (query-v3-static-predicate-output-edn
            "nil?" values)))
    (is (= [["false"] ["true"] ["0"]]
           (query-v3-static-predicate-output-edn
            "some?" values)))
    (is (= [["nil"] ["false"]]
           (query-v3-static-predicate-output-edn
            "not" values)))))

(deftest test-query-v3-core-truthiness-functions
  (is
   (=
    [["true"]]
    (query-v3-static-function-output-edn
     "true?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Bool true))])))
  (is
   (=
    [["false"]]
    (query-v3-static-function-output-edn
     "false?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Bool true))])))
  (is
   (=
    [["true"]]
    (query-v3-static-function-output-edn
     "nil?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Nil))])))
  (is
   (=
    [["false"]]
    (query-v3-static-function-output-edn
     "some?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Nil))])))
  (is
   (=
    [["true"]]
    (query-v3-static-function-output-edn
     "not"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Bool false))]))))

(deftest test-query-v3-and-or-preserve-upstream-values
  (is
   (=
    [["true"]]
    (query-v3-static-function-output-edn "and" [])))
  (is
   (=
    [["\"last\""]]
    (query-v3-static-function-output-edn
     "and"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Bool true))
      (parser/constant-argument
       (Datascript_runtime.Data_value.Int 7))
      (parser/constant-argument
       (Datascript_runtime.Data_value.String "last"))])))
  (is
   (=
    [["false"]]
    (query-v3-static-function-output-edn
     "and"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Int 7))
      (parser/constant-argument
       (Datascript_runtime.Data_value.Bool false))
      (parser/constant-argument
       (Datascript_runtime.Data_value.String "unreached"))])))
  (is
   (=
    []
    (query-v3-static-function-output-edn "or" [])))
  (is
   (=
    [["\"winner\""]]
    (query-v3-static-function-output-edn
     "or"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Nil))
      (parser/constant-argument
       (Datascript_runtime.Data_value.Bool false))
      (parser/constant-argument
       (Datascript_runtime.Data_value.String "winner"))
      (parser/constant-argument
       (Datascript_runtime.Data_value.Int 9))])))
  (is
   (=
    [["false"]]
    (query-v3-static-function-output-edn
     "or"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Nil))
      (parser/constant-argument
       (Datascript_runtime.Data_value.Bool false))]))))

(deftest test-query-v3-core-truthiness-invalid-arity
  (let [zero-argument-query
        (query-v3-function-query
         (parser/relation-find ["?result"])
         []
         (parser/static-function-clause
          "true?"
          []
          (parser/scalar-input "?result")))
        two-argument-query
        (query-v3-function-query
         (parser/relation-find ["?result"])
         []
         (parser/static-function-clause
          "not"
          [(parser/constant-argument
            (Datascript_runtime.Data_value.Bool true))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Bool false))]
          (parser/scalar-input "?result")))]
    (is
     (=
      "Invalid arguments for query function: true?"
      (try
        (let [_output (query-v3/q zero-argument-query)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arguments for query function: not"
      (try
        (let [_output (query-v3/q two-argument-query)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-core-type-predicates
  (let [values
        [(Datascript_runtime.Data_value.Int 1)
         (Datascript_runtime.Data_value.Wide_int
          (Int64.of_int 2))
         (Datascript_runtime.Data_value.Float 3.5)
         (Datascript_runtime.Data_value.Ref 7)
         (Datascript_runtime.Data_value.String "text")
         (Datascript_runtime.Data_value.Bool false)
         (Datascript_runtime.Data_value.Bool true)
         (Datascript_runtime.Data_value.Keyword ":kind")
         (Datascript_runtime.Data_value.Symbol "symbol")
         (Datascript_runtime.Data_value.Nil)]]
    (is
     (=
      [["1"] ["2"] ["3.5"] ["7"]]
      (query-v3-static-predicate-output-edn
       "number?" values)))
    (is
     (=
      [["1"] ["2"] ["7"]]
      (query-v3-static-predicate-output-edn
       "integer?" values)))
    (is
     (=
      [["\"text\""]]
      (query-v3-static-predicate-output-edn
       "string?" values)))
    (is
     (=
      [["false"] ["true"]]
      (query-v3-static-predicate-output-edn
       "boolean?" values)))
    (is
     (=
      [[":kind"]]
      (query-v3-static-predicate-output-edn
       "keyword?" values)))))

(deftest test-query-v3-core-type-functions
  (is
   (=
    [["true"]]
    (query-v3-static-function-output-edn
     "number?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Float 1.5))])))
  (is
   (=
    [["false"]]
    (query-v3-static-function-output-edn
     "integer?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Float 1.5))])))
  (is
   (=
    [["false"]]
    (query-v3-static-function-output-edn
     "string?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Keyword ":kind"))])))
  (is
   (=
    [["true"]]
    (query-v3-static-function-output-edn
     "boolean?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Bool false))])))
  (is
   (=
    [["true"]]
    (query-v3-static-function-output-edn
     "keyword?"
     [(parser/constant-argument
       (Datascript_runtime.Data_value.Keyword ":kind"))]))))

(deftest test-query-v3-core-type-predicate-invalid-arity
  (let [zero-argument-query
        (query-v3-function-query
         (parser/relation-find ["?result"])
         []
         (parser/static-function-clause
          "number?"
          []
          (parser/scalar-input "?result")))
        two-argument-query
        (query-v3-function-query
         (parser/relation-find ["?result"])
         []
         (parser/static-function-clause
          "keyword?"
          [(parser/constant-argument
            (Datascript_runtime.Data_value.Keyword ":kind"))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Keyword ":extra"))]
          (parser/scalar-input "?result")))]
    (is
     (=
      "Invalid arguments for query function: number?"
      (try
        (let [_output (query-v3/q zero-argument-query)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arguments for query function: keyword?"
      (try
        (let [_output (query-v3/q two-argument-query)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-function-clause-built-in
  (let [query
        (query-v3-function-query
         (parser/relation-find ["?x" "?next"])
         [(parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?x")))]
         (parser/static-function-clause
          "inc"
          [(parser/variable-argument "?x")]
          (parser/scalar-input "?next")))
        output
        (query-v3/q
         query
         (query-v3-int-collection-input [1 2]))]
    (is (= [[1 2] [2 3]]
           (query-int-rows
            (require-query-v3-relation-output output)))))
  (let [query
        (query-v3-function-query
         (parser/tuple-find ["?x" "?sum"])
         [(parser/make-static-value-input
           (parser/scalar-input "?x"))]
         (parser/static-function-clause
          "+"
          [(parser/variable-argument "?x")
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 3))]
          (parser/scalar-input "?sum")))
        output
        (query-v3/q
         query
         (query-types/binding-input
          (query-types/scalar-binding
           (query-int-result 4))))]
    (is
     (match (query-types/output-tuple output)
       (Some (Some row))
       (= [4 7] (mapv query-result-int (vec row)))
       _ false))))

(deftest test-query-v3-function-clause-variable-callable
  (let [query
        (query-v3-function-query
         (parser/relation-find ["?x" "?sum"])
         [(parser/make-static-value-input
           (parser/scalar-input "?function"))
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?x")))]
         (parser/variable-function-clause
          "?function"
          [(parser/variable-argument "?x")
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 10))]
          (parser/scalar-input "?sum")))
        output
        (query-v3/q
         query
         (query-types/binding-input
          (query-types/scalar-binding
           (query-types/callable-result
            (query-types/callable
             sum-query-arguments))))
         (query-v3-int-collection-input [1 2]))]
    (is (= [[1 11] [2 12]]
           (query-int-rows
            (require-query-v3-relation-output output))))))

(deftest test-query-v3-function-clause-binding-shapes
  (let [tuple-query
        (query-v3-function-query
         (parser/tuple-find ["?left" "?right"])
         []
         (parser/static-function-clause
          "vector"
          [(parser/constant-argument
            (Datascript_runtime.Data_value.Int 1))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 2))]
          (parser/tuple-input
           [(parser/scalar-input "?left")
            (parser/scalar-input "?right")])))
        tuple-output (query-v3/q tuple-query)
        collection-query
        (query-v3-function-query
         (parser/collection-find "?item")
         []
         (parser/static-function-clause
          "vector"
          [(parser/constant-argument
            (Datascript_runtime.Data_value.Int 3))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 4))]
          (parser/collection-input
           (parser/scalar-input "?item"))))
        collection-output (query-v3/q collection-query)]
    (is
     (match (query-types/output-tuple tuple-output)
       (Some (Some row))
       (= [1 2] (mapv query-result-int (vec row)))
       _ false))
    (is (= [3 4]
           (mapv
            query-result-int
            (require-query-v3-collection-output
             collection-output))))))

(deftest test-query-v3-function-clause-relation-binding
  (let [query
        (query-v3-function-query
         (parser/relation-find ["?x" "?tag"])
         [(parser/make-static-value-input
           (parser/scalar-input "?function"))
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?x")))]
         (parser/variable-function-clause
          "?function"
          [(parser/variable-argument "?x")]
          (parser/collection-input
           (parser/tuple-input
            [(parser/scalar-input "?x")
             (parser/scalar-input "?tag")]))))
        output
        (query-v3/q
         query
         (query-types/binding-input
          (query-types/scalar-binding
           (query-types/callable-result
            (query-types/callable
             relation-query-result))))
         (query-v3-int-collection-input [1 2]))]
    (is (= [[1 10] [1 20] [2 10] [2 20]]
           (query-int-rows
            (require-query-v3-relation-output output))))))

(deftest test-query-v3-function-clause-empty-results
  (let [query
        (fn [^:string function-variable]
          (query-v3-function-query
           (parser/relation-find ["?value"])
           [(parser/make-static-value-input
             (parser/scalar-input function-variable))]
           (parser/variable-function-clause
            function-variable
            []
            (parser/scalar-input "?value"))))
        nil-output
        (query-v3/q
         (query "?nil-function")
         (query-types/binding-input
          (query-types/scalar-binding
           (query-types/callable-result
            (query-types/callable nil-query-result)))))
        absent-output
        (query-v3/q
         (query "?absent-function")
         (query-types/binding-input
          (query-types/scalar-binding
           (query-types/callable-result
            (query-types/callable absent-query-value)))))]
    (is (= []
           (query-int-rows
            (require-query-v3-relation-output
             nil-output))))
    (is (= []
           (query-int-rows
            (require-query-v3-relation-output
             absent-output))))))

(deftest test-query-v3-function-clause-overlapping-binding
  (let [query
        (query-v3-function-query
         (parser/relation-find ["?x"])
         [(parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?x")))]
         (parser/static-function-clause
          "inc"
          [(parser/variable-argument "?x")]
          (parser/scalar-input "?x")))
        output
        (query-v3/q
         query
         (query-v3-int-collection-input [1 2]))]
    (is (= []
           (query-int-rows
            (require-query-v3-relation-output output))))))

(deftest test-query-v3-function-clause-errors
  (let [unknown-query
        (query-v3-function-query
         (parser/relation-find ["?value"])
         []
         (parser/static-function-clause
          "unknown-function"
          []
          (parser/scalar-input "?value")))
        unbound-query
        (query-v3-function-query
         (parser/relation-find ["?value"])
         []
         (parser/static-function-clause
          "inc"
          [(parser/variable-argument "?missing")]
          (parser/scalar-input "?value")))
        invalid-arity-query
        (query-v3-function-query
         (parser/relation-find ["?value"])
         []
         (parser/static-function-clause
          "inc"
          [(parser/constant-argument
            (Datascript_runtime.Data_value.Int 1))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 2))]
          (parser/scalar-input "?value")))]
    (is
     (=
      "Unknown built-in unknown-function"
      (try
        (let [_output (query-v3/q unknown-query)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Insufficient bindings: #{?missing}"
      (try
        (let [_output (query-v3/q unbound-query)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arguments for query function: inc"
      (try
        (let [_output (query-v3/q invalid-arity-query)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-query-v3-database-function-get-else
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :age 42]
           [:db/add 2 :name "Ada"]]))
        query
        (query-v3-function-query
         (parser/relation-find ["?entity" "?age"])
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?entity")))]
         (parser/static-function-clause
          "get-else"
          [(parser/source-argument "$")
           (parser/variable-argument "?entity")
           (parser/constant-argument
            (Datascript_runtime.Data_value.Keyword ":age"))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 99))]
          (parser/scalar-input "?age")))
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         (query-v3-int-collection-input [1 2]))]
    (is (= [[1 42] [2 99]]
           (query-int-rows
            (require-query-v3-relation-output output))))))

(deftest test-query-v3-database-function-get-some
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :name "Ada"]
           [:db/add 2 :age 42]
           [:db/add 3 :other true]]))
        query
        (query-v3-function-query
         (parser/relation-find
          ["?entity" "?attribute" "?value"])
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?entity")))]
         (parser/static-function-clause
          "get-some"
          [(parser/source-argument "$")
           (parser/variable-argument "?entity")
           (parser/constant-argument
            (Datascript_runtime.Data_value.Keyword ":age"))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Keyword ":name"))]
          (parser/tuple-input
           [(parser/scalar-input "?attribute")
            (parser/scalar-input "?value")])))
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         (query-v3-int-collection-input [1 2 3]))]
    (is
     (=
      [["1" ":name" "\"Ada\""]
       ["2" ":age" "42"]]
      (query-output-edn-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-database-function-errors
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :age 42]]))
        nil-default-query
        (query-v3-function-query
         (parser/relation-find ["?value"])
         [(parser/make-static-source-input "$")]
         (parser/static-function-clause
          "get-else"
          [(parser/source-argument "$")
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 1))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Keyword ":missing"))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Nil))]
          (parser/scalar-input "?value")))
        relation-source-query
        (query-v3-function-query
         (parser/relation-find ["?value"])
         [(parser/make-static-source-input "$")]
         (parser/static-function-clause
          "get-else"
          [(parser/source-argument "$")
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 1))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Keyword ":age"))
           (parser/constant-argument
            (Datascript_runtime.Data_value.Int 0))]
          (parser/scalar-input "?value")))]
    (is
     (=
      "get-else: nil default value is not supported"
      (try
        (let [_output
              (query-v3/q
               nil-default-query
               (query-types/source-input
                (query-types/database-source database)))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Predicate source is not a database: $"
      (try
        (let [_output
              (query-v3/q
               relation-source-query
               (query-types/source-input
                (query-types/relation-source
                 [(query-int-row [1])])))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(defn ^datascript.lg.query-types/input
  query-v3-int-tuple-collection-input
  [^:vector<vector<int>> rows]
  (query-types/binding-input
   (query-types/collection-binding
    (mapv
     (fn [^:vector<int> row]
       (query-types/collection-binding
        (mapv
         (fn [^:int value]
           (query-types/scalar-binding
            (query-int-result value)))
         row)))
     rows))))

(deftest test-query-v3-aggregates-grouping-and-all-aggregate
  (let [binding
        (parser/collection-input
         (parser/tuple-input
          [(parser/scalar-input "?group")
           (parser/scalar-input "?x")]))
        input
        (query-v3-int-tuple-collection-input
         [[1 10] [2 7] [1 20]])
        sum-x
        (parser/aggregate-find-element
         "sum"
         [(parser/variable-argument "?x")])
        count-x
        (parser/aggregate-find-element
         "count"
         [(parser/variable-argument "?x")])
        grouped-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [(parser/variable-find-element "?group")
           sum-x
           count-x])
         []
         [(parser/make-static-value-input binding)])
        all-aggregate-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [sum-x count-x])
         []
         [(parser/make-static-value-input binding)])]
    (is
     (=
      [[1 30 2] [2 7 1]]
      (query-int-rows
       (require-query-v3-relation-output
        (query-v3/q grouped-query input)))))
    (is
     (=
      [[37 3]]
      (query-int-rows
       (require-query-v3-relation-output
        (query-v3/q all-aggregate-query input)))))))

(deftest test-query-v3-aggregate-with-preserves-multiplicity
  (let [binding
        (parser/collection-input
         (parser/tuple-input
          [(parser/scalar-input "?x")
           (parser/scalar-input "?tag")]))
        find
        (parser/single-find-element
         (parser/aggregate-find-element
          "count"
          [(parser/variable-argument "?x")]))
        base-query
        (parser/static-query-clauses-with-inputs
         find
         []
         [(parser/make-static-value-input binding)])
        with-query (parser/query-with base-query ["?tag"])
        input
        (query-v3-int-tuple-collection-input
         [[1 10] [1 20] [2 30]])
        base-output (query-v3/q base-query input)
        with-output (query-v3/q with-query input)]
    (is
     (match (query-types/output-scalar base-output)
       (Some (Some result)) (= 2 (query-result-int result))
       _ false))
    (is
     (match (query-types/output-scalar with-output)
       (Some (Some result)) (= 3 (query-result-int result))
       _ false))))

(deftest test-query-v3-parameterized-and-custom-aggregates
  (let [parameterized-max
        (parser/aggregate-find-element
         "max"
         [(parser/variable-argument "?limit")
          (parser/variable-argument "?x")])
        custom-sum
        (parser/custom-aggregate-find-element
         "?aggregate"
         [(parser/variable-argument "?x")])
        parameterized-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements [parameterized-max])
         []
         [(parser/make-static-value-input
           (parser/scalar-input "?limit"))
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?x")))])
        custom-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements [custom-sum])
         []
         [(parser/make-static-value-input
           (parser/scalar-input "?aggregate"))
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?x")))])
        parameterized-output
        (query-v3/q
         parameterized-query
         (query-types/binding-input
          (query-types/scalar-binding
           (query-int-result 2)))
         (query-v3-int-collection-input [1 3 2]))
        custom-output
        (query-v3/q
         custom-query
         (query-types/binding-input
          (query-types/scalar-binding
           (query-types/callable-result
            (query-types/callable sum-query-arguments))))
         (query-v3-int-collection-input [1 3 2]))]
    (is
     (=
      [["[2 3]"]]
      (query-output-edn-rows
       (require-query-v3-relation-output
        parameterized-output))))
    (is
     (=
      [[6]]
      (query-int-rows
       (require-query-v3-relation-output
        custom-output))))))

(deftest test-query-v3-aggregate-errors
  (let [unknown-query
        (query-v3-collection-input-query
         (parser/relation-find-elements
          [(parser/aggregate-find-element
            "unknown-aggregate"
            [(parser/variable-argument "?x")])])
         "?x")
        unbound-parameter-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [(parser/aggregate-find-element
            "max"
            [(parser/variable-argument "?limit")
             (parser/variable-argument "?x")])])
         []
         [(parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?x")))])]
    (is
     (=
      "Unknown aggregate function: unknown-aggregate"
      (try
        (let [_output
              (query-v3/q
               unknown-query
               (query-v3-int-collection-input [1]))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Aggregate parameters must be constants"
      (try
        (let [_output
              (query-v3/q
               unbound-parameter-query
               (query-v3-int-collection-input [1 3]))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(defn ^datascript.lg.query-types/input query-v3-value-input
  [^:Datascript_runtime.Data_value.t value]
  (query-types/binding-input
   (query-types/scalar-binding
    (query-types/value-result value))))

(defn ^:Datascript_runtime.Data_value.t query-v3-name-pattern []
  (query-form-vector
   [(Datascript_runtime.Data_value.Keyword ":name")]))

(deftest test-query-v3-pull-default-source-and-find-shapes
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :name "Ada"]
           [:db/add 1 :age 42]
           [:db/add 2 :name "Bob"]]))
        pull
        (parser/pull-find-element
         "?entity"
         (query-v3-name-pattern))
        descriptors
        [(parser/make-static-source-input "$")
         (parser/make-static-value-input
          (parser/collection-input
           (parser/scalar-input "?entity")))]
        relation-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [(parser/variable-find-element "?entity")
           pull])
         []
         descriptors)
        collection-query
        (parser/static-query-clauses-with-inputs
         (parser/collection-find-element pull)
         []
         descriptors)
        scalar-query
        (parser/static-query-clauses-with-inputs
         (parser/single-find-element pull)
         []
         descriptors)
        database-input
        (query-types/source-input
         (query-types/database-source database))
        entity-input (query-v3-int-collection-input [1 2])]
    (is
     (=
      [["1" "{:name \"Ada\"}"]
       ["2" "{:name \"Bob\"}"]]
      (query-output-edn-rows
       (require-query-v3-relation-output
        (query-v3/q
         relation-query database-input entity-input)))))
    (is
     (=
      ["{:name \"Ada\"}" "{:name \"Bob\"}"]
      (mapv
       (fn [^datascript.lg.query-types/result result]
         (Datascript_runtime.Data_value.to_edn_string
          (query-types/result-pattern-value result)))
       (require-query-v3-collection-output
        (query-v3/q
         collection-query database-input entity-input)))))
    (is
     (match
      (query-types/output-scalar
       (query-v3/q
        scalar-query database-input entity-input))
       (Some (Some result))
       (=
        "{:name \"Ada\"}"
        (Datascript_runtime.Data_value.to_edn_string
         (query-types/result-pattern-value result)))
       _ false))))

(deftest test-query-v3-pull-pattern-variable
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :name "Ada"]]))
        query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [(parser/pull-variable-find-element
            "?entity" "?pattern")])
         []
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/scalar-input "?pattern"))
          (parser/make-static-value-input
           (parser/scalar-input "?entity"))])
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         (query-v3-value-input
          (query-v3-name-pattern))
         (query-v3-value-input
          (Datascript_runtime.Data_value.Int 1)))]
    (is
     (=
      [["{:name \"Ada\"}"]]
      (query-output-edn-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-pull-explicit-source
  (let [first-database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :name "First"]]))
        second-database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :name "Second"]]))
        query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [(parser/pull-source-find-element
            "$second" "?entity"
            (query-v3-name-pattern))])
         []
         [(parser/make-static-source-input "$first")
          (parser/make-static-source-input "$second")
          (parser/make-static-value-input
           (parser/scalar-input "?entity"))])
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source first-database))
         (query-types/source-input
          (query-types/database-source second-database))
         (query-v3-value-input
          (Datascript_runtime.Data_value.Int 1)))]
    (is
     (=
      [["{:name \"Second\"}"]]
      (query-output-edn-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-pull-errors
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :name "Ada"]]))
        default-pull-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [(parser/pull-find-element
            "?entity" (query-v3-name-pattern))])
         []
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/scalar-input "?entity"))])
        variable-pattern-query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find-elements
          [(parser/pull-variable-find-element
            "?entity" "?pattern")])
         []
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/scalar-input "?entity"))])]
    (is
     (=
      "Query source is not a database: $"
      (try
        (let [_output
              (query-v3/q
               default-pull-query
               (query-types/source-input
                (query-types/relation-source
                 [(query-int-row [1])]))
               (query-v3-value-input
                (Datascript_runtime.Data_value.Int 1)))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Pull find pattern variable is not bound"
      (try
        (let [_output
              (query-v3/q
               variable-pattern-query
               (query-types/source-input
                (query-types/database-source database))
               (query-v3-value-input
                (Datascript_runtime.Data_value.Int 1)))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Pull find entity must be an entity reference"
      (try
        (let [_output
              (query-v3/q
               default-pull-query
               (query-types/source-input
                (query-types/database-source database))
               (query-v3-value-input
                (Datascript_runtime.Data_value.String
                 "not-an-entity")))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(defn ^datascript.parser/Query query-v3-rule-query
  [^datascript.parser/find-spec find
   ^:vector<datascript.parser/static-query-input> inputs
   ^datascript.parser/clause clause]
  (parser/static-query-clauses-with-inputs
   find [clause] inputs))

(defn ^:vector<datascript.parser/Rule>
  query-v3-adult-rules
  []
  (let [adult-18
        (parser/pattern-clause
         [(parser/pattern-variable "?entity")
          (parser/pattern-attribute :age)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Int 18))])
        adult-20
        (parser/pattern-clause
         [(parser/pattern-variable "?entity")
          (parser/pattern-attribute :age)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Int 20))])
        ^:vector<datascript.parser/RuleBranch> branches
        [(parser/static-rule-branch
          "adult" ["?entity"] [adult-18])
         (parser/static-rule-branch
          "adult" ["?entity"] [adult-20])
         (parser/static-rule-branch
          "adult" ["?entity"] [adult-18])]]
    (parser/static-rules branches)))

(deftest test-query-v3-rules-input-and-branches
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :age 18]
           [:db/add 2 :age 20]
           [:db/add 3 :age 18]]))
        query
        (query-v3-rule-query
         (parser/relation-find ["?person"])
         [(parser/make-static-source-input "$")
          (parser/make-static-rules-input)]
         (parser/static-rule-clause
          "adult"
          [(parser/pattern-variable "?person")]))
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         (query-types/rules-input
          (query-v3-adult-rules)))]
    (is
     (=
      [[1] [3] [2]]
      (query-int-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-rule-explicit-source
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :age 18]
           [:db/add 2 :age 20]]))
        query
        (query-v3-rule-query
         (parser/relation-find ["?person"])
         [(parser/make-static-source-input "$people")
          (parser/make-static-rules-input)]
         (parser/static-source-rule-clause
          "$people"
          "adult"
          [(parser/pattern-variable "?person")]))
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         (query-types/rules-input
          (query-v3-adult-rules)))]
    (is
     (=
      [[1] [2]]
      (query-int-rows
       (require-query-v3-relation-output output))))))

(defn ^:vector<datascript.parser/Rule>
  query-v3-older-than-rules
  []
  (let [^:vector<datascript.parser/RuleBranch> branches
        [(parser/static-rule-branch-with-vars
          "older-than"
          ["?minimum"]
          ["?entity"]
          [(parser/pattern-clause
            [(parser/pattern-variable "?entity")
             (parser/pattern-attribute :age)
             (parser/pattern-variable "?age")])
           (parser/static-predicate-clause
            ">"
            [(parser/variable-argument "?age")
             (parser/variable-argument "?minimum")])])]]
    (parser/static-rules branches)))

(deftest test-query-v3-rule-required-arguments
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :age 18]
           [:db/add 2 :age 20]]))
        clause
        (parser/static-rule-clause
         "older-than"
         [(parser/pattern-variable "?minimum")
          (parser/pattern-variable "?person")])
        bound-query
        (query-v3-rule-query
         (parser/relation-find ["?person"])
         [(parser/make-static-source-input "$")
          (parser/make-static-rules-input)
          (parser/make-static-value-input
           (parser/scalar-input "?minimum"))]
         clause)
        unbound-query
        (query-v3-rule-query
         (parser/relation-find ["?person"])
         [(parser/make-static-source-input "$")
          (parser/make-static-rules-input)]
         clause)
        database-input
        (query-types/source-input
         (query-types/database-source database))
        rules-input
        (query-types/rules-input
         (query-v3-older-than-rules))]
    (is
     (=
      [[2]]
      (query-int-rows
       (require-query-v3-relation-output
        (query-v3/q
         bound-query
         database-input
         rules-input
         (query-v3-value-input
          (Datascript_runtime.Data_value.Int 18)))))))
    (is
     (=
      "Insufficient bindings for required rule arguments"
      (try
        (let [_output
              (query-v3/q
               unbound-query database-input rules-input)]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(defn ^:vector<datascript.parser/Rule>
  query-v3-ancestor-rules
  []
  (let [direct
        (parser/pattern-clause
         [(parser/pattern-variable "?ancestor")
          (parser/pattern-attribute :parent)
          (parser/pattern-variable "?descendant")])
        step
        (parser/pattern-clause
         [(parser/pattern-variable "?ancestor")
          (parser/pattern-attribute :parent)
          (parser/pattern-variable "?middle")])
        recurse
        (parser/static-rule-clause
         "ancestor"
         [(parser/pattern-variable "?middle")
          (parser/pattern-variable "?descendant")])
        ^:vector<datascript.parser/RuleBranch> branches
        [(parser/static-rule-branch-with-vars
          "ancestor"
          ["?ancestor"]
          ["?descendant"]
          [direct])
         (parser/static-rule-branch-with-vars
          "ancestor"
          ["?ancestor"]
          ["?descendant"]
          [step recurse])]]
    (parser/static-rules branches)))

(deftest test-query-v3-recursive-rules
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db)
          [[:db/add 1 :parent 2]
           [:db/add 2 :parent 3]
           [:db/add 3 :parent 4]]))
        query
        (query-v3-rule-query
         (parser/relation-find ["?descendant"])
         [(parser/make-static-source-input "$")
          (parser/make-static-rules-input)]
         (parser/static-rule-clause
          "ancestor"
          [(parser/pattern-constant
            (Datascript_runtime.Data_value.Int 1))
           (parser/pattern-variable "?descendant")]))
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         (query-types/rules-input
          (query-v3-ancestor-rules)))]
    (is
     (=
      [[2] [3] [4]]
      (query-int-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-repeated-rule-call-terminates
  (let [self-call
        (parser/static-rule-clause
         "loop"
         [(parser/pattern-variable "?value")])
        ^:vector<datascript.parser/RuleBranch> branches
        [(parser/static-rule-branch-with-vars
          "loop"
          ["?value"]
          []
          [self-call])]
        rules
        (parser/static-rules branches)
        query
        (query-v3-rule-query
         (parser/relation-find ["?value"])
         [(parser/make-static-rules-input)
          (parser/make-static-value-input
           (parser/scalar-input "?value"))]
         self-call)
        output
        (query-v3/q
         query
         (query-types/rules-input rules)
         (query-v3-value-input
          (Datascript_runtime.Data_value.Int 1)))]
    (is
     (=
      []
      (require-query-v3-relation-output output)))))

(deftest test-query-v3-rule-errors
  (let [adult-rules (query-v3-adult-rules)
        unknown-query
        (query-v3-rule-query
         (parser/relation-find ["?person"])
         [(parser/make-static-rules-input)]
         (parser/static-rule-clause
          "missing"
          [(parser/pattern-variable "?person")]))
        arity-query
        (query-v3-rule-query
         (parser/relation-find ["?person"])
         [(parser/make-static-rules-input)]
         (parser/static-rule-clause
          "adult"
          [(parser/pattern-variable "?person")
           (parser/pattern-constant
            (Datascript_runtime.Data_value.Int 1))]))
        input-type-query
        (query-v3-rule-query
         (parser/relation-find ["?person"])
         [(parser/make-static-rules-input)]
         (parser/static-rule-clause
          "adult"
          [(parser/pattern-variable "?person")]))]
    (is
     (=
      "Unknown rule 'missing in (missing ?person)"
      (try
        (let [_output
              (query-v3/q
               unknown-query
               (query-types/rules-input []))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Rule arity mismatch"
      (try
        (let [_output
              (query-v3/q
               arity-query
               (query-types/rules-input adult-rules))]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Rules query input requires a Rules_input"
      (try
        (let [_output
              (query-v3/q
               input-type-query
               (query-v3-value-input
                (Datascript_runtime.Data_value.Int 1)))]
          "no error")
      (catch (Invalid_argument message)
        (str message)))))))

(deftest test-query-v3-lookup-ref-input-joins-entity-position
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db
           {:email {:db/unique :db.unique/identity}})
          [[:db/add 1 :email "ada@example.com"]
           [:db/add 1 :age 30]
           [:db/add 2 :email "bob@example.com"]
           [:db/add 2 :age 40]]))
        query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find ["?age"])
         [(parser/pattern-clause
           [(parser/pattern-variable "?entity")
            (parser/pattern-attribute :age)
            (parser/pattern-variable "?age")])]
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?entity")))])
        input
        (query-v3-data-collection-input
         [(query-v3-lookup-ref :email "ada@example.com")
          (query-v3-lookup-ref :email "missing@example.com")
          (query-v3-lookup-ref :email "bob@example.com")])
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         input)]
    (is
     (=
      [[30] [40]]
      (query-int-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-lookup-ref-input-joins-ref-value
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db
           {:email {:db/unique :db.unique/identity}
            :friend {:db/valueType :db.type/ref}})
          [[:db/add 1 :email "ada@example.com"]
           [:db/add 2 :email "bob@example.com"]
           [:db/add 10 :friend 1]
           [:db/add 20 :friend 2]]))
        query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find ["?person"])
         [(parser/pattern-clause
           [(parser/pattern-variable "?person")
            (parser/pattern-attribute :friend)
            (parser/pattern-variable "?friend")])]
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?friend")))])
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         (query-v3-data-collection-input
          [(query-v3-lookup-ref :email "ada@example.com")
           (query-v3-lookup-ref :email "bob@example.com")]))]
    (is
     (=
      [[10] [20]]
      (query-int-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-lookup-ref-uses-explicit-source
  (let [default-database
        (db/database-view
         (d/db-with
          (d/empty-db
           {:email {:db/unique :db.unique/identity}})
          [[:db/add 1 :email "same@example.com"]
           [:db/add 1 :age 10]]))
        other-database
        (db/database-view
         (d/db-with
          (d/empty-db
           {:email {:db/unique :db.unique/identity}})
          [[:db/add 2 :email "same@example.com"]
           [:db/add 2 :age 20]]))
        query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find ["?age"])
         [(parser/explicit-pattern-clause
           "$other"
           [(parser/pattern-variable "?entity")
            (parser/pattern-attribute :age)
            (parser/pattern-variable "?age")])]
         [(parser/make-static-source-input "$")
          (parser/make-static-source-input "$other")
          (parser/make-static-value-input
           (parser/collection-input
            (parser/scalar-input "?entity")))])
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source default-database))
         (query-types/source-input
          (query-types/database-source other-database))
         (query-v3-data-collection-input
          [(query-v3-lookup-ref :email "same@example.com")
           (query-v3-lookup-ref :email "missing@example.com")]))]
    (is
     (=
      [[20]]
      (query-int-rows
       (require-query-v3-relation-output output))))))

(deftest test-query-v3-lookup-ref-composite-join-key
  (let [database
        (db/database-view
         (d/db-with
          (d/empty-db
           {:email {:db/unique :db.unique/identity}})
          [[:db/add 1 :email "ada@example.com"]
           [:db/add 1 :tag "x"]
           [:db/add 2 :email "bob@example.com"]
           [:db/add 2 :tag "y"]]))
        query
        (parser/static-query-clauses-with-inputs
         (parser/relation-find ["?tag"])
         [(parser/pattern-clause
           [(parser/pattern-variable "?entity")
            (parser/pattern-attribute :tag)
            (parser/pattern-variable "?tag")])]
         [(parser/make-static-source-input "$")
          (parser/make-static-value-input
           (parser/collection-input
            (parser/tuple-input
             [(parser/scalar-input "?entity")
              (parser/scalar-input "?tag")])))] )
        input
        (query-v3-data-tuple-collection-input
         [[(query-v3-lookup-ref :email "ada@example.com")
           (Datascript_runtime.Data_value.String "x")]
          [(query-v3-lookup-ref :email "ada@example.com")
           (Datascript_runtime.Data_value.String "wrong")]
          [(query-v3-lookup-ref :email "bob@example.com")
           (Datascript_runtime.Data_value.String "y")]])
        output
        (query-v3/q
         query
         (query-types/source-input
          (query-types/database-source database))
         input)]
    (is
     (=
      [["\"x\""] ["\"y\""]]
      (query-output-edn-rows
       (require-query-v3-relation-output output))))))

(deftest test-public-tuple-key-helpers
  (let [attrs (query-types/index-attrs ["?x" "?y"])
        row (query-int-row [10 20])
        getter (query/getter-fn attrs "?y")]
    (is (= 20 (query-result-int (getter row))))
    (is
     (match ((query/tuple-key-fn attrs ["?x"]) row)
       (query/SingleTupleKey value)
       (= 10 (query-result-int value))
       _ false))
    (is
     (match ((query/tuple-key-fn attrs ["?x" "?y"]) row)
       (query/CompositeTupleKey values)
       (= [10 20] (mapv query-result-int values))
       _ false)))

  (let [database
        (->
         (d/empty-db {:name {:db/unique :db.unique/identity}})
         (d/db-with [[:db/add 1 :name "Ada"]]))
        attrs (query-types/index-attrs ["?e"])
        lookup-ref
        (query-form-vector
         [(Datascript_runtime.Data_value.Keyword ":name")
          (Datascript_runtime.Data_value.String "Ada")])
        row
        (to-array [(query-types/value-result lookup-ref)])
        resolved
        (binding
         [query/*lookup-attrs* (conj (set-of :string) "?e")
          query/*implicit-source*
          (Some (db/database-view database))]
          ((query/getter-fn attrs "?e") row))]
    (is (= 1 (query-result-int resolved)))))

(defn ^datascript.lg.query-types/context predicate-test-context
  [^:vector<int> values]
  (let [database (db/database-view (d/empty-db))
        relation
        (query-types/relation
         (query-types/index-attrs ["?x"])
         (mapv
          (fn [^:int value]
            (query-int-row [value]))
          values)
         {})]
    (query-types/context
     [relation]
     {"$" (query-types/database-source database)}
     [])))

(defn ^datascript.parser/clause greater-than-clause
  [^:int value]
  (parser/static-predicate-clause
   ">"
   [(parser/variable-argument "?x")
    (parser/constant-argument
     (Datascript_runtime.Data_value.Int value))]))

(defn ^:vector<int> context-x-values
  [^datascript.lg.query-types/context context]
  (let [relations (query-types/context-relations context)]
    (if-some [relation (first relations)]
      (let [^:vector<string> variables ["?x"]
            ^:vector<vector<int>> rows
            (relation-int-rows relation variables)]
        (mapv
         (fn [^:vector<int> row]
           (nth row 0))
         rows))
      [])))

(deftest test-public-resolve-clause-helpers
  (let [context (predicate-test-context [1 2 3])
        clause (greater-than-clause 1)]
    (is (= [2 3] (context-x-values
                  (query/-resolve-clause context clause))))
    (is (= [2 3] (context-x-values
                  (query/-resolve-clause context clause clause))))
    (is (= [2 3] (context-x-values
                  (query/resolve-clause context clause))))
    (is (= [3] (context-x-values
                (query/-q
                 context
                 [clause (greater-than-clause 2)])))))

  (let [empty-context (predicate-test-context [])]
    (is (= []
           (context-x-values
            (query/resolve-clause
             empty-context
             (greater-than-clause 1)))))))

(deftest test-public-predicate-and-function-helpers
  (let [context (predicate-test-context [1 2 3])
        filtered
        (query/filter-by-pred context (greater-than-clause 1))]
    (is (= [2 3] (context-x-values filtered))))

  (let [context (predicate-test-context [1 2])
        clause
        (parser/static-function-clause
         "+"
         [(parser/variable-argument "?x")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 10))]
         (parser/scalar-input "?y"))
        bound (query/bind-by-fn context clause)
        relations (query-types/context-relations bound)]
    (if-some [relation (first relations)]
      (is (= [[1 11] [2 12]]
             (relation-int-rows relation ["?x" "?y"])))
      (is false))))

(deftest test-public-call-fn
  (let [context (predicate-test-context [1 2])
        relations (query-types/context-relations context)]
    (if-some [relation (first relations)]
      (let [rows (query-types/relation-rows relation)
            call
            (query/-call-fn
             context
             relation
             (query-types/callable sum-query-arguments)
             [(parser/variable-argument "?x")
              (parser/constant-argument
               (Datascript_runtime.Data_value.Int 10))])
            source-call
            (query/-call-fn
             context
             relation
             (query-types/callable database-marker)
             [(parser/source-argument "$")])]
        (is (= 11 (query-call-int (call (nth rows 0)))))
        (is (= 12 (query-call-int (call (nth rows 1)))))
        (is (= 1 (query-call-int (source-call (nth rows 0)))))
        (let [missing-call
              (query/-call-fn
               context
               relation
               (query-types/callable sum-query-arguments)
               [(parser/variable-argument "?missing")])]
          (is
           (thrown-msg?
            "Callable variable is not bound: ?missing"
            (missing-call (nth rows 0))))))
      (is false))))

(deftest test-public-form-predicates
  (let [source (Datascript_runtime.Data_value.Symbol "$")
        named-source (Datascript_runtime.Data_value.Symbol "$users")
        variable (Datascript_runtime.Data_value.Symbol "?user")
        empty-symbol (Datascript_runtime.Data_value.Symbol "")
        attr (Datascript_runtime.Data_value.Keyword ":user/name")
        string-attr (Datascript_runtime.Data_value.String "user/name")
        value (Datascript_runtime.Data_value.Int 42)]
    (is (query/source? source))
    (is (query/source? named-source))
    (is (not (query/source? variable)))
    (is (not (query/source? empty-symbol)))
    (is (not (query/source? attr)))
    (is (query/free-var? variable))
    (is (not (query/free-var? source)))
    (is (not (query/free-var? empty-symbol)))
    (is (not (query/free-var? attr)))
    (is (query/attr? attr))
    (is (query/attr? string-attr))
    (is (not (query/attr? variable)))
    (is (not (query/attr? (Datascript_runtime.Data_value.Nil))))
    (is (query/lookup-ref? (query-form-vector [attr value])))
    (is (query/lookup-ref? (query-form-list [string-attr value])))
    (is (not (query/lookup-ref? (query-form-vector [attr]))))
    (is (not (query/lookup-ref?
              (query-form-vector [attr value value]))))
    (is (not (query/lookup-ref?
              (query-form-vector [variable value]))))
    (is (not (query/lookup-ref?
              (Datascript_runtime.Data_value.Nil))))))

(deftest test-public-relation-utilities
  (let [left {"?x" 0 "?shared" 1}
        right {"?shared" 0 "?y" 1}
        same-keys-different-indexes {"?shared" 8 "?x" 9}
        left-relation
        (query-types/relation left [] {})
        right-relation
        (query-types/relation right [] {})
        context
        (query-types/context
         [left-relation right-relation]
         {}
         [])]
    (is (= #{"?shared"} (query/intersect-keys left right)))
    (is (= #{} (query/intersect-keys left {})))
    (is (query/same-keys? left same-keys-different-indexes))
    (is (not (query/same-keys? left right)))
    (is (= #{"?x" "?shared" "?y"} (query/bound-vars context)))
    (is (= #{}
           (query/bound-vars
           (query-types/context [] {} []))))))

(deftest test-public-relation-algebra
  (let [left
        (query-types/relation
         {"?x" 0}
         [(array (query-int-result 1))
          (array (query-int-result 2))]
         {})
        right
        (query-types/relation
         {"?y" 0}
         [(array (query-int-result 10))
          (array (query-int-result 20))]
         {})
        product (query/prod-rel left right)]
    (is (= {} (query-types/relation-attrs (query/prod-rel))))
    (is (= [[]]
           (relation-int-rows (query/prod-rel) [])))
    (is (= {"?x" 0 "?y" 1}
           (query-types/relation-attrs product)))
    (is (= [[1 10] [1 20] [2 10] [2 20]]
           (relation-int-rows product ["?x" "?y"]))))
  (let [left
        (query-types/relation
         {"?x" 0 "?shared" 1}
         [(array (query-int-result 1) (query-int-result 7))
          (array (query-int-result 2) (query-int-result 8))]
         {})
        right
        (query-types/relation
         {"?shared" 0 "?y" 1}
         [(array (query-int-result 7) (query-int-result 70))
          (array (query-int-result 9) (query-int-result 90))]
         {})
        joined (query/hash-join left right)
        unrelated
        (query-types/relation
         {"?z" 0}
         [(array (query-int-result 5))]
         {})
        collapsed (query/collapse-rels [unrelated left] right)]
    (is (= {"?x" 0 "?shared" 1 "?y" 2}
           (query-types/relation-attrs joined)))
    (is (= [[1 7 70]]
           (relation-int-rows joined ["?x" "?shared" "?y"])))
    (is (= 2 (count collapsed)))
    (is (= [[5]]
           (relation-int-rows (nth collapsed 0) ["?z"])))
    (is (= [[1 7 70]]
           (relation-int-rows
            (nth collapsed 1)
            ["?x" "?shared" "?y"])))))

(deftest test-public-relation-subtraction
  (let [database
        (datascript.db/database-view (d/empty-db))
        left
        (query-types/relation
         {"?x" 0 "?y" 1}
         [(array (query-int-result 1) (query-int-result 10))
          (array (query-int-result 2) (query-int-result 20))
          (array (query-int-result 3) (query-int-result 30))]
         {"?x" database})
        right
        (query-types/relation
         {"?y" 0 "?z" 1}
         [(array (query-int-result 20) (query-int-result 200))
          (array (query-int-result 99) (query-int-result 999))]
         {})
        subtracted (query/subtract-rel left right)
        empty-right (query-types/relation {"?y" 0} [] {})
        unrelated
        (query-types/relation
         {"?other" 0}
         [(array (query-int-result 1))]
         {})]
    (is (= {"?x" 0 "?y" 1}
           (query-types/relation-attrs subtracted)))
    (is (= [[1 10] [3 30]]
           (relation-int-rows subtracted ["?x" "?y"])))
    (is
     (some?
      (query-types/relation-lookup-database
       subtracted "?x")))
    (is (= [[1 10] [2 20] [3 30]]
           (relation-int-rows
            (query/subtract-rel left empty-right)
            ["?x" "?y"])))
    (is (= []
           (relation-int-rows
            (query/subtract-rel left unrelated)
            ["?x" "?y"])))
    (is (= [[1 10] [2 20] [3 30]]
           (relation-int-rows
            (query/subtract-rel
             left
             (query-types/relation {"?other" 0} [] {}))
            ["?x" "?y"])))))

(deftest test-public-sum-rel
  (let [left
        (query-types/relation
         {"?x" 0 "?y" 1}
         [(array (query-int-result 1) (query-int-result 10))]
         {})
        exact-right
        (query-types/relation
         {"?x" 0 "?y" 1}
         [(array (query-int-result 2) (query-int-result 20))]
         {})
        reordered-right
        (query-types/relation
         {"?y" 0 "?x" 1}
         [(array (query-int-result 30) (query-int-result 3))]
         {})
        empty-other
        (query-types/relation {"?other" 0} [] {})
        different
        (query-types/relation
         {"?z" 0}
         [(array (query-int-result 9))]
         {})]
    (is (= [[1 10] [2 20]]
           (relation-int-rows
            (query/sum-rel left exact-right)
            ["?x" "?y"])))
    (is (= [[1 10] [3 30]]
           (relation-int-rows
            (query/sum-rel left reordered-right)
            ["?x" "?y"])))
    (is (= [[1 10]]
           (relation-int-rows
            (query/sum-rel empty-other left)
            ["?x" "?y"])))
    (is (= [[1 10]]
           (relation-int-rows
            (query/sum-rel left empty-other)
            ["?x" "?y"])))
    (is
     (thrown-msg?
      "Can’t sum relations with different attrs: {\"?x\" 0, \"?y\" 1} and {\"?z\" 0}"
      (query/sum-rel left different)))))

(deftest test-public-empty-and-limited-relations
  (let [binding
        (parser/tuple-input
         [(parser/scalar-input "?x")
          (parser/tuple-input
           [(parser/scalar-input "?y")
            (parser/ignore-input)])])
        empty-relation (query/empty-rel binding)
        relation
        (query-types/relation
         {"?x" 0 "?y" 1 "?z" 2}
         [(array
           (query-int-result 1)
           (query-int-result 2)
           (query-int-result 3))]
         {})]
    (is (= {"?x" 0 "?y" 1}
           (query-types/relation-attrs empty-relation)))
    (is (= [] (query-types/relation-rows empty-relation)))
    (if-some [limited (query/limit-rel relation #{"?x" "?z"})]
      (do
        (is (= {"?x" 0 "?z" 2}
               (query-types/relation-attrs limited)))
        (is (= [[1 3]]
               (relation-int-rows limited ["?x" "?z"]))))
      (is false))
    (is (= None (query/limit-rel relation #{"?missing"})))))

(deftest test-public-limit-context
  (let [xy
        (query-types/relation
         {"?x" 0 "?y" 1}
         [(array (query-int-result 1) (query-int-result 2))]
         {})
        z
        (query-types/relation
         {"?z" 0}
         [(array (query-int-result 3))]
         {})
        sources {"$input" (query-types/relation-source [])}
        context (query-types/context [xy z] sources [])
        limited (query/limit-context context #{"?x"})
        limited-relations (query-types/context-relations limited)]
    (is (= 1 (count limited-relations)))
    (is (= {"?x" 0}
           (query-types/relation-attrs
            (nth limited-relations 0))))
    (is (= [[1]]
           (relation-int-rows
            (nth limited-relations 0)
            ["?x"])))
    (is (= sources (query-types/context-sources limited)))
    (is (= [] (query-types/context-rules limited)))
    (is (= []
           (query-types/context-relations
            (query/limit-context context (set-of :string)))))))

(deftest test-public-variable-collection-and-binding-checks
  (let [x (Datascript_runtime.Data_value.Symbol "?x")
        y (Datascript_runtime.Data_value.Symbol "?y")
        map-key (Datascript_runtime.Data_value.Symbol "?map-key")
        set-value (Datascript_runtime.Data_value.Symbol "?set-value")
        nested
        (query-form-vector
         [x
          (query-form-list [y x])
          (Datascript_runtime.Data_value.Map
           (list
            (tuple
             map-key
             (Datascript_runtime.Data_value.Set
              (list set-value set-value)))))])
        collected
        (query/walk-collect
         nested
         (fn [value] (query/free-var? value)))]
    (is (= ["?x" "?y" "?x" "?map-key" "?set-value" "?set-value"]
           (mapv
            (fn [value]
              (Datascript_runtime.Data_value.to_edn_string value))
            collected)))
    (is (= #{"?x" "?y" "?map-key" "?set-value"}
           (query/collect-vars nested)))
    (is (= #{}
           (query/collect-vars
            (query-form-vector
             [(Datascript_runtime.Data_value.Symbol "")
              (Datascript_runtime.Data_value.Keyword ":name")
              (Datascript_runtime.Data_value.Int 1)])))))
  (let [form
        (query-form-vector
         [(Datascript_runtime.Data_value.Symbol "?x")
          (Datascript_runtime.Data_value.Symbol "?y")])]
    (query/check-bound #{"?x" "?y"} ["?y" "?x"] form)
    (is
     (thrown-msg?
      "Insufficient bindings: #{?y} not bound in [?x ?y]"
      (query/check-bound #{"?x"} ["?x" "?y"] form)))))

(deftest test-public-free-variable-checks
  (let [e (Datascript_runtime.Data_value.Symbol "?e")
        x (Datascript_runtime.Data_value.Symbol "?x")
        y (Datascript_runtime.Data_value.Symbol "?y")
        branch-x
        (query-form-vector
         [e (Datascript_runtime.Data_value.Keyword ":a") x])
        branch-x-reordered
        (query-form-vector
         [x (Datascript_runtime.Data_value.Keyword ":b") e])
        branch-y
        (query-form-vector
         [e (Datascript_runtime.Data_value.Keyword ":b") y])
        or-form
        (query-form-list
         [(Datascript_runtime.Data_value.Symbol "or")
          branch-x
          branch-y])]
    (query/check-free-same
     #{"?e"} [branch-x branch-x-reordered] or-form)
    (is
     (thrown-msg?
      "All clauses in 'or' must use same set of free vars, had [#{?x} #{?y}] in (or [?e :a ?x] [?e :b ?y])"
      (query/check-free-same
       #{"?e"} [branch-x branch-y] or-form)))
    (query/check-free-subset
     #{"?e"} ["?e" "?x"] [branch-x branch-x-reordered])
    (is
     (thrown-msg?
      "All clauses in 'or' must use same set of free vars, had #{?x} not bound in [?e :b ?y]"
      (query/check-free-subset
       #{"?e"} ["?e" "?x"] [branch-x branch-y])))))

(deftest test-public-tuple-and-pattern-utilities
  (let [left
        (array
         (query-int-result 1)
         (query-int-result 2)
         (query-int-result 3))
        right
        (array
         (query-int-result 10)
         (query-int-result 20))
        joined
        (query/join-tuples
         left (array 2 0)
         right (array 1))]
    (is (= [3 1 20]
           (mapv
            (fn [^:int index]
              (query-result-int (aget joined index)))
            (range (Array.length joined))))))
  (let [variable (Datascript_runtime.Data_value.Symbol "?x")
        placeholder (Datascript_runtime.Data_value.Symbol "_")
        one (Datascript_runtime.Data_value.Int 1)
        two (Datascript_runtime.Data_value.Int 2)
        three (Datascript_runtime.Data_value.Int 3)]
    (is (query/matches-pattern?
         [variable two placeholder]
         [one two three]))
    (is (not
         (query/matches-pattern?
          [variable three]
          [one two])))
    (is (query/matches-pattern? [one] [one two]))
    (is (query/matches-pattern? [one two] [one]))
    (is (query/matches-pattern? [] [one]))
    (is (query/matches-pattern? [one] []))))

(deftest test-public-pattern-normalization-and-pair-removal
  (let [source (Datascript_runtime.Data_value.Symbol "$users")
        entity (Datascript_runtime.Data_value.Symbol "?e")
        attr (Datascript_runtime.Data_value.Keyword ":name")
        ^:vector<Datascript_runtime.Data_value.t>
        explicit [source entity attr]
        ^:vector<Datascript_runtime.Data_value.t>
        implicit [entity attr]]
    (is (= ["$users" "?e" ":name"]
           (query-form-strings
            (query/normalize-pattern-clause explicit))))
    (is (= ["$" "?e" ":name"]
           (query-form-strings
            (query/normalize-pattern-clause implicit))))
    (is (= ["$"]
           (query-form-strings
            (query/normalize-pattern-clause [])))))
  (let [same-symbol (Datascript_runtime.Data_value.Symbol "?x")
        left-one (Datascript_runtime.Data_value.Int 1)
        right-two (Datascript_runtime.Data_value.Int 2)
        left-attr (Datascript_runtime.Data_value.Keyword ":left")
        right-attr (Datascript_runtime.Data_value.Keyword ":right")
        same-string (Datascript_runtime.Data_value.String "same")
        ^:vector<Datascript_runtime.Data_value.t>
        left
        [same-symbol left-one left-attr same-string
         (Datascript_runtime.Data_value.Int 99)]
        ^:vector<Datascript_runtime.Data_value.t>
        right
        [same-symbol right-two right-attr same-string]
        ^:tuple<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>>
        remaining
        (query/remove-pairs left right)]
    (is (= ["1" ":left"]
           (query-form-strings (tuple-get remaining 0))))
    (is (= ["2" ":right"]
           (query-form-strings (tuple-get remaining 1))))))

(deftest test-public-constant-substitution
  (let [x (Datascript_runtime.Data_value.Symbol "?x")
        same (Datascript_runtime.Data_value.Symbol "?same")
        empty-variable
        (Datascript_runtime.Data_value.Symbol "?empty")
        shadowed
        (Datascript_runtime.Data_value.Symbol "?shadowed")
        nil-variable
        (Datascript_runtime.Data_value.Symbol "?nil")
        missing
        (Datascript_runtime.Data_value.Symbol "?missing")
        literal (Datascript_runtime.Data_value.Keyword ":name")
        one-row
        (query-types/relation
         {"?x" 0 "?nil" 1}
         [(array
           (query-int-result 7)
           (query-types/value-result
            (Datascript_runtime.Data_value.Nil)))]
         {})
        repeated-value
        (query-types/relation
         {"?same" 0}
         [(array (query-int-result 5))
          (array (query-int-result 5))]
         {})
        empty-relation
        (query-types/relation {"?empty" 0} [] {})
        shadowing-empty
        (query-types/relation {"?shadowed" 0} [] {})
        shadowed-value
        (query-types/relation
         {"?shadowed" 0}
         [(array (query-int-result 9))]
         {})
        context
        (query-types/context
         [one-row
          repeated-value
          empty-relation
          shadowing-empty
          shadowed-value]
         {}
         [])]
    (if-some [value (query/substitute-constant context x)]
      (is (= "7"
             (Datascript_runtime.Data_value.to_edn_string value)))
      (is false))
    (if-some [value
              (query/substitute-constant context nil-variable)]
      (is (= "nil"
             (Datascript_runtime.Data_value.to_edn_string value)))
      (is false))
    (is (= None (query/substitute-constant context literal)))
    (is (= None (query/substitute-constant context missing)))
    (is (= None (query/substitute-constant context same)))
    (is (= None
           (query/substitute-constant context empty-variable)))
    (is (= None (query/substitute-constant context shadowed)))
    (is (= ["7" "?same" "nil" ":name" "?missing"]
           (query-form-strings
            (query/substitute-constants
             context
             [x same nil-variable literal missing]))))))

(deftest test-public-dynamic-lookup-attrs
  (let [database
        (datascript.db/database-view
         (d/empty-db
          {:friend {:db/valueType :db.type/ref}}))
        entity (Datascript_runtime.Data_value.Symbol "?e")
        attr-variable
        (Datascript_runtime.Data_value.Symbol "?a")
        value (Datascript_runtime.Data_value.Symbol "?v")
        tx (Datascript_runtime.Data_value.Symbol "?tx")
        ref-attr
        (Datascript_runtime.Data_value.Keyword ":friend")
        scalar-attr
        (Datascript_runtime.Data_value.Keyword ":name")
        ^:vector<Datascript_runtime.Data_value.t>
        ref-pattern [entity ref-attr value tx]
        ^:vector<Datascript_runtime.Data_value.t>
        scalar-pattern [entity scalar-attr value tx]
        ^:vector<Datascript_runtime.Data_value.t>
        free-attr-pattern [entity attr-variable value tx]
        ^:vector<Datascript_runtime.Data_value.t>
        short-pattern [entity]]
    (is (= #{"?e" "?v" "?tx"}
           (query/dynamic-lookup-attrs database ref-pattern)))
    (is (= #{"?e" "?tx"}
           (query/dynamic-lookup-attrs database scalar-pattern)))
    (is (= #{"?e" "?tx"}
           (query/dynamic-lookup-attrs
            database free-attr-pattern)))
    (is (= #{"?e"}
           (query/dynamic-lookup-attrs database short-pattern)))
    (is (= #{}
           (query/dynamic-lookup-attrs database [])))))

(deftest test-public-pattern-lookup-ref-resolution
  (let [database
        (-> (d/empty-db
             {:friend
              {:db/valueType :db.type/ref
               :db/unique :db.unique/identity}})
            (d/db-with
             [[:db/add 1 :db/ident :person/one]
              [:db/add 1 :friend 2]
              [:db/add 2 :friend 1]]))
        database-source
        (query-types/database-source
         (datascript.db/database-view database))
        relation-source (query-types/relation-source [])
        friend (Datascript_runtime.Data_value.Keyword ":friend")
        lookup-one
        (query-form-vector
         [friend (Datascript_runtime.Data_value.Int 1)])
        lookup-two
        (query-form-vector
         [friend (Datascript_runtime.Data_value.Int 2)])
        missing
        (query-form-vector
         [friend (Datascript_runtime.Data_value.Int 999)])
        ^:vector<Datascript_runtime.Data_value.t>
        pattern [lookup-two friend lookup-one lookup-two]
        ^:vector<Datascript_runtime.Data_value.t>
        ident-pattern
        [(Datascript_runtime.Data_value.Keyword ":person/one")]]
    (is (= ["1" ":friend" "2" "1"]
           (query-form-strings
            (query/resolve-pattern-lookup-refs
             database-source pattern))))
    (is (= ["1"]
           (query-form-strings
            (query/resolve-pattern-lookup-refs
             database-source ident-pattern))))
    (is (= ["[:friend 2]" ":friend" "[:friend 1]" "[:friend 2]"]
           (query-form-strings
            (query/resolve-pattern-lookup-refs
             relation-source pattern))))
    (is (= []
           (query/resolve-pattern-lookup-refs
            database-source [])))
    (is
     (thrown-msg?
      "Nothing found for entity id [:friend 999]"
      (query/resolve-pattern-lookup-refs
       database-source
       [missing])))))

(deftest test-public-rule-parsing
  (let [entity (Datascript_runtime.Data_value.Symbol "?e")
        age (Datascript_runtime.Data_value.Symbol "?age")
        age-clause
        (query-form-vector
         [entity
          (Datascript_runtime.Data_value.Keyword ":age")
          age])
        name-clause
        (query-form-vector
         [entity
          (Datascript_runtime.Data_value.Keyword ":name")
          (Datascript_runtime.Data_value.Symbol "_")])
        first-branch
        (query-rule-branch "adult" [entity] [age-clause])
        second-branch
        (query-rule-branch "adult" [entity] [name-clause])
        form-rules
        (query/parse-rules
         (query-form-vector [first-branch second-branch]))
        string-rules
        (query/parse-rules
         (Datascript_runtime.Data_value.String
          "[[[adult ?e] [?e :age 18]]]"))
        empty-rules
        (query/parse-rules (query-form-vector []))]
    (is (= 1 (count form-rules)))
    (if-some [branches (parser/rule-branches form-rules "adult")]
      (is (= 2 (count branches)))
      (is false))
    (if-some [branches
              (parser/rule-branches string-rules "adult")]
      (is (= 1 (count branches)))
      (is false))
    (is (= 0 (count empty-rules))))
  (let [x (Datascript_runtime.Data_value.Symbol "?x")
        y (Datascript_runtime.Data_value.Symbol "?y")
        placeholder-clause
        (query-form-vector
         [(Datascript_runtime.Data_value.Symbol "_")])
        one-arg
        (query-rule-branch
         "rule" [x] [placeholder-clause])
        two-args
        (query-rule-branch
         "rule" [x y] [placeholder-clause])]
    (is
     (thrown-msg?
      "Arity mismatch for rule 'rule': [?x] vs. [?x ?y]"
      (query/parse-rules
       (query-form-vector [one-arg two-args]))))))

(deftest test-public-rule-predicate
  (let [entity (Datascript_runtime.Data_value.Symbol "?e")
        placeholder-clause
        (query-form-vector
         [(Datascript_runtime.Data_value.Symbol "_")])
        rules
        (query/parse-rules
         (query-form-vector
          [(query-rule-branch
            "adult" [entity] [placeholder-clause])]))
        context (query-types/context [] {} rules)
        known
        (query-form-list
         [(Datascript_runtime.Data_value.Symbol "adult")
          entity])
        sourced-known
        (query-form-list
         [(Datascript_runtime.Data_value.Symbol "$people")
          (Datascript_runtime.Data_value.Symbol "adult")
          entity])
        variable-head
        (query-form-list
         [entity
          (Datascript_runtime.Data_value.Keyword ":age")])
        disjunction
        (query-form-list
         [(Datascript_runtime.Data_value.Symbol "or")
          known])
        unknown
        (query-form-list
         [(Datascript_runtime.Data_value.Symbol "missing")
          entity])]
    (is (not
         (query/rule?
          context
          (Datascript_runtime.Data_value.Keyword ":age"))))
    (is (not (query/rule? context variable-head)))
    (is (not (query/rule? context disjunction)))
    (is (query/rule? context known))
    (is (query/rule? context sourced-known))
    (is
     (thrown-msg?
      "Unknown rule 'missing in (missing ?e)"
      (query/rule? context unknown)))))

(deftest test-public-rule-guards
  (let [a (Datascript_runtime.Data_value.Symbol "?a")
        b (Datascript_runtime.Data_value.Symbol "?b")
        old (Datascript_runtime.Data_value.Symbol "?old")
        missing
        (Datascript_runtime.Data_value.Symbol "?missing")
        rule-clause
        (query-form-list
         [(Datascript_runtime.Data_value.Symbol "ancestor")
          a b])
        ^:map<string;vector<vector<Datascript_runtime.Data_value.t>>>
        used-args
        {"ancestor" [[a old] [old b]]}
        ^:map<string;vector<vector<Datascript_runtime.Data_value.t>>>
        empty-used-args {}
        ^:vector<Datascript_runtime.Data_value.t>
        guards (query/rule-gen-guards rule-clause used-args)]
    (is (= ["[(-differ? ?b ?old)]"
            "[(-differ? ?a ?old)]"]
           (query-form-strings guards)))
    (is (= []
           (query/rule-gen-guards
            rule-clause empty-used-args)))
    (let [clauses
          (query-form-vector
           [(query-form-vector
             [a
              (Datascript_runtime.Data_value.Keyword ":name")
              (Datascript_runtime.Data_value.String "A")])])
          active-guard
          (query-form-vector
           [(query-form-list
             [(Datascript_runtime.Data_value.Symbol "-differ?")
              a])])
          pending-guard
          (query-form-vector
           [(query-form-list
             [(Datascript_runtime.Data_value.Symbol "-differ?")
              missing])])
          zero-variable-guard
          (query-form-vector
           [(query-form-list
             [(Datascript_runtime.Data_value.Symbol "-differ?")])])
          ^:vector<Datascript_runtime.Data_value.t>
          guards-to-split
          [active-guard pending-guard zero-variable-guard]
          ^:tuple<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>>
          split
          (query/split-guards
           clauses
           guards-to-split)]
      (is (= ["[(-differ? ?a)]" "[(-differ?)]"]
             (query-form-strings (tuple-get split 0))))
      (is (= ["[(-differ? ?missing)]"]
             (query-form-strings (tuple-get split 1)))))))

(deftest test-public-solve-rule
  (let [database
        (->
         (d/empty-db)
         (d/db-with
          [[:db/add 1 :age 18]
           [:db/add 2 :age 20]
           [:db/add 3 :age 18]]))
        database-view (db/database-view database)
        adult-18-clause
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :age)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Int 18))])
        adult-20-clause
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :age)
          (parser/pattern-constant
           (Datascript_runtime.Data_value.Int 20))])
        ^:vector<datascript.parser/RuleBranch> rule-branches
         [(parser/static-rule-branch
           "adult" ["?e"] [adult-18-clause])
          (parser/static-rule-branch
           "adult" ["?e"] [adult-20-clause])
          (parser/static-rule-branch
           "adult" ["?e"] [adult-18-clause])
          (parser/static-rule-branch
           "has-adult"
           []
           [(parser/pattern-clause
             [(parser/pattern-placeholder)
              (parser/pattern-attribute :age)
              (parser/pattern-constant
               (Datascript_runtime.Data_value.Int 18))])])]
        rules
        (parser/static-rules rule-branches)
        default-context
        (query-types/context
         []
         {"$" (query-types/database-source database-view)}
         rules)
        explicit-context
        (query-types/context
         []
         {"$people" (query-types/database-source database-view)}
         rules)
        adult-call
        (parser/static-rule-clause
         "adult"
         [(parser/pattern-variable "?person")])
        explicit-adult-call
        (parser/static-source-rule-clause
         "$people"
         "adult"
         [(parser/pattern-variable "?person")])]
    (is (= [[1] [3] [2]]
           (relation-int-rows
            (query/solve-rule default-context adult-call)
            ["?person"])))
    (is (= [[1] [3] [2]]
           (relation-int-rows
            (query/solve-rule explicit-context explicit-adult-call)
            ["?person"])))
    (let [exists-call
          (parser/static-rule-clause "has-adult" [])]
      (is (= 1
             (count
              (query-types/relation-rows
               (query/solve-rule default-context exists-call))))))
    (is
     (thrown-msg?
      "solve-rule expects a rule clause"
      (query/solve-rule
       default-context
       (greater-than-clause 10))))))

(deftest test-public-expand-rule
  (reset! query/rule-seqid 0)
  (let [database
        (->
         (d/empty-db)
         (d/db-with
          [[:db/add 1 :age 20]
           [:db/add 2 :age 15]
           [:db/add 2 :score 30]]))
        database-view (db/database-view database)
        age-pattern
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :age)
          (parser/pattern-variable "?age")])
        age-predicate
        (parser/static-predicate-clause
         ">"
         [(parser/variable-argument "?age")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 17))])
        score-pattern
        (parser/pattern-clause
         [(parser/pattern-variable "?e")
          (parser/pattern-attribute :score)
          (parser/pattern-variable "?score")])
        score-predicate
        (parser/static-predicate-clause
         ">"
         [(parser/variable-argument "?score")
          (parser/constant-argument
           (Datascript_runtime.Data_value.Int 20))])
        ^:vector<datascript.parser/RuleBranch> branches
        [(parser/static-rule-branch
          "adult" ["?e"] [age-pattern age-predicate])
         (parser/static-rule-branch
          "adult" ["?e"] [score-pattern score-predicate])]
        rules (parser/static-rules branches)
        context
        (query-types/context
         []
         {"$" (query-types/database-source database-view)}
         rules)
        variable-call
        (parser/static-rule-clause
         "adult"
         [(parser/pattern-variable "?person")])
        ^:map<string;vector<vector<Datascript_runtime.Data_value.t>>>
        used-args
        {"adult"
         [[(Datascript_runtime.Data_value.Symbol "?previous")]]}
        expanded
        (query/expand-rule variable-call context used-args)]
    (is (= 2 (count expanded)))
    (is (= ["?person"
            "?age__auto__1"
            "?age__auto__1"]
           (clause-variable-names (nth expanded 0))))
    (is (= ["?person"
            "?score__auto__1"
            "?score__auto__1"]
           (clause-variable-names (nth expanded 1))))
    (let [first-result
          (query/-q context (nth expanded 0))
          second-result
          (query/-q context (nth expanded 1))]
      (if-some [relation
                (first (query-types/context-relations first-result))]
        (is (= [[1]]
               (relation-int-rows relation ["?person"])))
        (is false))
      (if-some [relation
                (first (query-types/context-relations second-result))]
        (is (= [[2]]
               (relation-int-rows relation ["?person"])))
        (is false)))
    (let [expanded-again
          (query/expand-rule variable-call context {})]
      (is (= ["?person"
              "?age__auto__2"
              "?age__auto__2"]
             (clause-variable-names
              (nth expanded-again 0)))))
    (let [constant-call
          (parser/static-rule-clause
           "adult"
           [(parser/pattern-constant
             (Datascript_runtime.Data_value.Int 1))])
          constant-expanded
          (query/expand-rule constant-call context {})
          first-result
          (query/-q context (nth constant-expanded 0))
          second-result
          (query/-q context (nth constant-expanded 1))]
      (is (= ["?age__auto__3" "?age__auto__3"]
             (clause-variable-names
              (nth constant-expanded 0))))
      (if-some [relation
                (first (query-types/context-relations first-result))]
        (is (= 1
               (count (query-types/relation-rows relation))))
        (is false))
      (if-some [relation
                (first (query-types/context-relations second-result))]
        (is (= 0
               (count (query-types/relation-rows relation))))
        (is false)))
    (is (= 3 @query/rule-seqid))))

(deftest test-public-aggregation
  (let [color
        (parser/variable-find-element "?color")
        sum-x
        (parser/aggregate-find-element
         "sum"
         [(parser/variable-argument "?x")])
        count-x
        (parser/aggregate-find-element
         "count"
         [(parser/variable-argument "?x")])
        elements [color sum-x count-x]
        context (query-types/context [] {} [])
        rows
        [(query-int-row [1 10 10])
         (query-int-row [2 7 7])
         (query-int-row [1 20 20])]]
    (testing "-aggregate evaluates one complete group"
      (is (= [[1 37 3]]
             (query-int-rows
              [(query/-aggregate elements context rows)]))))
    (testing "aggregate groups by every non-aggregate find element"
      (is (= [[1 30 2] [2 7 1]]
             (query-int-rows
              (query/aggregate elements context rows)))))
    (testing "all-aggregate finds form one group"
      (is (= [[37 3]]
             (query-int-rows
              (query/aggregate
               [sum-x count-x]
               context
               [(query-int-row [10 10])
                (query-int-row [7 7])
                (query-int-row [20 20])])))))
    (testing "empty result sets produce no groups"
      (is (= []
             (query/aggregate elements context [])))
      (is (= 0
             (count
              (query/-aggregate elements context [])))))))

(deftest test-public-aggregation-context-resolution
  (let [limit-first
        (query-types/relation
         {"?limit" 0}
         [(query-int-row [2])]
         {})
        limit-second
        (query-types/relation
         {"?limit" 0}
         [(query-int-row [1])]
         {})
        callable
        (query-types/relation
         {"?aggregate" 0}
         [(array
           (query-types/callable-result
            (query-types/callable sum-query-arguments)))]
         {})
        parameterized-max
        (parser/aggregate-find-element
         "max"
         [(parser/variable-argument "?limit")
          (parser/variable-argument "?x")])
        custom-sum
        (parser/custom-aggregate-find-element
         "?aggregate"
         [(parser/variable-argument "?x")])
        rows
        [(query-int-row [1])
         (query-int-row [3])
         (query-int-row [2])]]
    (testing "aggregate parameters use the first matching context relation"
      (let [result
            (query/-aggregate
             [parameterized-max]
             (query-types/context
              [limit-first limit-second]
              {}
              [])
             rows)]
        (if-some [value
                  (query-types/result-value
                   (nth result 0))]
          (is (= "[2 3]"
                 (Datascript_runtime.Data_value.to_edn_string value)))
          (is false))))
    (testing "custom aggregate callables resolve from context"
      (is (= [[6]]
             (query-int-rows
              [(query/-aggregate
                [custom-sum]
                (query-types/context [callable] {} [])
                rows)]))))
    (testing "unbound aggregate parameters retain the upstream error"
      (is
       (thrown-msg?
        "Aggregate parameters must be constants"
        (query/-aggregate
         [parameterized-max]
         (query-types/context [] {} [])
         rows))))))

(deftest test-public-input-resolution
  (let [database
        (datascript.db/database-view (d/empty-db))
        source-binding
        (parser/static-input-binding-form
         (parser/make-static-source-input "$people"))
        rules-binding
        (parser/static-input-binding-form
         (parser/make-static-rules-input))
        scalar-binding
        (parser/static-input-binding-form
         (parser/make-static-value-input
          (parser/scalar-input "?x")))
        tuple-binding
        (parser/static-input-binding-form
         (parser/make-static-value-input
          (parser/tuple-input
           [(parser/scalar-input "?y")
            (parser/ignore-input)])))
        collection-binding
        (parser/static-input-binding-form
         (parser/make-static-value-input
          (parser/collection-input
           (parser/scalar-input "?z"))))
        rule-variable
        (Datascript_runtime.Data_value.Symbol "?e")
        placeholder-clause
        (query-form-vector
         [(Datascript_runtime.Data_value.Symbol "_")])
        rules
        (query/parse-rules
         (query-form-vector
          [(query-rule-branch
            "known"
            [rule-variable]
            [placeholder-clause])]))
        bindings
        [source-binding
         rules-binding
         scalar-binding
         tuple-binding
         collection-binding]
        inputs
        [(query-types/source-input
          (query-types/database-source database))
         (query-types/rules-input rules)
         (query-types/binding-input
          (query-types/scalar-binding
           (query-int-result 10)))
         (query-types/binding-input
          (query-types/collection-binding
           [(query-types/scalar-binding
             (query-int-result 20))
            (query-types/scalar-binding
             (query-int-result 999))]))
         (query-types/binding-input
          (query-types/collection-binding
           [(query-types/scalar-binding
             (query-int-result 30))
            (query-types/scalar-binding
             (query-int-result 40))]))]
        resolved
        (query/resolve-ins
         (query-types/context [] {} [])
         bindings
         inputs)
        relations (query-types/context-relations resolved)]
    (testing "source and rules inputs update their dedicated context fields"
      (if-some [source
                (get
                 (query-types/context-sources resolved)
                 "$people")]
        (is
         (some?
          (query-types/source-database source)))
        (is false))
      (is (= 1
             (count (query-types/context-rules resolved)))))
    (testing "value inputs append scalar, tuple, and collection relations"
      (is (= 3 (count relations)))
      (is (= [[10]]
             (relation-int-rows
              (nth relations 0)
              ["?x"])))
      (is (= [[20]]
             (relation-int-rows
              (nth relations 1)
              ["?y"])))
      (is (= [[30] [40]]
             (relation-int-rows
              (nth relations 2)
              ["?z"]))))))

(deftest test-public-single-input-resolution-and-count-errors
  (let [existing
        (query-types/relation
         {"?existing" 0}
         [(query-int-row [1])]
         {})
        scalar-binding
        (parser/static-input-binding-form
         (parser/make-static-value-input
          (parser/scalar-input "?x")))
        source-binding
        (parser/static-input-binding-form
         (parser/make-static-source-input "$people"))
        input
        (query-types/binding-input
         (query-types/scalar-binding
          (query-int-result 2)))
        resolved
        (query/resolve-in
         (query-types/context [existing] {} [])
         (tuple scalar-binding input))
        relations (query-types/context-relations resolved)]
    (testing "resolve-in preserves existing relations and appends one binding"
      (is (= 2 (count relations)))
      (is (= [[1]]
             (relation-int-rows
              (nth relations 0)
              ["?existing"])))
      (is (= [[2]]
             (relation-int-rows
              (nth relations 1)
              ["?x"]))))
    (testing "resolve-ins rejects extra inputs with upstream source forms"
      (is
       (thrown-msg?
        "Extra inputs passed, expected: [$people ?x], got: 3"
        (query/resolve-ins
         (query-types/context [] {} [])
         [source-binding scalar-binding]
         [(query-types/source-input
           (query-types/database-source
            (datascript.db/database-view (d/empty-db))))
          input
          input]))))
    (testing "resolve-ins rejects missing inputs with upstream source forms"
      (is
       (thrown-msg?
        "Too few inputs passed, expected: [$people ?x], got: 1"
        (query/resolve-ins
         (query-types/context [] {} [])
         [source-binding scalar-binding]
         [(query-types/source-input
           (query-types/database-source
            (datascript.db/database-view
             (d/empty-db))))]))))))

(deftest test-public-pattern-lookup
  (let [database
        (datascript.db/database-view
         (d/db-with
          (d/empty-db
           {:person/email
            {:db/unique :db.unique/identity}})
          [{:db/id 1
            :person/email "ivan@example.com"
            :person/name "Ivan"
            :person/age 30}
           {:db/id 2
            :person/email "petr@example.com"
            :person/name "Petr"
            :person/age 40}]))
        entity (Datascript_runtime.Data_value.Symbol "?e")
        name (Datascript_runtime.Data_value.Symbol "?name")
        wanted (Datascript_runtime.Data_value.Symbol "?wanted")
        email-lookup
        (query-form-vector
         [(Datascript_runtime.Data_value.Keyword ":person/email")
          (Datascript_runtime.Data_value.String
           "petr@example.com")])
        name-pattern
        [entity
         (Datascript_runtime.Data_value.Keyword ":person/name")
         name]
        context
        (query-types/context
         [(query-types/relation
           {"?wanted" 0}
           [(array
             (query-types/value-result
              (Datascript_runtime.Data_value.String "Ivan")))]
           {})]
         {}
         [])]
    (testing "lookup-pattern-db projects variables from indexed datoms"
      (is
       (=
        [[(Datascript_runtime.Data_value.Int 1)
          (Datascript_runtime.Data_value.String "Ivan")]
         [(Datascript_runtime.Data_value.Int 2)
          (Datascript_runtime.Data_value.String "Petr")]]
        (relation-data-rows
         (query/lookup-pattern-db
          (query-types/context [] {} [])
          database
          name-pattern)
         ["?e" "?name"]))))
    (testing "context constants are substituted before DB lookup"
      (is (= [[1]]
             (relation-int-rows
              (query/lookup-pattern-db
               context
               database
               [entity
                (Datascript_runtime.Data_value.Keyword
                 ":person/name")
                wanted])
              ["?e"]))))
    (testing "entity lookup refs resolve through the database"
      (is (= [[40]]
             (relation-int-rows
              (query/lookup-pattern-db
               (query-types/context [] {} [])
               database
               [email-lookup
                (Datascript_runtime.Data_value.Keyword
                 ":person/age")
                (Datascript_runtime.Data_value.Symbol "?age")])
              ["?age"]))))
    (testing "lookup-pattern dispatches a closed database source"
      (is (= [[1] [2]]
             (relation-int-rows
              (query/lookup-pattern
               (query-types/context [] {} [])
               (query-types/database-source database)
               name-pattern)
              ["?e"]))))
    (testing "invalid DB pattern arity keeps the typed index error"
      (is
       (thrown-msg?
        "DataScript patterns must contain one to five elements"
        (query/lookup-pattern-db
         (query-types/context [] {} [])
         database
         []))))))

(deftest test-public-collection-pattern-lookup
  (let [rows
        [(query-int-row [1 1])
         (query-int-row [1 2])
         (query-int-row [2 2])
         (query-int-row [2 2])]
        repeated
        [(Datascript_runtime.Data_value.Symbol "?x")
         (Datascript_runtime.Data_value.Symbol "?x")]
        context (query-types/context [] {} [])]
    (testing "collection patterns enforce repeated-variable equality"
      (is (= [[1] [2] [2]]
             (relation-int-rows
              (query/lookup-pattern-coll
               context rows repeated)
              ["?x"]))))
    (testing "collection lookup preserves an empty relation schema"
      (let [relation
            (query/lookup-pattern-coll
             context
             []
             [(Datascript_runtime.Data_value.Symbol "?x")])]
        (is (= {"?x" 0}
               (query-types/relation-attrs relation)))
        (is (= []
               (query-types/relation-rows relation)))))
    (testing "lookup-pattern dispatches a closed relation source"
      (is (= [[1] [2] [2]]
             (relation-int-rows
              (query/lookup-pattern
               context
               (query-types/relation-source rows)
               repeated)
              ["?x"]))))))

(deftest test-public-tuples-to-return-map
  (let [rows
        [(query-int-row [1 10])
         (query-int-row [1 10])
         (query-int-row [2 20])]
        keyword-map
        (datascript.parser/ReturnKeys [:x :y])
        symbol-map
        (datascript.parser/ReturnSyms
         [(symbol "x") (symbol "y")])
        string-map
        (datascript.parser/ReturnStrs ["x" "y"])]
    (testing "keyword keys preserve tuple order and duplicates"
      (if-some [mapped
                (query-types/output-keyword-relation
                 (query/tuples->return-map
                  keyword-map rows))]
        (is (= [[1 10] [1 10] [2 20]]
               (mapped-int-rows mapped [":x" ":y"])))
        (is false)))
    (testing "symbol and string return maps keep their closed variants"
      (if-some [mapped
                (query-types/output-symbol-relation
                 (query/tuples->return-map
                  symbol-map rows))]
        (is (= [[1 10] [1 10] [2 20]]
               (mapped-int-rows mapped ["x" "y"])))
        (is false))
      (if-some [mapped
                (query-types/output-string-relation
                 (query/tuples->return-map
                  string-map rows))]
        (is (= [[1 10] [1 10] [2 20]]
               (mapped-int-rows mapped ["x" "y"])))
        (is false)))
    (testing "row arity mismatch retains the upstream mapping failure"
      (is
       (thrown-msg?
        "Return-map key count must match result row arity"
        (query/tuples->return-map
         keyword-map
         [(query-int-row [1])]))))))

(deftest test-public-post-process-protocol
  (let [element
        (datascript.parser/FindVariable
         (datascript.parser/Variable. (symbol "?x")))
        relation (datascript.parser/FindRel. [element])
        collection (datascript.parser/FindColl. element)
        scalar (datascript.parser/FindScalar. element)
        tuple-result
        (datascript.parser/FindTuple. [element element])
        keyword-map
        (datascript.parser/ReturnKeys [:x :y])
        rows
        [(query-int-row [1 10])
         (query-int-row [2 20])]]
    (testing "all upstream find records implement IPostProcess"
      (is (satisfies? query/IPostProcess relation))
      (is (satisfies? query/IPostProcess collection))
      (is (satisfies? query/IPostProcess scalar))
      (is (satisfies? query/IPostProcess tuple-result)))
    (testing "relation output preserves rows and optional mapping"
      (if-some [plain
                (query-types/output-relation
                 (query/-post-process relation None rows))]
        (is (= [[1 10] [2 20]]
               (query-int-rows plain)))
        (is false))
      (if-some [mapped
                (query-types/output-keyword-relation
                 (query/-post-process
                  relation (Some keyword-map) rows))]
        (is (= [[1 10] [2 20]]
               (mapped-int-rows mapped [":x" ":y"])))
        (is false)))
    (testing "collection and scalar shapes ignore return-map"
      (if-some [values
                (query-types/output-collection
                 (query/-post-process
                  collection (Some keyword-map) rows))]
        (is (= [1 2]
               (mapv query-result-int values)))
        (is false))
      (if-some [scalar-output
                (query-types/output-scalar
                 (query/-post-process
                  scalar (Some keyword-map) rows))]
        (if-some [value scalar-output]
          (is (= 1 (query-result-int value)))
          (is false))
        (is false))
      (if-some [scalar-output
                (query-types/output-scalar
                 (query/-post-process scalar None []))]
        (is (= None scalar-output))
        (is false)))
    (testing "tuple shape reads only the first tuple"
      (if-some [tuple-output
                (query-types/output-tuple
                 (query/-post-process
                  tuple-result None rows))]
        (if-some [row tuple-output]
          (is (= [1 10]
                 (mapv query-result-int row)))
          (is false))
        (is false))
      (if-some [mapped
                (query-types/output-keyword-tuple
                 (query/-post-process
                  tuple-result
                  (Some keyword-map)
                  [(query-int-row [1 10])
                   (query-int-row [999])]))]
        (if-some [row mapped]
          (is (= [[1 10]]
                 (mapped-int-rows [row] [":x" ":y"])))
          (is false))
        (is false))
      (if-some [tuple-output
                (query-types/output-tuple
                 (query/-post-process tuple-result None []))]
        (is (= None tuple-output))
        (is false)))))

(deftest test-public-context-resolution-protocol
  (let [database
        (datascript.db/database-view (d/empty-db))
        source
        (query-types/database-source database)
        first-x
        (query-types/relation
         {"?x" 0}
         [(query-int-row [1])]
         {})
        second-x
        (query-types/relation
         {"?x" 0}
         [(query-int-row [2])]
         {})
        context
        (query-types/context
         [first-x second-x]
         {"$people" source}
         [])
        variable
        (datascript.parser/Variable. (symbol "?x"))
        missing-variable
        (datascript.parser/Variable. (symbol "?missing"))
        source-variable
        (datascript.parser/SrcVar. (symbol "$people"))
        missing-source
        (datascript.parser/SrcVar. (symbol "$missing"))
        sum-symbol
        (datascript.parser/PlainSymbol. (symbol "sum"))
        unknown-symbol
        (datascript.parser/PlainSymbol.
         (symbol "example/unknown"))
        constant
        (datascript.parser/Constant.
         (Datascript_runtime.Data_value.Int 42))]
    (testing "all parser resolver records implement IContextResolve"
      (is (satisfies? query/IContextResolve variable))
      (is (satisfies? query/IContextResolve source-variable))
      (is (satisfies? query/IContextResolve sum-symbol))
      (is (satisfies? query/IContextResolve constant)))
    (testing "variables resolve from the first matching relation and row"
      (match (query/-context-resolve variable context)
        (Some (query/ContextResult result))
        (is (= 1 (query-result-int result)))
        _ (is false))
      (is (= None
             (query/-context-resolve
              missing-variable context))))
    (testing "sources resolve without erasing their closed source variant"
      (match (query/-context-resolve source-variable context)
        (Some (query/ContextSource resolved))
        (is
         (some?
          (query-types/source-database resolved)))
        _ (is false))
      (is (= None
             (query/-context-resolve
              missing-source context))))
    (testing "plain symbols resolve only through the typed aggregate registry"
      (match (query/-context-resolve sum-symbol context)
        (Some (query/ContextAggregate aggregate))
        (is (datascript.built-ins/sum-aggregate? aggregate))
        _ (is false))
      (is (= None
             (query/-context-resolve
              unknown-symbol context))))
    (testing "constants become closed query results"
      (match (query/-context-resolve constant context)
        (Some (query/ContextResult result))
        (is (= 42 (query-result-int result)))
        _ (is false)))))

(deftest test-public-context-resolution-empty-first-relation
  (let [empty-first
        (query-types/relation {"?x" 0} [] {})
        later
        (query-types/relation
         {"?x" 0}
         [(query-int-row [9])]
         {})
        variable
        (datascript.parser/Variable. (symbol "?x"))]
    (is
     (=
      None
      (query/-context-resolve
       variable
       (query-types/context
        [empty-first later]
        {}
        []))))))

(deftest test-public-collect
  (let [left
        (query-types/relation
         {"?x" 0}
         [(query-int-row [1])
          (query-int-row [2])]
         {})
        right
        (query-types/relation
         {"?y" 0}
         [(query-int-row [10])
          (query-int-row [20])]
         {})
        irrelevant
        (query-types/relation
         {"?z" 0}
         [(query-int-row [999])]
         {})
        context
        (query-types/context
         [left irrelevant right]
         {}
         [])]
    (testing "zero relations retain one all-absent seed row"
      (is (= [[(absent-int) (absent-int)]]
             (optional-int-rows
              (query/-collect
               (query-types/context [] {} [])
               ["?x" "?y"])))))
    (testing "multiple relations fill requested columns in symbol order"
      (is (= [[(Some 10) (Some 1)]
              [(Some 20) (Some 1)]
              [(Some 10) (Some 2)]
              [(Some 20) (Some 2)]]
             (optional-int-rows
              (query/-collect context ["?y" "?x"])))))
    (testing "unbound symbols remain option absence"
      (is (= [[(Some 1) (absent-int)]
              [(Some 2) (absent-int)]]
             (optional-int-rows
              (query/-collect
               (query-types/context [left] {} [])
               ["?x" "?missing"])))))
    (testing "three-arity collect preserves the supplied accumulator"
      (is (= [[(Some 100) (Some 1)]
              [(Some 100) (Some 2)]]
             (optional-int-rows
              (query/-collect
               [(array
                 (Some (query-int-result 100))
                 (absent-query-result))]
               [left]
               ["?seed" "?x"])))))
    (testing "any empty relation short-circuits even when irrelevant"
      (is (= []
             (query/-collect
              (query-types/context
               [left
                (query-types/relation {"?z" 0} [] {})]
               {}
               [])
              ["?x"]))))))

(deftest test-public-collect-tuples-and-uniqueness
  (let [relation
        (query-types/relation
         {"?x" 0 "?y" 1}
         [(query-int-row [1 2])
          (query-int-row [1 2])]
         {})
        seed
        [(array
          (absent-query-result)
          (Some (query-int-result 50))
          (absent-query-result))]
        copy-map (array (Some 1) (absent-int) (Some 0))]
    (testing "-collect-tuples copies only mapped positions"
      (is (= [[(Some 2) (Some 50) (Some 1)]
              [(Some 2) (Some 50) (Some 1)]]
             (optional-int-rows
              (query/-collect-tuples
               seed relation 3 copy-map)))))
    (testing "-collect preserves duplicates while collect removes them"
      (is (= [[(Some 1)] [(Some 1)]]
             (optional-int-rows
              (query/-collect
               (query-types/context [relation] {} [])
               ["?x"]))))
      (is (= [[(Some 1)]]
             (optional-int-rows
              (query/collect
               (query-types/context [relation] {} [])
               ["?x"])))))
    (testing "collect keeps first-seen order while removing duplicates"
      (let [ordered
            (query-types/relation
             {"?x" 0}
             [(query-int-row [2])
              (query-int-row [1])
              (query-int-row [2])]
             {})]
        (is (= [[(Some 2)] [(Some 1)]]
               (optional-int-rows
                (query/collect
                 (query-types/context [ordered] {} [])
                 ["?x"]))))))))

(deftest test-public-binding-protocol
  (let [ignored (parser/ignore-input)
        scalar (parser/scalar-input "?x")
        tuple-binding
        (parser/tuple-input
         [(parser/scalar-input "?x")
          (parser/scalar-input "?y")])
        collection-binding
        (parser/collection-input
         (parser/scalar-input "?x"))
        scalar-value
        (query-types/scalar-binding
         (query-int-result 10))
        tuple-value
        (query-types/collection-binding
         [(query-types/scalar-binding
           (query-int-result 20))
          (query-types/scalar-binding
           (query-int-result 30))])
        collection-value
        (query-types/collection-binding
         [(query-types/scalar-binding
           (query-int-result 40))
          (query-types/scalar-binding
           (query-int-result 50))])]
    (testing "all closed binding variants implement IBinding"
      (is (satisfies? query/IBinding ignored))
      (is (satisfies? query/IBinding scalar))
      (is (satisfies? query/IBinding tuple-binding))
      (is (satisfies? query/IBinding collection-binding)))
    (testing "ignore produces the identity relation"
      (let [relation (query/in->rel ignored scalar-value)]
        (is (= {} (query-types/relation-attrs relation)))
        (is (= [[]] (relation-int-rows relation [])))))
    (testing "scalar, tuple, and collection bindings preserve upstream rows"
      (is (= [[10]]
             (relation-int-rows
              (query/in->rel scalar scalar-value)
              ["?x"])))
      (is (= [[20 30]]
             (relation-int-rows
              (query/in->rel tuple-binding tuple-value)
              ["?x" "?y"])))
      (is (= [[40] [50]]
             (relation-int-rows
              (query/in->rel collection-binding collection-value)
              ["?x"]))))
    (testing "empty collections retain binding attributes with no rows"
      (let [relation
            (query/in->rel
             collection-binding
             (query-types/collection-binding []))]
        (is (= {"?x" 0}
               (query-types/relation-attrs relation)))
        (is (= []
               (query-types/relation-rows relation)))))))

(deftest test-public-binding-edge-cases
  (let [tuple-binding
        (parser/tuple-input
         [(parser/scalar-input "?x")
          (parser/scalar-input "?y")])
        extra-tuple-value
        (query-types/collection-binding
         [(query-types/scalar-binding
           (query-int-result 1))
          (query-types/scalar-binding
           (query-int-result 2))
          (query-types/scalar-binding
           (query-int-result 999))])
        short-tuple-value
        (query-types/collection-binding
         [(query-types/scalar-binding
           (query-int-result 1))])
        nested-binding
        (parser/collection-input
         (parser/tuple-input
          [(parser/scalar-input "?x")
           (parser/ignore-input)]))
        nested-value
        (query-types/collection-binding
         [(query-types/collection-binding
           [(query-types/scalar-binding
             (query-int-result 3))
            (query-types/scalar-binding
             (query-int-result 30))])
          (query-types/collection-binding
           [(query-types/scalar-binding
             (query-int-result 4))
            (query-types/scalar-binding
             (query-int-result 40))])])]
    (testing "tuple bindings ignore extra input elements like upstream"
      (is (= [[1 2]]
             (relation-int-rows
              (query/in->rel tuple-binding extra-tuple-value)
              ["?x" "?y"]))))
    (testing "nested collection and tuple bindings recurse through IBinding"
      (is (= [[3] [4]]
             (relation-int-rows
              (query/in->rel nested-binding nested-value)
              ["?x"]))))
    (testing "tuple bindings reject too few elements"
      (is
       (thrown-msg?
        "Tuple query input has too few values"
        (query/in->rel tuple-binding short-tuple-value))))
    (testing "collection bindings reject scalar values"
      (is
       (thrown-msg?
        "Collection query input requires a Collection_binding"
        (query/in->rel
         (parser/collection-input
          (parser/scalar-input "?x"))
         (query-types/scalar-binding
          (query-int-result 1))))))))

(deftest test-joins
  (let [db (-> (d/empty-db)
             (d/db-with [{:db/id 1, :name  "Ivan", :age   15}
                         {:db/id 2, :name  "Petr", :age   37}
                         {:db/id 3, :name  "Ivan", :age   37}
                         {:db/id 4, :age 15}]))]
    (is (tdc/query-relation?
         (d/q '[:find ?e
                :where [?e :name]] db)
         [[1] [2] [3]]))
    (is (tdc/query-relation?
         (d/q '[:find  ?e ?v
                :where [?e :name "Ivan"]
                [?e :age ?v]] db)
         [[1 15] [3 37]]))
    (is (tdc/query-relation?
         (d/q '[:find  ?e1 ?e2
                :where [?e1 :name ?n]
                [?e2 :name ?n]] db)
         [[1 1] [2 2] [3 3] [1 3] [3 1]]))
    (is (tdc/query-relation?
         (d/q '[:find  ?e ?e2 ?n
                :where [?e :name "Ivan"]
                [?e :age ?a]
                [?e2 :age ?a]
                [?e2 :name ?n]] db)
         [[1 1 "Ivan"]
          [3 3 "Ivan"]
          [3 2 "Petr"]]))))

(deftest test-q-many
  (let [db (-> (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
             (d/db-with [[:db/add 1 :name "Ivan"]
                         [:db/add 1 :aka  "ivolga"]
                         [:db/add 1 :aka  "pi"]
                         [:db/add 2 :name "Petr"]
                         [:db/add 2 :aka  "porosenok"]
                         [:db/add 2 :aka  "pi"]]))]
    (is (tdc/query-relation?
         (d/q '[:find  ?n1 ?n2
                :where [?e1 :aka ?x]
                [?e2 :aka ?x]
                [?e1 :name ?n1]
                [?e2 :name ?n2]] db)
         [["Ivan" "Ivan"]
          ["Petr" "Petr"]
          ["Ivan" "Petr"]
          ["Petr" "Ivan"]]))))

(deftest test-q-coll
  (is (tdc/query-relation?
       (d/q '[:find  ?n ?a
              :where [?e :aka "dragon_killer_94"]
              [?e :name ?n]
              [?e :age  ?a]]
            [[1 :name "Ivan"]
             [1 :age  19]
             [1 :aka  "dragon_killer_94"]
             [1 :aka  "-=autobot=-"]])
       [["Ivan" 19]]))

  (testing "Query over long tuples"
    (is (tdc/query-relation?
         (d/q '[:find  ?e ?v
                :where [?e :name ?v]]
              [[1 :name "Ivan" 945 :db/add]
               [1 :age  39     999 :db/retract]])
         [[1 "Ivan"]]))
    (is (tdc/query-relation?
         (d/q '[:find  ?e ?a ?v ?t
                :where [?e ?a ?v ?t :db/retract]]
              [[1 :name "Ivan" 945 :db/add]
               [1 :age  39     999 :db/retract]])
         [[1 :age 39 999]]))))

(deftest test-q-in
  (let [db (-> (d/empty-db)
             (d/db-with [{:db/id 1, :name  "Ivan", :age   15}
                         {:db/id 2, :name  "Petr", :age   37}
                         {:db/id 3, :name  "Ivan", :age   37}]))]
    (is (tdc/query-relation?
         (d/q '{:find  [?e]
                :in    [$ ?attr ?value]
                :where [[?e ?attr ?value]]}
              db :name "Ivan")
         [[1] [3]]))
    (is (tdc/query-relation?
         (d/q '{:find  [?e]
                :in    [$ ?attr ?value]
                :where [[?e ?attr ?value]]}
              db :age 37)
         [[2] [3]]))

    (testing "Named DB"
      (is (tdc/query-relation?
           (d/q '[:find  ?a ?v
                  :in    $db ?e
                  :where [$db ?e ?a ?v]] db 1)
           [[:name "Ivan"]
            [:age 15]])))

    (testing "DB join with collection"
      (is (tdc/query-relation?
           (d/q '[:find  ?e ?email
                  :in    $ $b
                  :where [?e :name ?n]
                  [$b ?n ?email]]
                db
                [["Ivan" "ivan@mail.ru"]
                 ["Petr" "petr@gmail.com"]])
           [[1 "ivan@mail.ru"]
            [2 "petr@gmail.com"]
            [3 "ivan@mail.ru"]])))
    
    (testing "Query without DB"
      (is (tdc/query-relation?
           (d/q '[:find ?a ?b
                  :in   ?a ?b]
                10 20)
           [[10 20]])))

    (is (thrown-msg? "Extra inputs passed, expected: [], got: 1"
          (d/q '[:find ?e :where [(inc 1) ?e]] db)))

    (is (thrown-msg? "Too few inputs passed, expected: [$ $2], got: 1"
          (d/q '[:find ?e :in $ $2 :where [?e]] db)))

    (is (thrown-msg? "Extra inputs passed, expected: [$], got: 2"
          (d/q '[:find ?e :where [?e]] db db)))

    (is (thrown-msg? "Extra inputs passed, expected: [$ $2], got: 3"
          (d/q '[:find ?e :in $ $2 :where [?e]] db db db)))))

(deftest test-bindings
  (let [db (-> (d/empty-db)
             (d/db-with [{:db/id 1, :name  "Ivan", :age   15}
                         {:db/id 2, :name  "Petr", :age   37}
                         {:db/id 3, :name  "Ivan", :age   37}]))]
    (testing "Relation binding"
      (is (tdc/query-relation?
           (d/q '[:find  ?e ?email
                  :in    $ [[?n ?email]]
                  :where [?e :name ?n]]
                db
                [["Ivan" "ivan@mail.ru"]
                 ["Petr" "petr@gmail.com"]])
           [[1 "ivan@mail.ru"]
            [2 "petr@gmail.com"]
            [3 "ivan@mail.ru"]])))

    (testing "Tuple binding"
      (is (tdc/query-relation?
           (d/q '[:find  ?e
                  :in    $ [?name ?age]
                  :where [?e :name ?name]
                  [?e :age ?age]]
                db ["Ivan" 37])
           [[3]])))

    (testing "Collection binding"
      (is (tdc/query-relation?
           (d/q '[:find  ?attr ?value
                  :in    $ ?e [?attr ...]
                  :where [?e ?attr ?value]]
                db 1 [:name :age])
           [[:name "Ivan"] [:age 15]])))

    (testing "Empty coll handling"
      (is (tdc/query-relation?
           (d/q '[:find ?id
                  :in $ [?id ...]
                  :where [?id :age _]]
                [[1 :name "Ivan"]
                 [2 :name "Petr"]]
                [])
           []))
      (is (tdc/query-relation?
           (d/q '[:find ?id
                  :in $ [[?id]]
                  :where [?id :age _]]
                [[1 :name "Ivan"]
                 [2 :name "Petr"]]
                [])
           [])))
    
    (testing "Placeholders"
      (is (tdc/query-relation?
           (d/q '[:find ?x ?z
                  :in [?x _ ?z]]
                [:x :y :z])
           [[:x :z]]))
      (is (tdc/query-relation?
           (d/q '[:find ?x ?z
                  :in [[?x _ ?z]]]
                [[:x :y :z] [:a :b :c]])
           [[:x :z] [:a :c]])))
    
    (testing "Error reporting"
      (is (thrown-msg? "Cannot bind value :a to tuple [?a ?b]"
            (d/q '[:find ?a ?b :in [?a ?b]] :a)))
      (is (thrown-msg? "Cannot bind value :a to collection [?a ...]"
            (d/q '[:find ?a :in [?a ...]] :a)))
      (is (thrown-msg? "Not enough elements in a collection [:a] to bind tuple [?a ?b]"
            (d/q '[:find ?a ?b :in [?a ?b]] [:a]))))))

(defn ^:int query-int [^query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Value
     (Datascript_runtime.Data_value.Int value))
    value
    _
    (Stdlib.invalid_arg "Expected an integer query argument")))

(defn ^:int data-int [^:Datascript_runtime.Data_value.t value]
  (match value
    (Datascript_runtime.Data_value.Int value) value
    _ (Stdlib.invalid_arg "Expected an integer data value")))

(defn ^:option<Datascript_runtime.Data_value.t> min-max-values
  [^:vector<query-types/result> arguments]
  (if-some [argument (first arguments)]
    (match argument
      (Datascript_runtime.Query_value.Value value)
      (if-some [values
                (Datascript_runtime.Data_value.sequential_items value)]
        (Some
         (Datascript_runtime.Data_value.vector_of_vector
          [(Datascript_runtime.Data_value.Int
            (reduce min (mapv data-int values)))
           (Datascript_runtime.Data_value.Int
            (reduce max (mapv data-int values)))]))
        None)
      _ None)
    None))

(defn ^:option<Datascript_runtime.Data_value.t> integer-range
  [^:vector<query-types/result> arguments]
  (if-some [minimum (first arguments)]
    (if-some [maximum (first (subvec arguments 1))]
      (Some
       (Datascript_runtime.Data_value.vector_of_vector
        (mapv
         (fn [^:int value]
           (Datascript_runtime.Data_value.Int value))
         (range
          (query-int minimum)
          (query-int maximum)))))
      None)
    None))

(defn ^:option<Datascript_runtime.Data_value.t> constant-five
  [^:vector<query-types/result> _]
  (Some (Datascript_runtime.Data_value.Int 5)))

(deftest test-nested-bindings
  (is (tdc/query-relation?
       (d/q '[:find  ?k ?v
              :in    [[?k ?v] ...]
              :where [(> ?v 1)]]
            {:a 1, :b 2, :c 3})
       [[:b 2] [:c 3]]))

  (is (tdc/query-relation?
       (d/q '[:find  ?k ?min ?max
              :in    [[?k ?v] ...] ?minmax
              :where [(?minmax ?v) [?min ?max]]
              [(> ?max ?min)]]
            {:a [1 2 3 4]
             :b [5 6 7]
             :c [3]}
            min-max-values)
       [[:a 1 4] [:b 5 7]]))

  (is (tdc/query-relation?
       (d/q '[:find  ?k ?x
              :in    [[?k [?min ?max]] ...] ?range
              :where [(?range ?min ?max) [?x ...]]
              [(even? ?x)]]
            {:a [1 7]
             :b [2 4]}
            integer-range)
       [[:a 2] [:a 4] [:a 6]
        [:b 2]])))

(deftest test-built-in-regex
  (is (tdc/query-relation?
       (d/q '[:find  ?name
              :in    [?name ...] ?key
              :where [(re-pattern ?key) ?pattern]
              [(re-find ?pattern ?name)]]
            #{"abc" "abcX" "aXb"}
            "X")
       [["abcX"] ["aXb"]])))

(deftest test-built-in-get
  (is (tdc/query-relation?
       (d/q '[:find ?m ?m-value
              :in [[?k ?m] ...] ?m-key
              :where [(get ?m ?m-key) ?m-value]]
            {:a {:b 1}
             :c {:d 2}}
            :d)
       [[{:d 2} 2]])))

(deftest ^{:doc "issue-385"} test-join-unrelated
  (is (tdc/query-relation?
       (d/q '[:find ?name
              :in $ ?my-fn
              :where [?e :person/name ?name]
              [(?my-fn) ?result]
              [(< ?result 3)]]
            (d/db-with (d/empty-db) [{:person/name "Joe"}])
            constant-five)
       [])))

(deftest ^{:doc "issue-425"} test-symbol-comparison
  (is (tdc/query-collection?
       (d/q
         '[:find [?e ...]
           :where [?e :s b]]
         '[[1 :s a]
           [2 :s b]])
       [2]))
  (let [db (-> (d/empty-db)
             (d/db-with
              [{:db/id 1
                :s (Datascript_runtime.Data_value.Symbol "a")}
               {:db/id 2
                :s (Datascript_runtime.Data_value.Symbol "b")}]))]
    (is (tdc/query-collection?
         (d/q
           '[:find [?e ...]
             :where [?e :s b]]
           db)
         [2]))))

(deftest ^{:doc "issue-462"} test-constant-substitution
  (let [db     (-> (d/empty-db {:a {:db/index true}
                                :b {:db/index true}
                                :c {:db/index true}})
                 (d/db-with
                   (vec
                    (for [eid  (range 1 11)
                          attr [:a :b :c]]
                      (db/tx-add
                       (Datascript_runtime.Data_value.Entity_id eid)
                       attr
                       (Datascript_runtime.Data_value.String
                        (str eid (name attr))))))))]
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result (d/q '[:find ?v :where [5 :b ?v]] filtered)]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [["5b"]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result (d/q '[:find ?a :where [5 ?a "5b"]] filtered)]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [[:b]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result (d/q '[:find ?e :where [?e :b "5b"]] filtered)]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [[5]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result
          (d/q '[:find ?e ?a ?v
                 :in $ ?e ?a
                 :where [?e ?a ?v]]
               filtered 5 :b)]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [[5 :b "5b"]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result
          (d/q '[:find ?e2 ?a ?v
                 :in $ ?a ?v
                 :where [?e ?a ?v]
                        [?e2 ?a ?v]]
               filtered :b "5b")]
      (is (= 2 @counter))
      (is (tdc/query-relation? result [[5 :b "5b"]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result
          (d/q '[:find ?a ?v
                 :in $ ?e
                 :where [?e ?a ?v]]
               filtered 5)]
      (is (= 3 @counter))
      (is (tdc/query-relation?
           result
           [[:a "5a"] [:b "5b"] [:c "5c"]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result (d/q '[:find ?e ?a :where [?e ?a "5b"]] filtered)]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [[5 :b]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result
          (d/q '[:find ?e ?a
                 :in $ ?v
                 :where [?e ?a ?v]]
               filtered "5b")]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [[5 :b]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result
          (d/q '[:find ?e ?a
                 :in $ [?v ...]
                 :where [?e ?a ?v]]
               filtered ["5b"])]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [[5 :b]])))
    (let [counter (volatile! 0)
          filtered (d/filter db
                     (fn [_ _]
                       (vswap! counter inc)
                       true))
          result
          (d/q '[:find ?e ?a
                 :where [(ground "5b") ?v]
                        [?e ?a ?v]]
               filtered)]
      (is (= 1 @counter))
      (is (tdc/query-relation? result [[5 :b]])))))
