(ns datascript.test.query-fns
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.lg.query-types :as query-types]
    [datascript.parser :as parser]
    [datascript.query-v3 :as query-v3]
    [datascript.test.core :as tdc]))

(defn ^:int query-int [^query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Entity value) value
    (Datascript_runtime.Query_value.Value
     (Datascript_runtime.Data_value.Int value))
    value
    _ (Stdlib.invalid_arg "Expected an integer query argument")))

(defn ^datascript.db/database-view query-database
  [^query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Database database) database
    _ (Stdlib.invalid_arg "Expected a database query argument")))

(defn ^:option<Datascript_runtime.Data_value.t> adult?
  [^:vector<query-types/result> arguments]
  (if-some [age (first arguments)]
    (Some
     (Datascript_runtime.Data_value.Bool
      (> (query-int age) 18)))
    None))

(defn ^:option<Datascript_runtime.Data_value.t> keep-even
  [^:vector<query-types/result> arguments]
  (if-some [value (first arguments)]
    (let [value (query-int value)]
      (if (even? value)
        (Some (Datascript_runtime.Data_value.Int value))
        None))
    None))

(defn ^:option<Datascript_runtime.Data_value.t> entity-age?
  [^:vector<query-types/result> arguments]
  (if-some [database-result (first arguments)]
    (if-some [entity-result (first (subvec arguments 1))]
      (if-some [age-result (first (subvec arguments 2))]
        (let [database (query-database database-result)
              entity (query-int entity-result)
              age (query-int age-result)
              datoms
              (db/database-view-search-vector
               database (Some entity) (Some :age) None None)]
          (Some
           (Datascript_runtime.Data_value.Bool
            (if-some [datom (first datoms)]
              (Datascript_runtime.Data_value.equal
               (.-v datom)
               (Datascript_runtime.Data_value.Int age))
              false))))
        None)
      None)
    None))

(defn scalar-output-value?
  [^query-types/output output
   ^:Datascript_runtime.Data_value.t expected]
  (match (query-types/output-scalar output)
    (Some (Some result))
    (Datascript_runtime.Data_value.equal
     (tdc/query-result-value result)
     expected)
    _ false))

(defn scalar-output-nan?
  [^query-types/output output]
  (match (query-types/output-scalar output)
    (Some
     (Some
      (Datascript_runtime.Query_value.Value
       (Datascript_runtime.Data_value.Float value))))
    (Float.is_nan value)
    _ false))

(defn scalar-output-missing?
  [^query-types/output output]
  (match (query-types/output-scalar output)
    (Some None) true
    _ false))

(defn scalar-output-float-in-range?
  [^query-types/output output
   ^:float lower
   ^:float upper]
  (match (query-types/output-scalar output)
    (Some
     (Some
      (Datascript_runtime.Query_value.Value
       (Datascript_runtime.Data_value.Float value))))
    (and (<= lower value) (< value upper))
    _ false))

(defn scalar-output-int-in-range?
  [^query-types/output output
   ^:int lower
   ^:int upper]
  (match (query-types/output-scalar output)
    (Some
     (Some
      (Datascript_runtime.Query_value.Value
       (Datascript_runtime.Data_value.Int value))))
    (and (<= lower value) (< value upper))
    _ false))

(deftest test-core-numeric-query-functions
  (testing "division preserves upstream arities and floating-point results"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(/ 8 2 2) ?x]])
      (Datascript_runtime.Data_value.Float 2.0)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(/ 4) ?x]])
      (Datascript_runtime.Data_value.Float 0.25)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(/ 1 0) ?x]])
      (Datascript_runtime.Data_value.Float ##Inf)))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(/) ?x]]))))

  (testing "quotient, remainder, and modulo retain negative-number semantics"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(quot -7 3) ?x]])
      (Datascript_runtime.Data_value.Int -2)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(rem -7 3) ?x]])
      (Datascript_runtime.Data_value.Int -1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(mod -7 3) ?x]])
      (Datascript_runtime.Data_value.Int 2)))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(quot 1 0) ?x]])))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(rem 1 0) ?x]])))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(mod 1 0) ?x]]))))

  (testing "minimum and maximum preserve variadic and empty results"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(max 3 9 4) ?x]])
      (Datascript_runtime.Data_value.Int 9)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(min 3 9 4) ?x]])
      (Datascript_runtime.Data_value.Int 3)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(max 3 4.5) ?x]])
      (Datascript_runtime.Data_value.Float 4.5)))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(max) ?x]])))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(min) ?x]])))))

  (testing "empty ordered comparisons retain upstream CLJS results"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(<) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(>) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(<=) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(>=) ?x]])
      (Datascript_runtime.Data_value.Bool true))))

(deftest test-core-integer-query-predicate
  (testing "integer? accepts every finite integral numeric representation"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? 2) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? 2.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? -0.0) ?x]])
      (Datascript_runtime.Data_value.Bool true))))

  (testing "integer? rejects fractional, non-finite, and non-number values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? 2.5) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? ##NaN) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? ##Inf) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? ##-Inf) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? "2") ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? nil) ?x]])
      (Datascript_runtime.Data_value.Bool false))))

  (testing "integer? retains upstream CLJS unary invocation behavior"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? 1 2) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(integer? 1.5 2) ?x]])
      (Datascript_runtime.Data_value.Bool false)))))

(deftest test-core-sign-and-parity-query-predicates
  (testing "zero? uses upstream numeric identity and ignores extra arguments"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero? 0.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero? -0.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero? 0.5) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero? ##NaN) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero? "0") ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero? nil) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(zero? 0 1) ?x]])
      (Datascript_runtime.Data_value.Bool true))))

  (testing "pos? and neg? preserve JavaScript primitive number coercion"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? 0.5) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? ##Inf) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? ##NaN) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? "2") ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? "0x10") ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? "0b10") ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? "0o10") ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? "1_0") ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? true) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? nil) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? []) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(neg? -0.5) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(neg? ##-Inf) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(neg? "-2") ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(neg?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pos? 1 0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(neg? -1 0) ?x]])
      (Datascript_runtime.Data_value.Bool true))))

  (testing "even? and odd? accept integral numbers and reject other values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(even? 2.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(even? -0.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(odd? 3.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(odd? -3.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (thrown-msg?
      "Argument must be an integer: 2.5"
      (d/q '[:find ?x .
             :where [(even? 2.5) ?x]])))
    (is
     (thrown-msg?
      "Argument must be an integer: NaN"
      (d/q '[:find ?x .
             :where [(odd? ##NaN) ?x]])))
    (is
     (thrown-msg?
      "Argument must be an integer: Infinity"
      (d/q '[:find ?x .
             :where [(even? ##Inf) ?x]])))
    (is
     (thrown-msg?
      "Argument must be an integer: 2"
      (d/q '[:find ?x .
             :where [(odd? "2") ?x]])))
    (is
     (thrown-msg?
      "Argument must be an integer: "
      (d/q '[:find ?x .
             :where [(even? nil) ?x]])))
    (is
     (thrown-msg?
      "Argument must be an integer: "
      (d/q '[:find ?x .
             :where [(odd?) ?x]])))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(even? 2 3) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(odd? 3 2) ?x]])
      (Datascript_runtime.Data_value.Bool true)))))

(deftest test-core-unary-truth-type-and-empty-query-predicates
  (testing "truth predicates treat a missing argument as nil and ignore extras"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(true?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(true? true false) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(false?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(false? false true) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(nil?) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(nil? nil 1) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(some?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(some? 1 nil) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(not) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(not false true) ?x]])
      (Datascript_runtime.Data_value.Bool true))))

  (testing "type predicates treat a missing argument as nil and ignore extras"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(number?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(number? 1 "x") ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(string?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(string? "x" 1) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(boolean?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(boolean? true 1) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(keyword?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(keyword? :kind 1) ?x]])
      (Datascript_runtime.Data_value.Bool true))))

  (testing "empty? treats a missing argument as nil and ignores extras"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(empty?) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(empty? nil) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(empty? [] [1]) ?x]])
      (Datascript_runtime.Data_value.Bool true))))

  (testing "empty? rejects a scalar with the upstream ISeqable error"
    (is
     (=
      "1 is not ISeqable"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(empty? 1) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-core-identity-query-functions-unary-invocation
  (testing "identity aliases treat a missing argument as nil"
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(identity) ?x]])))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(ground) ?x]])))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(untuple) ?x]]))))

  (testing "identity aliases ignore extra arguments"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identity 1 2) ?x]])
      (Datascript_runtime.Data_value.Int 1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(ground 1 2) ?x]])
      (Datascript_runtime.Data_value.Int 1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(untuple [1] [2]) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.Int 1)))))))

(deftest test-core-increment-and-decrement-query-functions
  (testing "missing arguments produce NaN and extra arguments are ignored"
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(inc) ?x]])))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(dec) ?x]])))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(inc 1 9) ?x]])
      (Datascript_runtime.Data_value.Int 2)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(dec 1 9) ?x]])
      (Datascript_runtime.Data_value.Int 0))))

  (testing "nil and booleans retain JavaScript numeric coercion"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(inc nil) ?x]])
      (Datascript_runtime.Data_value.Int 1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(dec nil) ?x]])
      (Datascript_runtime.Data_value.Int -1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(inc true) ?x]])
      (Datascript_runtime.Data_value.Int 2)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(inc false) ?x]])
      (Datascript_runtime.Data_value.Int 1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(dec true) ?x]])
      (Datascript_runtime.Data_value.Int 0)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(dec false) ?x]])
      (Datascript_runtime.Data_value.Int -1))))

  (testing "increment uses JavaScript addition for strings and printed values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(inc "1") ?x]])
      (Datascript_runtime.Data_value.String "11")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(inc :kind) ?x]])
      (Datascript_runtime.Data_value.String ":kind1")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(inc [1 2]) ?x]])
      (Datascript_runtime.Data_value.String "[1 2]1"))))

  (testing "decrement converts numeric strings and rejects other printed values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(dec "1") ?x]])
      (Datascript_runtime.Data_value.Float 0.0)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(dec "1.5") ?x]])
      (Datascript_runtime.Data_value.Float 0.5)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(dec "0x10") ?x]])
      (Datascript_runtime.Data_value.Float 15.0)))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(dec "text") ?x]])))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(dec :kind) ?x]])))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(dec [1 2]) ?x]])))))

(deftest test-core-set-unary-query-function
  (testing "set treats a missing argument as nil and ignores extras"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set) ?x]])
      (Datascript_runtime.Data_value.Set (list))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set [1 1] [2]) ?x]])
      (Datascript_runtime.Data_value.Set
       (list (Datascript_runtime.Data_value.Int 1)))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set "aba") ?x]])
      (Datascript_runtime.Data_value.Set
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set {:a 1}) ?x]])
      (Datascript_runtime.Data_value.Set
       (list
        (Datascript_runtime.Data_value.Vector
         (list
          (Datascript_runtime.Data_value.Keyword ":a")
          (Datascript_runtime.Data_value.Int 1)))))))
    (is
     (=
      "1 is not ISeqable"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(set 1) ?x]])]
          "no error")
        (catch (Invalid_argument message)
               (str message)))))))

(deftest test-core-count-unary-query-function
  (testing "count treats a missing argument as nil and ignores extras"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(count) ?x]])
      (Datascript_runtime.Data_value.Int 0)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(count [1 2] [3]) ?x]])
      (Datascript_runtime.Data_value.Int 2)))
    (is
     (=
      "No protocol method ICounted.-count defined for type number: 1"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(count 1) ?x]])]
          "no error")
        (catch (Invalid_argument message)
               (str message)))))
    (is
     (=
      "No protocol method ICounted.-count defined for type boolean: true"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(count true) ?x]])]
          "no error")
        (catch (Invalid_argument message)
               (str message)))))))

(deftest test-core-not-empty-unary-query-function
  (testing "not-empty treats a missing argument as nil and ignores extras"
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(not-empty) ?x]])))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(not-empty [1] []) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.Int 1)))))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(not-empty "") ?x]])))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(not-empty "a") ?x]])
      (Datascript_runtime.Data_value.String "a")))
    (is
     (=
      "1 is not ISeqable"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(not-empty 1) ?x]])]
          "no error")
        (catch (Invalid_argument message)
               (str message)))))))

(deftest test-core-contains-query-function-invocation
  (testing "contains? defaults missing arguments and ignores extras"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(contains?) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(contains? {:a 1}) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(contains? {:a 1} :a :ignored) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(contains? 1 0) ?x]])
      (Datascript_runtime.Data_value.Bool false)))))

(deftest test-core-named-query-function-invocation
  (testing "name and namespace ignore extra arguments"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(name :person/name :ignored) ?x]])
      (Datascript_runtime.Data_value.String "name")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(namespace :person/name :ignored) ?x]])
      (Datascript_runtime.Data_value.String "person"))))

  (testing "name and namespace preserve upstream unsupported-value errors"
    (is
     (=
      "Doesn't support name: "
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(name) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Doesn't support name: 1"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(name 1) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Doesn't support namespace: "
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(namespace) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Doesn't support namespace: person/name"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(namespace "person/name") ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message))))))

  (testing "keyword reports its exact supported arities"
    (is
     (=
      "Invalid arity: 0"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(keyword) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arity: 3"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(keyword "person" "name" "ignored") ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-core-map-constructor-query-function-invocation
  (testing "map constructors preserve nil keys and last-write-wins values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(hash-map nil 1) ?x]])
      (Datascript_runtime.Data_value.Map
       (list
        (tuple
         (Datascript_runtime.Data_value.Nil)
         (Datascript_runtime.Data_value.Int 1))))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(array-map :a 1 :a 2) ?x]])
      (Datascript_runtime.Data_value.Map
       (list
        (tuple
         (Datascript_runtime.Data_value.Keyword ":a")
         (Datascript_runtime.Data_value.Int 2)))))))

  (testing "map constructors report the final key for every odd arity"
    (is
     (=
      "No value supplied for key: :a"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(hash-map :a) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "No value supplied for key: :b"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(hash-map :a 1 :b) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "No value supplied for key: a"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(hash-map "a") ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "No value supplied for key: :a"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(array-map :a) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "No value supplied for key: :b"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(array-map :a 1 :b) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "No value supplied for key: "
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(array-map nil) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-core-collection-value-query-functions
  (testing "set preserves uniqueness and nil conversion"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set [1 1 2]) ?x]])
      (Datascript_runtime.Data_value.Set
       (list
        (Datascript_runtime.Data_value.Int 1)
        (Datascript_runtime.Data_value.Int 2)))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set nil) ?x]])
      (Datascript_runtime.Data_value.Set (list)))))

  (testing "range preserves one, two, and negative-step arities"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(range 4) ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.Int 0)
        (Datascript_runtime.Data_value.Int 1)
        (Datascript_runtime.Data_value.Int 2)
        (Datascript_runtime.Data_value.Int 3)))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(range 2 5) ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.Int 2)
        (Datascript_runtime.Data_value.Int 3)
        (Datascript_runtime.Data_value.Int 4)))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(range 5 0 -2) ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.Int 5)
        (Datascript_runtime.Data_value.Int 3)
        (Datascript_runtime.Data_value.Int 1))))))

  (testing "str and subs preserve JavaScript boundary behavior"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(str nil :a 1 "x") ?x]])
      (Datascript_runtime.Data_value.String ":a1x")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(str) ?x]])
      (Datascript_runtime.Data_value.String "")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" 2) ?x]])
      (Datascript_runtime.Data_value.String "llo")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" 1 4) ?x]])
      (Datascript_runtime.Data_value.String "ell")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" -2) ?x]])
      (Datascript_runtime.Data_value.String "hello")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" 2 20) ?x]])
      (Datascript_runtime.Data_value.String "llo"))))

  (testing "three-argument get returns present values or its default"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get {:a 1} :a 9) ?x]])
      (Datascript_runtime.Data_value.Int 1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get {:a 1} :b 9) ?x]])
      (Datascript_runtime.Data_value.Int 9)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get [10] 2 9) ?x]])
      (Datascript_runtime.Data_value.Int 9)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get nil :a 9) ?x]])
      (Datascript_runtime.Data_value.Int 9)))))

(deftest test-core-fixed-arity-query-function-errors
  (testing "get reports every unsupported arity"
    (is
     (=
      "Invalid arity: 0"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(get) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arity: 1"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(get {:a 1}) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arity: 4"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(get {:a 1} :a 9 10) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message))))))

  (testing "subs reports every unsupported arity"
    (is
     (=
      "Invalid arity: 0"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(subs) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arity: 1"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(subs "abc") ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "Invalid arity: 4"
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(subs "abc" 0 1 2) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message))))))

  (testing "re-pattern uses its first argument and validates missing values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-pattern "a" "ignored") ?x]])
      (Datascript_runtime.Data_value.Regex "a")))
    (is
     (=
      "re-find must match against a string."
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(re-pattern) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))
    (is
     (=
      "re-find must match against a string."
      (try
        (let [_output
              (d/q '[:find ?x .
                     :where [(re-pattern 1) ?x]])]
          "no error")
        (catch (Invalid_argument message)
          (str message)))))))

(deftest test-core-split-lines-query-function-coercion
  (testing "split-lines stringifies missing, nil, and scalar values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines nil) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines 12) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "12")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines true) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "true")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines :a) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String ":a"))))))

  (testing "split-lines stringifies collection values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines [1 2]) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "[1 2]")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines {:a 1}) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "{:a 1}"))))))

  (testing "split-lines ignores extra arguments"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/split-lines "a\nb" :ignored) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")))))))

(deftest test-core-string-query-functions
  (testing "case conversion, capitalization, and reversal preserve string results"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/lower-case "HeLLo") ?x]])
      (Datascript_runtime.Data_value.String "hello")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/upper-case "HeLLo") ?x]])
      (Datascript_runtime.Data_value.String "HELLO")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/capitalize "hELLO") ?x]])
      (Datascript_runtime.Data_value.String "Hello")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/capitalize "") ?x]])
      (Datascript_runtime.Data_value.String "")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/reverse "abc") ?x]])
      (Datascript_runtime.Data_value.String "cba"))))

  (testing "join preserves both arities and JavaScript coercion"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/join ["a" "b" "c"]) ?x]])
      (Datascript_runtime.Data_value.String "abc")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/join "," ["a" nil 2 :k]) ?x]])
      (Datascript_runtime.Data_value.String "a,,2,:k")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/join nil ["a" "b"]) ?x]])
      (Datascript_runtime.Data_value.String "anullb")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/join "," nil) ?x]])
      (Datascript_runtime.Data_value.String ""))))

  (testing "index lookup preserves start positions and missing results"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/index-of "banana" "na") ?x]])
      (Datascript_runtime.Data_value.Int 2)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/index-of "banana" "na" 3) ?x]])
      (Datascript_runtime.Data_value.Int 4)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/index-of "banana" "" 99) ?x]])
      (Datascript_runtime.Data_value.Int 6)))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(clojure.string/index-of "banana" "zz") ?x]])))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/last-index-of "banana" "na") ?x]])
      (Datascript_runtime.Data_value.Int 4)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/last-index-of "banana" "na" 3) ?x]])
      (Datascript_runtime.Data_value.Int 2)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/last-index-of "banana" "" 99) ?x]])
      (Datascript_runtime.Data_value.Int 6)))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(clojure.string/last-index-of "banana" "na" -1) ?x]]))))

  (testing "line splitting and trimming preserve boundary characters"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines "a\r\nb\n") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines "") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split-lines "a\rb") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "a\rb")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/trim "  hi \n") ?x]])
      (Datascript_runtime.Data_value.String "hi")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/triml "  hi  ") ?x]])
      (Datascript_runtime.Data_value.String "hi  ")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/trimr "  hi  ") ?x]])
      (Datascript_runtime.Data_value.String "  hi")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/trim-newline "hi\r\n\n") ?x]])
      (Datascript_runtime.Data_value.String "hi")))))

(deftest test-core-string-replacement-and-splitting-query-functions
  (testing "replace and replace-first preserve literal and regex behavior"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/replace "foo bar foo" "foo" "x") ?x]])
      (Datascript_runtime.Data_value.String "x bar x")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/replace "a1b22" #"[0-9]+" "#") ?x]])
      (Datascript_runtime.Data_value.String "a#b#")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/replace
               "ab12cd"
               #"([a-z]+)([0-9]+)"
               "$$:$&:$2:$1")
              ?x]])
      (Datascript_runtime.Data_value.String "$:ab12:12:abcd")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/replace-first "foo foo" "foo" "x") ?x]])
      (Datascript_runtime.Data_value.String "x foo")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/replace-first "a1b22" #"[0-9]+" "#") ?x]])
      (Datascript_runtime.Data_value.String "a#b22"))))

  (testing "split preserves regex captures, trailing items, and limits"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "a,b,,c," #",") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")
        (Datascript_runtime.Data_value.String "")
        (Datascript_runtime.Data_value.String "c")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "a,b,c,d" #"," 3) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")
        (Datascript_runtime.Data_value.String "c,d")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "" #",") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "abc" #"") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "")
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")
        (Datascript_runtime.Data_value.String "c")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "a1b2" #"([0-9])") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "1")
        (Datascript_runtime.Data_value.String "b")
        (Datascript_runtime.Data_value.String "2")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "a,b," #"," 0) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "a,b," #"," -1) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "b")
        (Datascript_runtime.Data_value.String "")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(clojure.string/split "a,b,c" #"," 1) ?x]])
      (Datascript_runtime.Data_value.Vector
       (list (Datascript_runtime.Data_value.String "a,b,c")))))))

(deftest test-core-regex-result-query-functions
  (testing "re-find returns strings, capture vectors, and nil"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-find #"b+" "abbbc") ?x]])
      (Datascript_runtime.Data_value.String "bbb")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-find #"(a)(b+)" "abbbc") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "abbb")
        (Datascript_runtime.Data_value.String "a")
        (Datascript_runtime.Data_value.String "bbb")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-find #"(a)?b" "b") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "b")
        (Datascript_runtime.Data_value.Nil)))))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(re-find #"z+" "abbbc") ?x]]))))

  (testing "re-matches requires the complete source"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-matches #"a(b+)" "abbb") ?x]])
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.String "abbb")
        (Datascript_runtime.Data_value.String "bbb")))))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(re-matches #"b+" "abbbc") ?x]]))))

  (testing "re-seq preserves captures, misses, and zero-width progress"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-seq #"[0-9]+" "a1b22") ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.String "1")
        (Datascript_runtime.Data_value.String "22")))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-seq #"([a-z])([0-9]+)" "a1b22") ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.Vector
         (list
          (Datascript_runtime.Data_value.String "a1")
          (Datascript_runtime.Data_value.String "a")
          (Datascript_runtime.Data_value.String "1")))
        (Datascript_runtime.Data_value.Vector
         (list
          (Datascript_runtime.Data_value.String "b22")
          (Datascript_runtime.Data_value.String "b")
          (Datascript_runtime.Data_value.String "22")))))))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(re-seq #"z+" "a1b22") ?x]])))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(re-seq #"" "ab") ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.String "")
        (Datascript_runtime.Data_value.String "")
        (Datascript_runtime.Data_value.String "")))))))

(deftest test-core-printing-query-functions
  (testing "pr-str prints readable values and separates arguments"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pr-str) ?x]])
      (Datascript_runtime.Data_value.String "")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pr-str "a\nb" :k nil [1 "x"]) ?x]])
      (Datascript_runtime.Data_value.String
       "\"a\\nb\" :k nil [1 \"x\"]")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(pr-str ["a\nb" ["x" nil]] 2) ?x]])
      (Datascript_runtime.Data_value.String
       "[\"a\\nb\" [\"x\" nil]] 2"))))

  (testing "print-str prints non-readable nested values"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(print-str) ?x]])
      (Datascript_runtime.Data_value.String "")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(print-str nil) ?x]])
      (Datascript_runtime.Data_value.String "nil")))
    (is
     (match
      (Datascript_runtime.Data_value.print_str
       [(Datascript_runtime.Data_value.Uuid
         "550e8400-e29b-41d4-a716-446655440000")])
      (Some actual)
      (Datascript_runtime.Data_value.equal
       actual
       (Datascript_runtime.Data_value.String
        "#uuid \"550e8400-e29b-41d4-a716-446655440000\""))
      None false))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(print-str #"a") ?x]])
      (Datascript_runtime.Data_value.String "#\"a\"")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(print-str ["a\nb" ["x" nil]] 2) ?x]])
      (Datascript_runtime.Data_value.String
       "[a\nb [x nil]] 2"))))

  (testing "println-str and prn-str always append one newline"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(println-str) ?x]])
      (Datascript_runtime.Data_value.String "\n")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(println-str "a\nb" :k nil [1 "x"]) ?x]])
      (Datascript_runtime.Data_value.String
       "a\nb :k nil [1 x]\n")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(prn-str) ?x]])
      (Datascript_runtime.Data_value.String "\n")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(prn-str "a\nb" :k nil [1 "x"]) ?x]])
      (Datascript_runtime.Data_value.String
       "\"a\\nb\" :k nil [1 \"x\"]\n")))))

(deftest test-core-random-query-functions
  (testing "rand preserves zero, positive, and negative bounds"
    (is
     (scalar-output-float-in-range?
      (d/q '[:find ?x .
             :where [(rand) ?x]])
      0.0 1.0))
    (is
     (scalar-output-float-in-range?
      (d/q '[:find ?x .
             :where [(rand 10) ?x]])
      0.0 10.0))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(rand 0) ?x]])
      (Datascript_runtime.Data_value.Float 0.0)))
    (is
     (scalar-output-float-in-range?
      (d/q '[:find ?x .
             :where [(rand -5) ?x]])
      -5.0 0.0000000000000001))
    (is
     (thrown-msg?
      "Invalid arguments for query function: rand"
      (d/q '[:find ?x .
             :where [(rand 1 2) ?x]]))))

  (testing "rand-int truncates random values toward zero"
    (is
     (scalar-output-int-in-range?
      (d/q '[:find ?x .
             :where [(rand-int 7) ?x]])
      0 7))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(rand-int 0) ?x]])
      (Datascript_runtime.Data_value.Int 0)))
    (is
     (scalar-output-int-in-range?
      (d/q '[:find ?x .
             :where [(rand-int -5) ?x]])
      -4 1))
    (is
     (scalar-output-int-in-range?
      (d/q '[:find ?x .
             :where [(rand-int 3.5) ?x]])
      0 4))
    (is
     (thrown-msg?
      "Invalid arguments for query function: rand-int"
      (d/q '[:find ?x .
             :where [(rand-int) ?x]])))))

(deftest test-core-string-escape-query-function
  (testing "escape replaces mapped characters and preserves unmapped text"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape
               "a<b&c"
               {\< "&lt;" \& "&amp;"})
              ?x]])
      (Datascript_runtime.Data_value.String "a&lt;b&amp;c")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape
               "abc"
               {\a nil \b 7})
              ?x]])
      (Datascript_runtime.Data_value.String "a7c")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape
               "abc"
               {\a false \b :k \c \x})
              ?x]])
      (Datascript_runtime.Data_value.String "false:kx")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape
               "你好a"
               {\a "!"})
              ?x]])
      (Datascript_runtime.Data_value.String "你好!")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape
               ""
               {\a "x"})
              ?x]])
      (Datascript_runtime.Data_value.String ""))))

  (testing "single-character string keys retain upstream CLJS character identity"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape
               "你好abc"
               {"你" "N" "a" "x" "bc" "ignored"})
              ?x]])
      (Datascript_runtime.Data_value.String "N好xbc"))))

  (testing "escape rejects unsupported arities and argument shapes"
    (is
     (thrown-msg?
      "Invalid arguments for query function: clojure.string/escape"
      (d/q '[:find ?x .
             :where [(clojure.string/escape) ?x]])))
    (is
     (thrown-msg?
      "Invalid arguments for query function: clojure.string/escape"
      (d/q '[:find ?x .
             :where [(clojure.string/escape "abc") ?x]])))
    (is
     (thrown-msg?
      "Invalid arguments for query function: clojure.string/escape"
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape "abc" {\a "x"} "extra")
              ?x]])))
    (is
     (thrown-msg?
      "Invalid arguments for query function: clojure.string/escape"
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape 1 {\a "x"})
              ?x]])))
    (is
     (thrown-msg?
      "Invalid arguments for query function: clojure.string/escape"
      (d/q '[:find ?x .
             :where
             [(clojure.string/escape "abc" 1)
              ?x]])))))

(defn ^query-types/input query-data-value-input
  [^:Datascript_runtime.Data_value.t value]
  (query-types/binding-input
   (query-types/scalar-binding
    (query-types/value-result value))))

(defn ^query-types/output query-identical-values
  [^:Datascript_runtime.Data_value.t left
   ^:Datascript_runtime.Data_value.t right]
  (let [query
        (parser/static-query-clauses-with-inputs
         (parser/single-find-element
          (parser/variable-find-element "?result"))
         [(parser/static-function-clause
           "identical?"
           [(parser/variable-argument "?left")
            (parser/variable-argument "?right")]
           (parser/scalar-input "?result"))]
         [(parser/make-static-value-input
           (parser/scalar-input "?left"))
          (parser/make-static-value-input
           (parser/scalar-input "?right"))])]
    (query-v3/q
     query
     (query-data-value-input left)
     (query-data-value-input right))))

(defn ^boolean query-identical-result?
  [^:Datascript_runtime.Data_value.t left
   ^:Datascript_runtime.Data_value.t right
   ^boolean expected]
  (scalar-output-value?
   (query-identical-values left right)
   (Datascript_runtime.Data_value.Bool expected)))

(deftest test-core-identical-query-function
  (testing "primitive values retain upstream JavaScript identity"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? nil nil) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? true true) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? 1 1.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? 0 -0.0) ?x]])
      (Datascript_runtime.Data_value.Bool true)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? ##NaN ##NaN) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (query-identical-result?
      (Datascript_runtime.Data_value.String "same")
      (Datascript_runtime.Data_value.String "same")
      true))
    (is
     (query-identical-result?
      (Datascript_runtime.Data_value.Int 7)
      (Datascript_runtime.Data_value.Wide_int
       (Int64.of_int 7))
      true))
    (is
     (query-identical-result?
      (Datascript_runtime.Data_value.Ref 7)
      (Datascript_runtime.Data_value.Float 7.0)
      true)))

  (testing "object values require the same closed value instance"
    (let [vector-value
          (Datascript_runtime.Data_value.Vector
           (list (Datascript_runtime.Data_value.Int 1)))
          map-value
          (Datascript_runtime.Data_value.Map
           (list
            (tuple
             (Datascript_runtime.Data_value.Keyword ":a")
             (Datascript_runtime.Data_value.Int 1))))
          keyword-value
          (Datascript_runtime.Data_value.Keyword ":a")
          symbol-value
          (Datascript_runtime.Data_value.Symbol "a")
          regex-value
          (Datascript_runtime.Data_value.Regex "a")
          uuid-value
          (Datascript_runtime.Data_value.Uuid
           "00000000-0000-0000-0000-000000000001")]
      (is (query-identical-result? vector-value vector-value true))
      (is
       (query-identical-result?
        vector-value
        (Datascript_runtime.Data_value.Vector
         (list (Datascript_runtime.Data_value.Int 1)))
        false))
      (is (query-identical-result? map-value map-value true))
      (is
       (query-identical-result?
        map-value
        (Datascript_runtime.Data_value.Map
         (list
          (tuple
           (Datascript_runtime.Data_value.Keyword ":a")
           (Datascript_runtime.Data_value.Int 1))))
        false))
      (is (query-identical-result? keyword-value keyword-value true))
      (is
       (query-identical-result?
        keyword-value
        (Datascript_runtime.Data_value.Keyword ":a")
        false))
      (is (query-identical-result? symbol-value symbol-value true))
      (is
       (query-identical-result?
        symbol-value
        (Datascript_runtime.Data_value.Symbol "a")
        false))
      (is (query-identical-result? regex-value regex-value true))
      (is
       (query-identical-result?
        regex-value
        (Datascript_runtime.Data_value.Regex "a")
        false))
      (is (query-identical-result? uuid-value uuid-value true))
      (is
       (query-identical-result?
        uuid-value
        (Datascript_runtime.Data_value.Uuid
         "00000000-0000-0000-0000-000000000001")
        false))))

  (testing "equal object literals are not identical"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? :a :a) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? [1] [1]) ?x]])
      (Datascript_runtime.Data_value.Bool false)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(identical? #"a" #"a") ?x]])
      (Datascript_runtime.Data_value.Bool false))))

  (testing "identical? rejects every unsupported arity"
    (is
     (thrown-msg?
      "Invalid arguments for query function: identical?"
      (d/q '[:find ?x .
             :where [(identical?) ?x]])))
    (is
     (thrown-msg?
      "Invalid arguments for query function: identical?"
      (d/q '[:find ?x .
             :where [(identical? 1) ?x]])))
    (is
     (thrown-msg?
      "Invalid arguments for query function: identical?"
      (d/q '[:find ?x .
             :where [(identical? 1 1 1) ?x]])))))

(deftest test-query-fns
  (testing "predicate without free variables"
    (is (tdc/query-relation?
         (d/q '[:find ?x
                :in [?x ...]
                :where [(> 2 1)]] [:a :b :c])
         [[:a] [:b] [:c]])))

  (let [db (-> (d/empty-db {:parent {:db/valueType :db.type/ref}})
             (d/db-with [{:db/id 1, :name  "Ivan",  :age   15}
                         {:db/id 2, :name  "Petr",  :age   22, :height 240, :parent 1}
                         {:db/id 3, :name  "Slava", :age   37, :parent 2}]))]

    (testing "ground"
      (is (tdc/query-relation?
           (d/q '[:find ?vowel
                  :where [(ground [:a :e :i :o :u]) [?vowel ...]]])
           [[:a] [:e] [:i] [:o] [:u]])))

    (testing "get-else"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?age ?height
                  :where [?e :age ?age]
                  [(get-else $ ?e :height 300) ?height]] db)
           [[1 15 300] [2 22 240] [3 37 300]]))
      
      (is (thrown-msg? "get-else: nil default value is not supported"
            (d/q '[:find ?e ?height
                   :where [?e :age]
                   [(get-else $ ?e :height nil) ?height]] db))))

    (testing "get-some"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?a ?v
                  :where [?e :name _]
                  [(get-some $ ?e :height :age) [?a ?v]]] db)
           [[1 :age 15]
            [2 :height 240]
            [3 :age 37]])))

    (testing "missing?"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?age
                  :in $
                  :where [?e :age ?age]
                  [(missing? $ ?e :height)]] db)
           [[1 15] [3 37]])))

    (testing "missing? back-ref"
      (is (tdc/query-relation?
           (d/q '[:find ?e
                  :in $
                  :where [?e :age ?age]
                  [(missing? $ ?e :_parent)]] db)
           [[3]])))

    (testing "Built-ins"
      (is (tdc/query-relation?
           (d/q '[:find  ?e1 ?e2
                  :where [?e1 :age ?a1]
                  [?e2 :age ?a2]
                  [(< ?a1 18 ?a2)]] db)
           [[1 2] [1 3]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(< ?a1 22)]] db)
           [[15]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(<= ?a1 22)]] db)
           [[15] [22]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(> ?a1 22)]] db)
           [[37]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(>= ?a1 22)]] db)
           [[22] [37]]))
      (testing "compare values of different types"
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(< ?e 1)]] [[0] [1] [""]])
             [[0]]))
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(<= ?e 1)]] [[0] [1] [""]])
             [[0] [1]]))
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(> ?e 1)]] [[0] [1] [""]])
             [[""]]))
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(>= ?e 1)]] [[0] [1] [""]])
             [[1] [""]])))
      
      (is (tdc/query-relation?
           (d/q '[:find  ?x ?c
                  :in    [?x ...]
                  :where [(count ?x) ?c]]
                ["a" "abc"])
           [["a" 1] ["abc" 3]])))

    (testing "Built-in vector, hashmap"
      (is (tdc/query-collection?
           (d/q '[:find [?tx-data ...]
                  :where
                  [(ground :db/add) ?op]
                  [(vector ?op -1 :attr 12) ?tx-data]])
           [[:db/add -1 :attr 12]]))

      (is (tdc/query-collection?
           (d/q '[:find [?tx-data ...]
                  :where
                  [(hash-map :db/id -1 :age 92 :name "Aaron") ?tx-data]])
           [{:db/id -1 :age 92 :name "Aaron"}])))

    (testing "Passing predicate as source"
      (is (tdc/query-relation?
           (d/q '[:find  ?e
                  :in    $ ?adult
                  :where [?e :age ?a]
                  [(?adult ?a)]]
                db adult?)
           [[2] [3]])))

    (testing "Calling a function"
      (is (tdc/query-relation?
           (d/q '[:find  ?e1 ?e2 ?e3
                  :where [?e1 :age ?a1]
                  [?e2 :age ?a2]
                  [?e3 :age ?a3]
                  [(+ ?a1 ?a2) ?a12]
                  [(= ?a12 ?a3)]]
                db)
           [[1 2 3] [2 1 3]])))

    (testing "Two conflicting function values for one binding."
      (is (tdc/query-relation?
           (d/q '[:find  ?n
                  :where [(identity 1) ?n]
                  [(identity 2) ?n]])
           [])))

    (testing "Destructured conflicting function values for two bindings."
      (is (tdc/query-relation?
           (d/q '[:find  ?n ?x
                  :where [(identity [3 4]) [?n ?x]]
                  [(identity [1 2]) [?n ?x]]])
           [])))

    (testing "Rule bindings interacting with function binding. (fn, rule)"
      (is (tdc/query-relation?
           (d/q '[:find  ?n
                  :in $ %
                  :where [(identity 2) ?n]
                  (my-vals ?n)]
                db
                '[[(my-vals ?x)
                   [(identity 1) ?x]]
                  [(my-vals ?x)
                   [(identity 2) ?x]]
                  [(my-vals ?x)
                   [(identity 3) ?x]]])
           [[2]])))

    (testing "Rule bindings interacting with function binding. (rule, fn)"
      (is (tdc/query-relation?
           (d/q '[:find  ?n
                  :in $ %
                  :where (my-vals ?n)
                  [(identity 2) ?n]]
                db
                '[[(my-vals ?x)
                   [(identity 1) ?x]]
                  [(my-vals ?x)
                   [(identity 2) ?x]]
                  [(my-vals ?x)
                   [(identity 3) ?x]]])
           [[2]])))

    (testing "Conflicting relational bindings with function binding. (rel, fn)"
      (is (tdc/query-relation?
           (d/q '[:find  ?age
                  :where [_ :age ?age]
                  [(identity 100) ?age]]
                db)
           [])))

    (testing "Conflicting relational bindings with function binding. (fn, rel)"
      (is (tdc/query-relation?
           (d/q '[:find  ?age
                  :where [(identity 100) ?age]
                  [_ :age ?age]]
                db)
           [])))

    (testing "Function on empty rel"
      (is (tdc/query-relation?
           (d/q '[:find  ?e ?y
                  :where [?e :salary ?x]
                  [(+ ?x 100) ?y]]
                [[0 :age 15] [1 :age 35]])
           [])))
    
    (testing "Returning nil from function filters out tuple from result"
      (is (tdc/query-relation?
           (d/q '[:find ?x
                  :in    [?in ...] ?f
                  :where [(?f ?in) ?x]]
                [1 2 3 4]
                keep-even)
           [[2] [4]])))

    (testing "Result bindings"
      (is (tdc/query-relation?
           (d/q '[:find ?a ?c
                  :in ?in
                  :where [(ground ?in) [?a _ ?c]]]
                [:a :b :c])
           [[:a :c]]))

      (is (tdc/query-relation?
           (d/q '[:find ?in
                  :in ?in
                  :where [(ground ?in) _]]
                :a)
           [[:a]]))

      (is (tdc/query-relation?
           (d/q '[:find ?x ?z
                  :in ?in
                  :where [(ground ?in) [[?x _ ?z]...]]]
                [[:a :b :c] [:d :e :f]])
           [[:a :c] [:d :f]]))
      
      (is (tdc/query-relation?
           (d/q '[:find ?in
                  :in [?in ...]
                  :where [(ground ?in) _]]
                [])
           [])))))

(deftest test-predicates
  (let [db
        (d/db-with
         (d/empty-db)
         [{:db/id 1 :name "Ivan" :age 10}
          {:db/id 2 :name "Ivan" :age 20}
          {:db/id 3 :name "Oleg" :age 10}
          {:db/id 4 :name "Oleg" :age 20}])]
    (is (tdc/query-relation?
         (d/q '[:find  ?e ?a
                :where [?e :age ?a]
                [(> ?a 10)]] db)
         [[2 20] [4 20]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e ?e2
                :where [?e  :name]
                [?e2 :name]
                [(< ?e ?e2)]] db)
         [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e ?e2
                :where [?e  :age ?a]
                [?e2 :age ?a2]
                [(< ?e ?e2)]] db)
         [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e ?e2
                :where [?e  :name "Ivan"]
                [?e2 :name "Oleg"]
                [(= ?e ?e2)]] db)
         []))

    (is (tdc/query-relation?
         (d/q '[:find  ?e
                :where [?e :name "Ivan"]
                [?e :age 20]
                [(= ?e 2)]] db)
         [[2]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e
                :where [?e :name "Ivan"]
                [?e :age 20]
                [(= ?e 1)]] db)
         []))

    (is (tdc/query-relation?
         (d/q '[:find ?e
                :in $ ?pred
                :where [?e :age ?a]
                [(?pred $ ?e 10)]]
              db entity-age?)
         [[1] [3]]))))

(deftest test-exceptions
  (is (thrown-msg? "Unknown predicate 'fun in [(fun ?e)]"
        (d/q '[:find ?e
               :in   [?e ...]
               :where [(fun ?e)]]
          [1])))
  
  (is (thrown-msg? "Unknown function 'fun in [(fun ?e) ?x]"
        (d/q '[:find ?e ?x
               :in   [?e ...]
               :where [(fun ?e) ?x]]
          [1])))

  (is (thrown-msg? "Insufficient bindings: #{?x} not bound in [(zero? ?x)]"
        (d/q '[:find ?x
               :where [(zero? ?x)]])))

  (is (thrown-msg? "Insufficient bindings: #{?x} not bound in [(inc ?x) ?y]"
        (d/q '[:find ?x
               :where [(inc ?x) ?y]])))

  (is (thrown-msg? "Where uses unknown source vars: [$2]"
        (d/q '[:find ?x
               :where [?x] [(zero? $2 ?x)]])))

  (is (thrown-msg? "Where uses unknown source vars: [$]"
        (d/q '[:find  ?x
               :in    $2 
               :where [$2 ?x] [(zero? $ ?x)]]))))

(deftest test-issue-180
  (is (tdc/query-relation?
       (d/q '[:find ?e ?a
              :where [_ :pred ?pred]
              [?e :age ?a]
              [(?pred ?a)]]
            (d/db-with (d/empty-db) [[:db/add 1 :age 20]]))
       [])))

(defn ^:option<Datascript_runtime.Data_value.t> sample-query-fn
  [^:vector<query-types/result> arguments]
  (if (empty? arguments)
    (Some (Datascript_runtime.Data_value.Int 42))
    None))

#?(:clj
   (deftest test-symbol-resolution
     (is
      (scalar-output-value?
       (d/q '[:find ?x .
              :where [(datascript.test.query-fns/sample-query-fn) ?x]])
       (Datascript_runtime.Data_value.Int 42)))))

(deftest test-issue-445
  (let [db (-> (d/empty-db {:name {:db/unique :db.unique/identity}})
             (d/db-with [{:db/id 1 :name "Ivan" :age 15}
                         {:db/id 2 :name "Petr" :age 22 :height 240}]))]
    (testing "get-else using lookup ref"
      (is
       (scalar-output-value?
        (d/q '[:find ?height .
               :in $ ?e
               :where [(get-else $ ?e :height "Unknown") ?height]]
             db
             [:name "Ivan"])
        (Datascript_runtime.Data_value.String "Unknown"))))

    (testing "get-some using lookup ref"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?a ?v
                  :in $ ?e
                  :where [(get-some $ ?e :weight :age :height) [?a ?v]]]
                db
                [:name "Petr"])
           [[[:name "Petr"] :age 22]])))))
