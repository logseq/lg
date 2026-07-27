(ns datascript.test.parser-query
  (:require
    [clojure.test :refer [deftest is]]
    [datascript.parser :as dp]))

(defn ^:Datascript_runtime.Data_value.t symbol-value [^:string value]
  (Datascript_runtime.Data_value.Symbol value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t sequence-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:Datascript_runtime.Data_value.t pattern
  [^:vector<string> values]
  (sequence-value (mapv symbol-value values)))

(defn ^:Datascript_runtime.Data_value.t query-form
  [^:vector<Datascript_runtime.Data_value.t> values]
  (sequence-value values))

(defn assert-invalid-query
  [^:Datascript_runtime.Data_value.t form ^:string message]
  (is (thrown-msg? message (dp/parse-query form))))

(deftest validation
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?e")
     (keyword-value :where) (pattern ["?x"])])
   "Query for unknown vars: [?e]")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?e")
     (keyword-value :with) (symbol-value "?f")
     (keyword-value :where) (pattern ["?e"])])
   "Query for unknown vars: [?f]")
  (assert-invalid-query
   (query-form
    [(keyword-value :find)
     (symbol-value "?e")
     (symbol-value "?x")
     (symbol-value "?t")
     (keyword-value :in)
     (symbol-value "?x")
     (keyword-value :where)
     (pattern ["?e"])])
   "Query for unknown vars: [?t]")
  (assert-invalid-query
   (query-form
    [(keyword-value :find)
     (symbol-value "?x")
     (symbol-value "?e")
     (keyword-value :with)
     (symbol-value "?y")
     (symbol-value "?e")
     (keyword-value :where)
     (pattern ["?x" "?e" "?y"])])
   ":find and :with should not use same variables: [?e]")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?e")
     (keyword-value :in)
     (symbol-value "$")
     (symbol-value "$")
     (symbol-value "?x")
     (keyword-value :where)
     (pattern ["?e"])])
   "Vars used in :in should be distinct")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?e")
     (keyword-value :in)
     (symbol-value "?x")
     (symbol-value "$")
     (symbol-value "?x")
     (keyword-value :where)
     (pattern ["?e"])])
   "Vars used in :in should be distinct")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?e")
     (keyword-value :in)
     (symbol-value "$")
     (symbol-value "%")
     (symbol-value "?x")
     (symbol-value "%")
     (keyword-value :where)
     (pattern ["?e"])])
   "Vars used in :in should be distinct")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?n")
     (keyword-value :with)
     (symbol-value "?e")
     (symbol-value "?f")
     (symbol-value "?e")
     (keyword-value :where)
     (pattern ["?e" "?f" "?n"])])
   "Vars used in :with should be distinct")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?x")
     (keyword-value :where)
     (pattern ["$1" "?x"])])
   "Where uses unknown source vars: [$1]")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?x")
     (keyword-value :in) (symbol-value "$1")
     (keyword-value :where)
     (pattern ["$2" "?x"])])
   "Where uses unknown source vars: [$2]")
  (assert-invalid-query
   (query-form
    [(keyword-value :find) (symbol-value "?e")
     (keyword-value :where)
     (sequence-value
      [(symbol-value "rule") (symbol-value "?e")])])
   "Missing rules var '%' in :in"))
