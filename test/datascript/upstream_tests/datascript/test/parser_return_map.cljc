(ns datascript.test.parser-return-map
  (:require
    [clojure.test :refer [deftest is testing]]
    [datascript.parser :as dp]))

(defn ^:Datascript_runtime.Data_value.t symbol-value [^:string value]
  (Datascript_runtime.Data_value.Symbol value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t sequence-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:Datascript_runtime.Data_value.t pattern
  [^:vector<string> variables]
  (sequence-value (mapv symbol-value variables)))

(defn ^:Datascript_runtime.Data_value.t return-map-query
  [^:vector<Datascript_runtime.Data_value.t> find
   ^:keyword return-type
   ^:vector<string> names
   ^:vector<string> where-variables]
  (sequence-value
   (vec
    (concat
     [(keyword-value :find)]
     find
     [(keyword-value return-type)]
     (mapv symbol-value names)
     [(keyword-value :where) (pattern where-variables)]))))

(defn assert-return-map
  [^:keyword expected-type
   ^:vector<string> expected-names
   ^:Datascript_runtime.Data_value.t form]
  (if-some [return-map (.-qreturn-map (dp/parse-query form))]
    (do
      (is (= expected-type (dp/return-map-type return-map)))
      (case expected-type
        :keys
        (is
         (=
          (Some (mapv (fn [^:string name] (str ":" name))
                      expected-names))
          (dp/return-map-key-names return-map)))
        :syms
        (is
         (=
          (Some expected-names)
          (dp/return-map-symbol-names return-map)))
        :strs
        (is
         (=
          (Some expected-names)
          (dp/return-map-string-names return-map)))
        (is false "Unexpected return-map type")))
    (is false "Expected a return-map")))

(deftest test-parse-return-map
  (assert-return-map
   :keys
   ["x" "y"]
   (return-map-query
    [(symbol-value "?a") (symbol-value "?b")]
    :keys
    ["x" "y"]
    ["?a" "?b"]))

  (assert-return-map
   :syms
   ["x"]
   (return-map-query
    [(symbol-value "?a")]
    :syms
    ["x"]
    ["?a"]))

  (assert-return-map
   :strs
   ["x" "y" "z"]
   (return-map-query
    [(symbol-value "?a")
     (symbol-value "?b")
     (symbol-value "?c")]
    :strs
    ["x" "y" "z"]
    ["?a" "?b" "?c"]))

  (testing "with find specs"
    (assert-return-map
     :keys
     ["x" "y"]
     (return-map-query
      [(sequence-value
        [(symbol-value "?a") (symbol-value "?b")])]
      :keys
      ["x" "y"]
      ["?a" "?b"]))

    (is
     (thrown-msg?
      ":keys does not work with collection :find"
      (dp/parse-query
       (return-map-query
        [(sequence-value
          [(symbol-value "?a") (symbol-value "...")])]
        :keys
        ["x"]
        ["?a"]))))

    (is
     (thrown-msg?
      ":keys does not work with single-scalar :find"
      (dp/parse-query
       (return-map-query
        [(symbol-value "?a") (symbol-value ".")]
        :keys
        ["x" "y"]
        ["?a"])))))

  (testing "errors"
    (is
     (thrown-msg?
      "Only one of :keys/:syms/:strs must be present"
      (dp/parse-query
       (sequence-value
        [(keyword-value :find)
         (symbol-value "?a")
         (symbol-value "?b")
         (keyword-value :keys)
         (symbol-value "x")
         (symbol-value "y")
         (keyword-value :strs)
         (symbol-value "zt")
         (keyword-value :where)
         (pattern ["?a" "?b"])]))))

    (is
     (thrown-msg?
      "Count of :keys must match count of :find"
      (dp/parse-query
       (return-map-query
        [(symbol-value "?a") (symbol-value "?b")]
        :keys
        ["x" "y" "z"]
        ["?a" "?b"]))))

    (is
     (thrown-msg?
      "Count of :syms must match count of :find"
      (dp/parse-query
       (return-map-query
        [(symbol-value "?a") (symbol-value "?b")]
        :syms
        ["x"]
        ["?a" "?b"]))))

    (is
     (thrown-msg?
      "Count of :strs must match count of :find"
      (dp/parse-query
       (return-map-query
        [(symbol-value "?a") (symbol-value "?b")]
        :strs
        ["x"]
        ["?a" "?b"]))))

    (is
     (thrown-msg?
      "Count of :keys must match count of :find"
      (dp/parse-query
       (return-map-query
        [(sequence-value
          [(symbol-value "?a") (symbol-value "?b")])]
        :keys
        ["x"]
        ["?a" "?b"]))))))
