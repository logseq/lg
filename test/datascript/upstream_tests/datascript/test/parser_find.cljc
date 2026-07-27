(ns datascript.test.parser-find
  (:require
    [clojure.test :as t :refer [is deftest]]
    [datascript.parser :as dp]))

(defn ^:Datascript_runtime.Data_value.t symbol-value [^:string value]
  (Datascript_runtime.Data_value.Symbol value))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t sequence-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:Datascript_runtime.Data_value.t call-form
  [^:string name ^:vector<Datascript_runtime.Data_value.t> arguments]
  (sequence-value (vec (cons (symbol-value name) arguments))))

(deftest test-find-protocols
  (let [x-symbol (symbol "?x")
        variable (dp/Variable. x-symbol)
        argument (dp/FnArgVariable variable)
        aggregate
        (dp/Aggregate.
         (dp/AggregatePlain (dp/PlainSymbol. (symbol "count")))
         [argument])
        empty-aggregate
        (dp/Aggregate.
         (dp/AggregatePlain (dp/PlainSymbol. (symbol "count")))
         [])
        pull
        (dp/Pull.
         (dp/SrcVar. (symbol "$"))
         variable
         (dp/PullVariable variable))
        element (dp/FindVariable variable)
        relation (dp/FindRel. [element])
        empty-relation (dp/FindRel. [])
        collection (dp/FindColl. element)
        scalar (dp/FindScalar. element)
        tuple-result (dp/FindTuple. [element])]
    (is (satisfies? dp/IFindVars variable))
    (is (satisfies? dp/IFindVars aggregate))
    (is (satisfies? dp/IFindVars pull))
    (is (= [x-symbol] (dp/-find-vars variable)))
    (is (= [x-symbol] (dp/-find-vars aggregate)))
    (is (= [] (dp/-find-vars empty-aggregate)))
    (is (= [x-symbol] (dp/-find-vars pull)))
    (is (satisfies? dp/IFindElements relation))
    (is (satisfies? dp/IFindElements collection))
    (is (satisfies? dp/IFindElements scalar))
    (is (satisfies? dp/IFindElements tuple-result))
    (is (= [element] (dp/find-elements relation)))
    (is (= [] (dp/find-elements empty-relation)))
    (is (= [element] (dp/find-elements collection)))
    (is (= [element] (dp/find-elements scalar)))
    (is (= [element] (dp/find-elements tuple-result)))))

(deftest test-parse-find
  (is
   (=
    (dp/relation-find-elements
     [(dp/variable-find-element "?a")
      (dp/variable-find-element "?b")])
    (dp/parse-find
     (sequence-value
      [(symbol-value "?a") (symbol-value "?b")]))))
  (is
   (=
    (dp/collection-find-element
     (dp/variable-find-element "?a"))
    (dp/parse-find
     (sequence-value
      [(sequence-value
        [(symbol-value "?a") (symbol-value "...")])]))))
  (is
   (=
    (dp/single-find-element
     (dp/variable-find-element "?a"))
    (dp/parse-find
     (sequence-value
      [(symbol-value "?a") (symbol-value ".")]))))
  (is
   (=
    (dp/tuple-find-elements
     [(dp/variable-find-element "?a")
      (dp/variable-find-element "?b")])
    (dp/parse-find
     (sequence-value
      [(sequence-value
        [(symbol-value "?a") (symbol-value "?b")])])))))

(deftest test-parse-aggregate
  (let [count-b
        (dp/aggregate-find-element
         "count" [(dp/variable-argument "?b")])
        count-a
        (dp/aggregate-find-element
         "count" [(dp/variable-argument "?a")])
        count-b-form
        (call-form "count" [(symbol-value "?b")])
        count-a-form
        (call-form "count" [(symbol-value "?a")])]
    (is
     (=
      (dp/relation-find-elements
       [(dp/variable-find-element "?a") count-b])
      (dp/parse-find
       (sequence-value
        [(symbol-value "?a") count-b-form]))))
    (is
     (=
      (dp/collection-find-element count-a)
      (dp/parse-find
       (sequence-value
        [(sequence-value
          [count-a-form (symbol-value "...")])]))))
    (is
     (=
      (dp/single-find-element count-a)
      (dp/parse-find
       (sequence-value
        [count-a-form (symbol-value ".")]))))
    (is
     (=
      (dp/tuple-find-elements
       [count-a (dp/variable-find-element "?b")])
      (dp/parse-find
       (sequence-value
        [(sequence-value
          [count-a-form (symbol-value "?b")])]))))))

(deftest test-parse-custom-aggregates
  (let [custom-a
        (dp/custom-aggregate-find-element
         "?f" [(dp/variable-argument "?a")])
        custom-b
        (dp/custom-aggregate-find-element
         "?f" [(dp/variable-argument "?b")])
        custom-a-form
        (call-form
         "aggregate"
         [(symbol-value "?f") (symbol-value "?a")])
        custom-b-form
        (call-form
         "aggregate"
         [(symbol-value "?f") (symbol-value "?b")])]
    (is
     (=
      (dp/relation-find-elements [custom-a])
      (dp/parse-find (sequence-value [custom-a-form]))))
    (is
     (=
      (dp/relation-find-elements
       [(dp/variable-find-element "?a") custom-b])
      (dp/parse-find
       (sequence-value
        [(symbol-value "?a") custom-b-form]))))
    (is
     (=
      (dp/collection-find-element custom-a)
      (dp/parse-find
       (sequence-value
        [(sequence-value
          [custom-a-form (symbol-value "...")])]))))
    (is
     (=
      (dp/single-find-element custom-a)
      (dp/parse-find
       (sequence-value
        [custom-a-form (symbol-value ".")]))))
    (is
     (=
      (dp/tuple-find-elements
       [custom-a (dp/variable-find-element "?b")])
      (dp/parse-find
       (sequence-value
        [(sequence-value
          [custom-a-form (symbol-value "?b")])]))))))

(deftest test-parse-find-elements
  (let [aggregate
        (call-form
         "count"
         [(symbol-value "?b")
          (int-value 1)
          (symbol-value "$x")])]
    (is
     (=
      (dp/single-find-element
       (dp/aggregate-find-element
        "count"
        [(dp/variable-argument "?b")
         (dp/constant-argument (int-value 1))
         (dp/source-argument "$x")]))
      (dp/parse-find
       (sequence-value [aggregate (symbol-value ".")]))))))
