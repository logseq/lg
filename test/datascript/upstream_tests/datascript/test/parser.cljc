(ns datascript.test.parser
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.parser :as dp]))

(defn ^:Datascript_runtime.Data_value.t symbol-value [^:string value]
  (Datascript_runtime.Data_value.Symbol value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:string value]
  (Datascript_runtime.Data_value.Keyword value))

(defn ^:Datascript_runtime.Data_value.t vector-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^datascript.parser/binding scalar [^:string variable]
  (dp/scalar-input variable))

(defn ^datascript.parser/binding ignore []
  (dp/ignore-input))

(defn ^datascript.parser/binding tuple-binding
  [^:vector<datascript.parser/binding> bindings]
  (dp/tuple-input bindings))

(defn ^datascript.parser/binding collection-binding
  [^datascript.parser/binding binding]
  (dp/collection-input binding))

(defn ^datascript.parser/input-binding source-input [^:string source]
  (dp/static-input-binding-form
   (dp/make-static-source-input source)))

(defn ^datascript.parser/input-binding rules-input []
  (dp/static-input-binding-form
   (dp/make-static-rules-input)))

(defn ^datascript.parser/input-binding value-input
  [^datascript.parser/binding binding]
  (dp/static-input-binding-form
   (dp/make-static-value-input binding)))

(deftest public-parser-utilities
  (let [one (Datascript_runtime.Data_value.Int 1)
        two (Datascript_runtime.Data_value.Int 2)]
    (is (dp/of-size? (vector-value [one two]) 2))
    (is
     (dp/of-size?
      (Datascript_runtime.Data_value.List (list one two))
      2))
    (is (not (dp/of-size? (vector-value [one]) 2)))
    (is (not (dp/of-size? one 1))))

  (let [default-pattern
        (dp/pattern-clause [(dp/pattern-variable "?e")])
        explicit-pattern
        (dp/explicit-pattern-clause
         "$history"
         [(dp/pattern-variable "?e")])
        predicate
        (dp/greater-than-clause
         (dp/variable-argument "?e")
         (dp/constant-argument
          (Datascript_runtime.Data_value.Int 0)))]
    (match (dp/explicit-input default-pattern)
      (Some source)
      (is (= None (dp/query-source-name source)))
      None (is false))
    (match (dp/explicit-input explicit-pattern)
      (Some source)
      (is (= (Some "$history") (dp/query-source-name source)))
      None (is false))
    (is (= None (dp/explicit-input predicate))))

  (let [valid
        (dp/static-or-clause
         [(dp/pattern-clause [(dp/pattern-variable "?e")])]
         "(or [?e :name _])")
        invalid
        (dp/static-or-clause
         [(dp/pattern-clause
           [(dp/pattern-constant
             (Datascript_runtime.Data_value.Int 1))])]
         "(or [1 :name _])")
        source
        (vector-value
         [(symbol-value "or")
          (vector-value
           [(Datascript_runtime.Data_value.Int 1)
            (keyword-value ":name")
            (symbol-value "_")])])]
    (is (= valid (dp/validate-or valid source)))
    (is
     (thrown-msg?
      "Join variables should not be empty"
      (dp/validate-or invalid source)))))

(deftest test-public-parse-seq
  (let [x (symbol-value "?x")
        y (symbol-value "?y")
        source (symbol-value "$source")
        variable-names
        (fn [^:vector<datascript.parser/Variable> variables]
          (mapv
           (fn [^datascript.parser/Variable variable]
             (str (.-symbol variable)))
           variables))]
    (match
     (dp/parse-seq dp/parse-variable (vector-value [x y]))
     None (is false)
     (Some variables)
     (is (= ["?x" "?y"] (variable-names variables))))
    (match
     (dp/parse-seq
      dp/parse-variable
      (Datascript_runtime.Data_value.List (list x y)))
     None (is false)
     (Some variables)
     (is (= ["?x" "?y"] (variable-names variables))))
    (match
     (dp/parse-seq dp/parse-variable (vector-value []))
     None (is false)
     (Some variables) (is (empty? variables)))
    (is
     (=
      None
      (dp/parse-seq
       dp/parse-variable
       (Datascript_runtime.Data_value.Int 1))))
    (match
     (dp/parse-seq dp/parse-src-var (vector-value [source]))
     None (is false)
     (Some sources)
     (if-some [parsed-source (first sources)]
       (is (= "$source" (str (.-symbol parsed-source))))
       (is false))))

  (let [calls (atom 0)
        x (symbol-value "?x")
        invalid (Datascript_runtime.Data_value.Int 1)
        y (symbol-value "?y")
        parse-variable
        (fn [^:Datascript_runtime.Data_value.t form]
          (swap! calls inc)
          (dp/parse-variable form))]
    (is
     (=
      None
      (dp/parse-seq
       parse-variable
       (vector-value [x invalid y]))))
    (is (= 2 @calls))))

(deftest test-public-distinct
  (let [x (dp/parse-var-required (symbol-value "?x"))
        y (dp/parse-var-required (symbol-value "?y"))]
    (is (dp/distinct? []))
    (is (dp/distinct? [1]))
    (is (dp/distinct? [1 2 3]))
    (is (not (dp/distinct? [1 2 1])))
    (is (dp/distinct? (list "x" "y")))
    (is (not (dp/distinct? (list "x" "y" "x"))))
    (is (dp/distinct? [x y]))
    (is (not (dp/distinct? [x y x])))))

(defn ^:vector<string> traversable-variable-names
  [^:vector<datascript.parser/traversable> nodes]
  (reduce
   (fn [^:vector<string> names
        ^datascript.parser/traversable node]
     (if-some [name (dp/traversable-variable-name node)]
       (conj names name)
       names))
   []
   nodes))

(deftest test-public-itraversable-collect
  (let [pattern
        (dp/pattern-clause
         [(dp/pattern-variable "?e")
          (dp/pattern-attribute :name)
          (dp/pattern-variable "?name")])
        predicate
        (dp/greater-than-clause
         (dp/variable-argument "?age")
         (dp/constant-argument
          (Datascript_runtime.Data_value.Int 18)))
        root
        (dp/clause-traversable
         (dp/static-and-clause [pattern predicate]))
        variable?
        (fn [^datascript.parser/traversable node]
          (some? (dp/traversable-variable-name node)))]
    (is (satisfies? dp/ITraversable root))
    (is
     (=
      ["?e" "?name" "?age"]
      (traversable-variable-names
       (dp/collect variable? root))))
    (is
     (=
      ["?seed" "?e" "?name" "?age"]
      (traversable-variable-names
       (dp/collect
        variable?
        root
        [(dp/variable-traversable "?seed")]))))
    (is
     (=
      1
      (count
       (dp/collect
        (fn [^datascript.parser/traversable node]
          (dp/traversable-clause? node))
        root))))
    (is
     (empty?
      (dp/collect
       (fn [^datascript.parser/traversable node]
         (dp/traversable-clause? node))
       (dp/variable-traversable "?leaf"))))))

(deftest test-public-itraversable-collect-vars
  (let [hidden
        (dp/pattern-clause
         [(dp/pattern-variable "?hidden")
          (dp/pattern-attribute :name)
          (dp/pattern-placeholder)])
        visible
        (dp/pattern-clause
         [(dp/pattern-variable "?shown")
          (dp/pattern-attribute :age)
          (dp/pattern-placeholder)])
        root
        (dp/clause-traversable
         (dp/static-and-clause
          [(dp/static-not-clause [hidden] "(not [?hidden :name _])")
           visible]))]
    (is
     (=
      ["?hidden" "?shown"]
      (variable-names (dp/-collect-vars root []))))))

(deftest test-public-itraversable-postwalk
  (let [root
        (dp/clause-traversable
         (dp/pattern-clause
          [(dp/pattern-variable "?e")
           (dp/pattern-attribute :name)
           (dp/pattern-variable "?name")]))
        visits (atom [])
        transformed
        (dp/postwalk
         root
         (fn [^datascript.parser/traversable node]
           (swap! visits conj (dp/traversable-kind node))
           (if-some [name (dp/traversable-variable-name node)]
             (dp/variable-traversable (str name "!"))
             node)))]
    (is (= (Some "clause") (last @visits)))
    (is
     (=
      ["?e!" "?name!"]
      (traversable-variable-names
       (dp/collect
        (fn [^datascript.parser/traversable node]
          (some? (dp/traversable-variable-name node)))
        transformed))))
    (is (some? (dp/traversable-clause-value transformed)))
    (is
     (thrown-msg?
      "Expected pattern-element traversal node"
      (dp/postwalk
       root
       (fn [^datascript.parser/traversable node]
         (if-some [_name (dp/traversable-variable-name node)]
           (dp/string-traversable "invalid")
           node)))))))

(deftest test-public-itraversable-closed-node-domain
  (let [pattern
        (dp/pattern-clause
         [(dp/pattern-variable "?e")
          (dp/pattern-attribute :name)
          (dp/pattern-placeholder)])
        find (dp/relation-find ["?e"])
        query (dp/static-db-query-clauses find [pattern])
        branch (dp/static-rule-branch "named" ["?e"] [pattern])
        rule
        (if-some [parsed (first (dp/static-rules [branch]))]
          parsed
          (Stdlib.invalid_arg "Expected a static rule"))
        nodes
        [(dp/variable-traversable "?e")
         (dp/binding-traversable (dp/scalar-input "?input"))
         (dp/find-traversable find)
         (dp/clause-traversable pattern)
         (dp/rule-traversable rule)
         (dp/query-traversable query)
         (dp/data-traversable
          (Datascript_runtime.Data_value.Int 1))
         (dp/string-traversable "text")]]
    (is
     (every?
      (fn [^datascript.parser/traversable node]
        (satisfies? dp/ITraversable node))
      nodes))
    (is
     (=
      ["?e" "?e"]
      (traversable-variable-names
       (dp/collect
        (fn [^datascript.parser/traversable node]
          (some? (dp/traversable-variable-name node)))
        (dp/query-traversable query)))))))

(deftest test-public-itraversable-collect-data-collections
  (let [one (Datascript_runtime.Data_value.Int 1)
        two (Datascript_runtime.Data_value.Int 2)
        three (Datascript_runtime.Data_value.Int 3)
        four (Datascript_runtime.Data_value.Int 4)
        five (Datascript_runtime.Data_value.Int 5)
        nil-value (Datascript_runtime.Data_value.Nil)
        map-key (Datascript_runtime.Data_value.Keyword ":key")
        nested-list
        (Datascript_runtime.Data_value.List (list one))
        root
        (dp/data-traversable
         (Datascript_runtime.Data_value.Vector
          (list
           nested-list
           (Datascript_runtime.Data_value.Set (list two))
           (Datascript_runtime.Data_value.Map
            (list
             (tuple map-key three)
             (tuple
              (Datascript_runtime.Data_value.Keyword ":nested")
              (Datascript_runtime.Data_value.Vector (list four)))))
           (Datascript_runtime.Data_value.Tuple
            (list None (Some five))))))
        scalar?
        (fn [^datascript.parser/traversable node]
          (or
           (= node (dp/data-traversable one))
           (= node (dp/data-traversable two))
           (= node (dp/data-traversable three))
           (= node (dp/data-traversable four))
           (= node (dp/data-traversable five))
           (= node (dp/data-traversable nil-value))))]
    (is
     (=
      [(dp/data-traversable one)
       (dp/data-traversable two)
       (dp/data-traversable three)
       (dp/data-traversable four)
       (dp/data-traversable nil-value)
       (dp/data-traversable five)]
      (dp/collect scalar? root)))
    (is
     (=
      [(dp/data-traversable map-key)]
      (dp/collect
       (fn [^datascript.parser/traversable node]
         (= node (dp/data-traversable map-key)))
       root)))
    (is
     (=
      [(dp/data-traversable nested-list)]
      (dp/collect
       (fn [^datascript.parser/traversable node]
         (or
          (= node (dp/data-traversable nested-list))
          (= node (dp/data-traversable one))))
       root)))))

(deftest test-public-itraversable-postwalk-data-collections
  (let [one (Datascript_runtime.Data_value.Int 1)
        ten (Datascript_runtime.Data_value.Int 10)
        nil-value (Datascript_runtime.Data_value.Nil)
        nine (Datascript_runtime.Data_value.Int 9)
        key (Datascript_runtime.Data_value.Keyword ":key")
        value (Datascript_runtime.Data_value.Keyword ":value")
        changed-key (Datascript_runtime.Data_value.Keyword ":changed-key")
        changed-value
        (Datascript_runtime.Data_value.Keyword ":changed-value")
        root
        (dp/data-traversable
         (Datascript_runtime.Data_value.Vector
          (list
           (Datascript_runtime.Data_value.List (list one))
           (Datascript_runtime.Data_value.Set (list one))
           (Datascript_runtime.Data_value.Map
            (list (tuple key value)))
           (Datascript_runtime.Data_value.Tuple
            (list None (Some one))))))
        expected
        (dp/data-traversable
         (Datascript_runtime.Data_value.Vector
          (list
           (Datascript_runtime.Data_value.List (list ten))
           (Datascript_runtime.Data_value.Set (list ten))
           (Datascript_runtime.Data_value.Map
            (list (tuple key changed-value)))
           (Datascript_runtime.Data_value.Tuple
            (list (Some nine) (Some ten))))))
        transformed
        (dp/postwalk
         root
         (fn [^datascript.parser/traversable node]
           (if (= node (dp/data-traversable one))
             (dp/data-traversable ten)
             (if (= node (dp/data-traversable nil-value))
               (dp/data-traversable nine)
               (if (= node (dp/data-traversable key))
                 (dp/data-traversable changed-key)
                 (if (= node (dp/data-traversable value))
                   (dp/data-traversable changed-value)
                   node))))))]
    (is (= expected transformed))))

(deftest test-public-itraversable-postwalk-data-order
  (let [leaf (Datascript_runtime.Data_value.Int 1)
        inner (Datascript_runtime.Data_value.List (list leaf))
        outer (Datascript_runtime.Data_value.Vector (list inner))
        visits (atom [])]
    (dp/postwalk
     (dp/data-traversable outer)
     (fn [^datascript.parser/traversable node]
       (cond
         (= node (dp/data-traversable leaf))
         (swap! visits conj "leaf")

         (= node (dp/data-traversable inner))
         (swap! visits conj "list")

         (= node (dp/data-traversable outer))
         (swap! visits conj "vector"))
       node))
    (is (= ["leaf" "list" "vector"] @visits))))

(deftest bindings
  (is (= (scalar "?x")
         (dp/parse-binding (symbol-value "?x"))))
  (is (= (ignore)
         (dp/parse-binding (symbol-value "_"))))
  (is
   (=
    (collection-binding (scalar "?x"))
    (dp/parse-binding
     (vector-value [(symbol-value "?x") (symbol-value "...")]))))
  (is
   (=
    (tuple-binding [(scalar "?x")])
    (dp/parse-binding
     (vector-value [(symbol-value "?x")]))))
  (is
   (=
    (tuple-binding [(scalar "?x") (scalar "?y")])
    (dp/parse-binding
     (vector-value [(symbol-value "?x") (symbol-value "?y")]))))
  (is
   (=
    (tuple-binding [(ignore) (scalar "?y")])
    (dp/parse-binding
     (vector-value [(symbol-value "_") (symbol-value "?y")]))))
  (is
   (=
    (collection-binding
     (tuple-binding
      [(ignore)
       (collection-binding (scalar "?x"))]))
    (dp/parse-binding
     (vector-value
      [(vector-value
        [(symbol-value "_")
         (vector-value
          [(symbol-value "?x") (symbol-value "...")])])
       (symbol-value "...")]))))
  (is
   (=
    (collection-binding
     (tuple-binding
      [(scalar "?a") (scalar "?b") (scalar "?c")]))
    (dp/parse-binding
     (vector-value
      [(vector-value
        [(symbol-value "?a")
         (symbol-value "?b")
         (symbol-value "?c")])]))))
  (is
   (thrown-msg?
    "Cannot parse binding, expected (bind-scalar | bind-tuple | bind-coll | bind-rel)"
    (dp/parse-binding (keyword-value ":key")))))

(deftest in
  (is
   (=
    [(value-input (scalar "?x"))]
    (dp/parse-in
     (vector-value [(symbol-value "?x")]))))
  (is
   (=
    [(source-input "$")
     (source-input "$1")
     (rules-input)
     (value-input (ignore))
     (value-input (scalar "?x"))]
    (dp/parse-in
     (vector-value
      [(symbol-value "$")
       (symbol-value "$1")
       (symbol-value "%")
       (symbol-value "_")
       (symbol-value "?x")]))))
  (is
   (=
    [(source-input "$")
     (value-input
      (collection-binding
       (tuple-binding
        [(ignore)
         (collection-binding (scalar "?x"))])))]
    (dp/parse-in
     (vector-value
      [(symbol-value "$")
       (vector-value
        [(vector-value
          [(symbol-value "_")
           (vector-value
            [(symbol-value "?x") (symbol-value "...")])])
         (symbol-value "...")])]))))
  (is
   (thrown-msg?
    "Cannot parse binding, expected (bind-scalar | bind-tuple | bind-coll | bind-rel)"
    (dp/parse-in
     (vector-value
      [(symbol-value "?x") (keyword-value ":key")])))))

(defn ^:vector<string> variable-names
  [^:vector<datascript.parser/Variable> variables]
  (mapv
   (fn [^datascript.parser/Variable variable]
     (str (.-symbol variable)))
   variables))

(deftest with
  (is
   (=
    ["?x" "?y"]
    (variable-names
     (dp/parse-with
      (vector-value
       [(symbol-value "?x") (symbol-value "?y")])))))
  (is
   (thrown-msg?
    "Cannot parse :with clause, expected [ variable+ ]"
    (dp/parse-with
     (vector-value
      [(symbol-value "?x") (symbol-value "_")])))))
