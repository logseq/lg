(ns datascript.test.parser-where
  (:require
    [clojure.test :as t :refer [is deftest]]
    [datascript.parser :as dp]))

(defn ^:Datascript_runtime.Data_value.t symbol-value [^:string value]
  (Datascript_runtime.Data_value.Symbol value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t string-value [^:string value]
  (Datascript_runtime.Data_value.String value))

(defn ^:Datascript_runtime.Data_value.t sequence-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:string form-display
  [^:Datascript_runtime.Data_value.t form]
  (Datascript_runtime.Data_value.to_edn_string form))

(defn ^:Datascript_runtime.Data_value.t call-form
  [^:string name ^:vector<Datascript_runtime.Data_value.t> arguments]
  (sequence-value (vec (cons (symbol-value name) arguments))))

(defn ^:Datascript_runtime.Data_value.t source-call-form
  [^:string source
   ^:string name
   ^:vector<Datascript_runtime.Data_value.t> arguments]
  (sequence-value
   (vec
    (cons
     (symbol-value source)
     (cons (symbol-value name) arguments)))))

(deftest pattern
  (let [variables
        (sequence-value
         [(symbol-value "?e")
          (symbol-value "?a")
          (symbol-value "?v")])
        placeholders
        (sequence-value
         [(symbol-value "_")
          (symbol-value "?a")
          (symbol-value "_")
          (symbol-value "_")])
        explicit-placeholders
        (sequence-value
         [(symbol-value "$x")
          (symbol-value "_")
          (symbol-value "?a")
          (symbol-value "_")
          (symbol-value "_")])
        explicit-keyword
        (sequence-value
         [(symbol-value "$x")
          (symbol-value "_")
          (keyword-value :name)
          (symbol-value "?v")])
        explicit-symbol
        (sequence-value
         [(symbol-value "$x")
          (symbol-value "_")
          (symbol-value "sym")
          (symbol-value "?v")])
        explicit-source-symbol
        (sequence-value
         [(symbol-value "$x")
          (symbol-value "_")
          (symbol-value "$src-sym")
          (symbol-value "?v")])]
    (is
     (=
      (dp/pattern-clause
       [(dp/pattern-variable "?e")
        (dp/pattern-variable "?a")
        (dp/pattern-variable "?v")])
      (dp/parse-clause variables)))
    (is
     (=
      (dp/pattern-clause
       [(dp/pattern-placeholder)
        (dp/pattern-variable "?a")
        (dp/pattern-placeholder)
        (dp/pattern-placeholder)])
      (dp/parse-clause placeholders)))
    (is
     (=
      (dp/explicit-pattern-clause
       "$x"
       [(dp/pattern-placeholder)
        (dp/pattern-variable "?a")
        (dp/pattern-placeholder)
        (dp/pattern-placeholder)])
      (dp/parse-clause explicit-placeholders)))
    (is
     (=
      (dp/explicit-pattern-clause
       "$x"
       [(dp/pattern-placeholder)
        (dp/pattern-attribute :name)
        (dp/pattern-variable "?v")])
      (dp/parse-clause explicit-keyword)))
    (is
     (=
      (dp/explicit-pattern-clause
       "$x"
       [(dp/pattern-placeholder)
        (dp/pattern-constant (symbol-value "sym"))
        (dp/pattern-variable "?v")])
      (dp/parse-clause explicit-symbol)))
    (is
     (=
      (dp/explicit-pattern-clause
       "$x"
       [(dp/pattern-placeholder)
        (dp/pattern-constant (symbol-value "$src-sym"))
        (dp/pattern-variable "?v")])
      (dp/parse-clause explicit-source-symbol))))

  (is
   (thrown-msg?
    "Pattern could not be empty"
    (dp/parse-clause (sequence-value [])))))

(deftest test-pred
  (let [static-with-args
        (sequence-value
         [(call-form
           "pred"
           [(symbol-value "?a") (int-value 1)])])
        static-empty
        (sequence-value [(call-form "pred" [])])
        variable
        (sequence-value
         [(call-form "?custom-pred" [(symbol-value "?a")])])]
    (is
     (=
      (dp/static-predicate-clause
       "pred"
       [(dp/variable-argument "?a")
        (dp/constant-argument (int-value 1))])
      (dp/parse-clause static-with-args)))
    (is
     (=
      (dp/static-predicate-clause "pred" [])
      (dp/parse-clause static-empty)))
    (is
     (=
      (dp/variable-predicate-clause
       "?custom-pred"
       [(dp/variable-argument "?a")])
      (dp/parse-clause variable)))))

(deftest test-fn
  (let [static-with-args
        (sequence-value
         [(call-form
           "fn"
           [(symbol-value "?a") (int-value 1)])
          (symbol-value "?x")])
        static-empty
        (sequence-value
         [(call-form "fn" []) (symbol-value "?x")])
        variable-empty
        (sequence-value
         [(call-form "?custom-fn" []) (symbol-value "?x")])
        variable-with-arg
        (sequence-value
         [(call-form "?custom-fn" [(symbol-value "?arg")])
          (symbol-value "?x")])
        binding (dp/scalar-input "?x")]
    (is
     (=
      (dp/static-function-clause
       "fn"
       [(dp/variable-argument "?a")
        (dp/constant-argument (int-value 1))]
       binding)
      (dp/parse-clause static-with-args)))
    (is
     (=
      (dp/static-function-clause "fn" [] binding)
      (dp/parse-clause static-empty)))
    (is
     (=
      (dp/variable-function-clause "?custom-fn" [] binding)
      (dp/parse-clause variable-empty)))
    (is
     (=
      (dp/variable-function-clause
       "?custom-fn"
       [(dp/variable-argument "?arg")]
       binding)
      (dp/parse-clause variable-with-arg)))))

(deftest rule-expr
  (let [variables
        (call-form
         "friends"
         [(symbol-value "?x") (symbol-value "?y")])
        constant-placeholder
        (call-form
         "friends"
         [(string-value "Ivan") (symbol-value "_")])
        explicit
        (source-call-form
         "$1"
         "friends"
         [(symbol-value "?x") (symbol-value "?y")])
        symbol-constant
        (call-form "friends" [(symbol-value "something")])]
    (is
     (=
      (dp/static-rule-clause
       "friends"
       [(dp/pattern-variable "?x")
        (dp/pattern-variable "?y")])
      (dp/parse-clause variables)))
    (is
     (=
      (dp/static-rule-clause
       "friends"
       [(dp/pattern-constant (string-value "Ivan"))
        (dp/pattern-placeholder)])
      (dp/parse-clause constant-placeholder)))
    (is
     (=
      (dp/static-source-rule-clause
       "$1"
       "friends"
       [(dp/pattern-variable "?x")
        (dp/pattern-variable "?y")])
      (dp/parse-clause explicit)))
    (is
     (=
      (dp/static-rule-clause
       "friends"
       [(dp/pattern-constant (symbol-value "something"))])
      (dp/parse-clause symbol-constant))))

  (is
   (thrown-msg?
    "rule-expr requires at least one argument"
    (dp/parse-clause (call-form "friends" [])))))

(deftest not-clause
  (let [follows
        (sequence-value
         [(symbol-value "?e")
          (keyword-value :follows)
          (symbol-value "?x")])
        x-to-y
        (sequence-value
         [(symbol-value "?x")
          (symbol-value "_")
          (symbol-value "?y")])
        follows-clause
        (dp/pattern-clause
         [(dp/pattern-variable "?e")
          (dp/pattern-attribute :follows)
          (dp/pattern-variable "?x")])
        x-to-y-clause
        (dp/pattern-clause
         [(dp/pattern-variable "?x")
          (dp/pattern-placeholder)
          (dp/pattern-variable "?y")])
        single
        (call-form "not" [follows])
        multiple
        (call-form "not" [follows x-to-y])
        source
        (source-call-form
         "$1"
         "not"
         [(sequence-value [(symbol-value "?x")])])
        join
        (call-form
         "not-join"
         [(sequence-value
           [(symbol-value "?e") (symbol-value "?y")])
          follows
          x-to-y])
        source-join
        (source-call-form
         "$1"
         "not-join"
         [(sequence-value [(symbol-value "?e")]) follows])]
    (is
     (=
      (dp/static-not-clause
       [follows-clause]
       (form-display single))
      (dp/parse-clause single)))
    (is
     (=
      (dp/static-not-clause
       [follows-clause x-to-y-clause]
       (form-display multiple))
      (dp/parse-clause multiple)))
    (is
     (=
      (dp/static-source-not-clause
       "$1"
       [(dp/pattern-clause [(dp/pattern-variable "?x")])]
       (form-display source))
      (dp/parse-clause source)))
    (is
     (=
      (dp/static-not-join-clause
       ["?e" "?y"]
       [follows-clause x-to-y-clause]
       (form-display join))
      (dp/parse-clause join)))
    (is
     (=
      (dp/static-source-not-join-clause
       "$1"
       ["?e"]
       [follows-clause]
       (form-display source-join))
      (dp/parse-clause source-join))))

  (is
   (thrown-msg?
    "Join variables should not be empty"
    (dp/parse-clause
     (call-form
      "not-join"
      [(sequence-value [])
       (sequence-value [(symbol-value "?y")])]))))
  (is
   (thrown-msg?
    "Join variables should not be empty"
    (dp/parse-clause
     (call-form
      "not"
      [(sequence-value [(symbol-value "_")])]))))
  (is
   (thrown-msg?
    "Cannot parse 'not-join' clause"
    (dp/parse-clause
     (call-form
      "not-join"
      [(sequence-value [(symbol-value "?x")])]))))
  (is
   (thrown-msg?
    "Cannot parse 'not' clause"
    (dp/parse-clause (call-form "not" [])))))

(deftest or-clause
  (let [follows
        (sequence-value
         [(symbol-value "?e")
          (keyword-value :follows)
          (symbol-value "?x")])
        friend
        (sequence-value
         [(symbol-value "?e")
          (keyword-value :friend)
          (symbol-value "?x")])
        inverse-friend
        (sequence-value
         [(symbol-value "?x")
          (keyword-value :friend)
          (symbol-value "?e")])
        follows-clause
        (dp/pattern-clause
         [(dp/pattern-variable "?e")
          (dp/pattern-attribute :follows)
          (dp/pattern-variable "?x")])
        friend-clause
        (dp/pattern-clause
         [(dp/pattern-variable "?e")
          (dp/pattern-attribute :friend)
          (dp/pattern-variable "?x")])
        inverse-friend-clause
        (dp/pattern-clause
         [(dp/pattern-variable "?x")
          (dp/pattern-attribute :friend)
          (dp/pattern-variable "?e")])
        single (call-form "or" [follows])
        multiple (call-form "or" [follows friend])
        nested
        (call-form
         "or"
         [follows
          (call-form "and" [friend inverse-friend])])
        source
        (source-call-form
         "$1"
         "or"
         [(sequence-value [(symbol-value "?x")])])
        join
        (call-form
         "or-join"
         [(sequence-value [(symbol-value "?e")])
          follows
          (sequence-value
           [(symbol-value "?e")
            (keyword-value :friend)
            (symbol-value "?y")])])
        required-join
        (call-form
         "or-join"
         [(sequence-value
           [(sequence-value [(symbol-value "?e")])])
          (call-form
           "and"
           [follows
            (sequence-value
             [(symbol-value "?e")
              (keyword-value :friend)
              (symbol-value "?y")])])])
        source-join
        (source-call-form
         "$1"
         "or-join"
         [(sequence-value
           [(sequence-value [(symbol-value "?e")])
            (symbol-value "?x")])
          follows])]
    (is
     (=
      (dp/static-or-clause
       [follows-clause]
       (form-display single))
      (dp/parse-clause single)))
    (is
     (=
      (dp/static-or-clause
       [follows-clause friend-clause]
       (form-display multiple))
      (dp/parse-clause multiple)))
    (is
     (=
      (dp/static-or-clause
       [follows-clause
        (dp/static-and-clause
         [friend-clause inverse-friend-clause])]
       (form-display nested))
      (dp/parse-clause nested)))
    (is
     (=
      (dp/static-source-or-clause
       "$1"
       [(dp/pattern-clause [(dp/pattern-variable "?x")])]
       (form-display source))
      (dp/parse-clause source)))
    (is
     (=
      (dp/static-or-join-clause
       []
       ["?e"]
       [follows-clause
        (dp/pattern-clause
         [(dp/pattern-variable "?e")
          (dp/pattern-attribute :friend)
          (dp/pattern-variable "?y")])]
       (form-display join))
      (dp/parse-clause join)))
    (is
     (=
      (dp/static-or-join-clause
       ["?e"]
       []
       [(dp/static-and-clause
         [follows-clause
          (dp/pattern-clause
           [(dp/pattern-variable "?e")
            (dp/pattern-attribute :friend)
            (dp/pattern-variable "?y")])])]
       (form-display required-join))
      (dp/parse-clause required-join)))
    (is
     (=
      (dp/static-source-or-join-clause
       "$1"
       ["?e"]
       ["?x"]
       [follows-clause]
       (form-display source-join))
      (dp/parse-clause source-join))))

  (is
   (thrown-msg?
    "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
    (dp/parse-clause
     (call-form
      "or-join"
      [(sequence-value [])
       (sequence-value [(symbol-value "?y")])]))))
  (is
   (thrown-msg?
    "Join variables should not be empty"
    (dp/parse-clause
     (call-form
      "or"
      [(sequence-value [(symbol-value "_")])]))))
  (is
   (thrown-msg?
    "Cannot parse 'or-join' clause"
    (dp/parse-clause
     (call-form
      "or-join"
      [(sequence-value [(symbol-value "?x")])]))))
  (is
   (thrown-msg?
    "Cannot parse 'or' clause"
    (dp/parse-clause (call-form "or" [])))))
