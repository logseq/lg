(ns datascript.test.parser-rules
  (:require
    [clojure.test :as t :refer [is deftest]]
    [datascript.parser :as dp]))

(defn ^:Datascript_runtime.Data_value.t symbol-value [^:string value]
  (Datascript_runtime.Data_value.Symbol value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t sequence-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:Datascript_runtime.Data_value.t rule-head
  [^:string name ^:vector<Datascript_runtime.Data_value.t> variables]
  (sequence-value
   (vec (cons (symbol-value name) variables))))

(defn ^:Datascript_runtime.Data_value.t rule-branch
  [^:Datascript_runtime.Data_value.t head
   ^:vector<Datascript_runtime.Data_value.t> clauses]
  (sequence-value (vec (cons head clauses))))

(defn ^:Datascript_runtime.Data_value.t rules-form
  [^:vector<Datascript_runtime.Data_value.t> branches]
  (sequence-value branches))

(defn ^:Datascript_runtime.Data_value.t pattern-form
  [^:vector<Datascript_runtime.Data_value.t> elements]
  (sequence-value elements))

(defn expected-pattern
  [^:vector<datascript.parser/pattern-element> elements]
  (dp/pattern-clause elements))

(defn ^datascript.parser/Rule only-rule
  [^:vector<datascript.parser/Rule> rules]
  (if-some [rule (first rules)]
    rule
    (Stdlib.invalid_arg "Expected one parsed rule")))

(defn ^datascript.parser/RuleBranch only-branch
  [^datascript.parser/Rule rule]
  (if-some [branch (first (.-branches rule))]
    branch
    (Stdlib.invalid_arg "Expected one parsed rule branch")))

(defn ^:string parsed-rule-name [^datascript.parser/Rule rule]
  (str (.-symbol (.-name rule))))

(defn ^:vector<string> branch-required-names
  [^datascript.parser/RuleBranch branch]
  (dp/rule-branch-required-parameter-names branch))

(defn ^:vector<string> branch-free-names
  [^datascript.parser/RuleBranch branch]
  (let [required-count (count (branch-required-names branch))]
    (subvec (dp/rule-branch-parameter-names branch) required-count)))

(deftest clauses
  (let [input
        (rules-form
         [(rule-branch
           (rule-head "rule" [(symbol-value "?x")])
           [(pattern-form
             [(symbol-value "?x")
              (keyword-value :name)
              (symbol-value "_")])])])
        rules (dp/parse-rules input)
        rule (only-rule rules)
        branch (only-branch rule)]
    (is (= 1 (count rules)))
    (is (= "rule" (parsed-rule-name rule)))
    (is (= [] (branch-required-names branch)))
    (is (= ["?x"] (branch-free-names branch)))
    (is
     (=
      [(expected-pattern
        [(dp/pattern-variable "?x")
         (dp/pattern-attribute :name)
         (dp/pattern-placeholder)])]
      (dp/rule-branch-clauses branch)))))

(deftest rule-vars
  (let [required-x
        (rules-form
         [(rule-branch
           (rule-head
            "rule"
            [(sequence-value [(symbol-value "?x")])
             (symbol-value "?y")])
           [(pattern-form [(symbol-value "_")])])])
        required-x-y
        (rules-form
         [(rule-branch
           (rule-head
            "rule"
            [(sequence-value
              [(symbol-value "?x") (symbol-value "?y")])
             (symbol-value "?a")
             (symbol-value "?b")])
           [(pattern-form [(symbol-value "_")])])])
        required-only
        (rules-form
         [(rule-branch
           (rule-head
            "rule"
            [(sequence-value [(symbol-value "?x")])])
           [(pattern-form [(symbol-value "_")])])])
        required-x-branch
        (only-branch (only-rule (dp/parse-rules required-x)))
        required-x-y-branch
        (only-branch (only-rule (dp/parse-rules required-x-y)))
        required-only-branch
        (only-branch (only-rule (dp/parse-rules required-only)))
        placeholder-clause
        [(expected-pattern [(dp/pattern-placeholder)])]]
    (is (= ["?x"] (branch-required-names required-x-branch)))
    (is (= ["?y"] (branch-free-names required-x-branch)))
    (is (= placeholder-clause
           (dp/rule-branch-clauses required-x-branch)))
    (is (= ["?x" "?y"]
           (branch-required-names required-x-y-branch)))
    (is (= ["?a" "?b"] (branch-free-names required-x-y-branch)))
    (is (= placeholder-clause
           (dp/rule-branch-clauses required-x-y-branch)))
    (is (= ["?x"] (branch-required-names required-only-branch)))
    (is (= [] (branch-free-names required-only-branch)))
    (is (= placeholder-clause
           (dp/rule-branch-clauses required-only-branch))))

  (is
   (thrown-msg?
    "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
    (dp/parse-rules
     (rules-form
      [(rule-branch
        (rule-head "rule" [])
        [(pattern-form [(symbol-value "_")])])]))))

  (is
   (thrown-msg?
    "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
    (dp/parse-rules
     (rules-form
      [(rule-branch
        (rule-head "rule" [(sequence-value [])])
        [(pattern-form [(symbol-value "_")])])]))))

  (is
   (thrown-msg?
    "Rule variables should be distinct"
    (dp/parse-rules
     (rules-form
      [(rule-branch
        (rule-head
         "rule"
         [(symbol-value "?x")
          (symbol-value "?y")
          (symbol-value "?x")])
        [(pattern-form [(symbol-value "_")])])]))))

  (is
   (thrown-msg?
    "Rule variables should be distinct"
    (dp/parse-rules
     (rules-form
      [(rule-branch
        (rule-head
         "rule"
         [(sequence-value
           [(symbol-value "?x") (symbol-value "?y")])
          (symbol-value "?z")
          (symbol-value "?x")])
        [(pattern-form [(symbol-value "_")])])])))))

(deftest branches
  (let [rule-a-b
        (rule-branch
         (rule-head "rule" [(symbol-value "?x")])
         [(pattern-form [(keyword-value :a)])
          (pattern-form [(keyword-value :b)])])
        rule-c
        (rule-branch
         (rule-head "rule" [(symbol-value "?x")])
         [(pattern-form [(keyword-value :c)])])
        other-c
        (rule-branch
         (rule-head "other" [(symbol-value "?x")])
         [(pattern-form [(keyword-value :c)])])
        expected-a-b
        [(expected-pattern [(dp/pattern-attribute :a)])
         (expected-pattern [(dp/pattern-attribute :b)])]
        expected-c
        [(expected-pattern [(dp/pattern-attribute :c)])]
        same-name-rules (dp/parse-rules (rules-form [rule-a-b rule-c]))
        same-name-rule (only-rule same-name-rules)
        same-name-branches (.-branches same-name-rule)
        different-name-rules
        (dp/parse-rules (rules-form [rule-a-b other-c]))]
    (is (= 1 (count same-name-rules)))
    (is (= "rule" (parsed-rule-name same-name-rule)))
    (is (= 2 (count same-name-branches)))
    (is (= expected-a-b
           (dp/rule-branch-clauses (nth same-name-branches 0))))
    (is (= expected-c
           (dp/rule-branch-clauses (nth same-name-branches 1))))
    (is (= 2 (count different-name-rules)))
    (is (= "rule" (parsed-rule-name (nth different-name-rules 0))))
    (is (= expected-a-b
           (dp/rule-branch-clauses
            (only-branch (nth different-name-rules 0)))))
    (is (= "other" (parsed-rule-name (nth different-name-rules 1))))
    (is (= expected-c
           (dp/rule-branch-clauses
            (only-branch (nth different-name-rules 1))))))

  (is
   (thrown-msg?
    "Rule branch should have clauses"
    (dp/parse-rules
     (rules-form
      [(rule-branch
        (rule-head "rule" [(symbol-value "?x")])
        [])]))))

  (is
   (thrown-msg?
    "Arity mismatch for rule 'rule': [?x] vs. [?x ?y]"
    (dp/parse-rules
     (rules-form
      [(rule-branch
        (rule-head "rule" [(symbol-value "?x")])
        [(pattern-form [(symbol-value "_")])])
       (rule-branch
        (rule-head
         "rule"
         [(symbol-value "?x") (symbol-value "?y")])
        [(pattern-form [(symbol-value "_")])])]))))

  (is
   (thrown-msg?
    "Arity mismatch for rule 'rule': [?x] vs. [[?x]]"
    (dp/parse-rules
     (rules-form
      [(rule-branch
        (rule-head "rule" [(symbol-value "?x")])
        [(pattern-form [(symbol-value "_")])])
       (rule-branch
        (rule-head
         "rule"
         [(sequence-value [(symbol-value "?x")])])
        [(pattern-form [(symbol-value "_")])])])))))
