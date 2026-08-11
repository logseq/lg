; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.test source API.

(ns cljs.test)

(defn- test-env-value [report-counters testing-vars testing-contexts reporter]
  (record test-env
    (report-counters report-counters)
    (testing-vars testing-vars)
    (testing-contexts testing-contexts)
    (reporter reporter)))

(defn empty-env
  "Returns an empty test environment for `reporter`.

  The default reporter is `:cljs.test/default`."
  ([] (empty-env :cljs.test/default))
  ([reporter]
   (test-env-value
    (hash-map :test 0 :pass 0 :fail 0 :error 0)
    (list)
    (list)
    reporter)))

(def ^:dynamic *current-env* None)

(defn get-current-env
  "Returns the current test environment, or a fresh default environment."
  []
  (or *current-env* (empty-env)))

(defn set-env!
  "Sets the current test environment to `new-env` and returns it."
  [new-env]
  (do
    (set! *current-env* (Some new-env))
    new-env))

(defn clear-env!
  "Clears the current test environment."
  []
  (set! *current-env* None))

(defn get-and-clear-env!
  "Returns the current test environment and then clears it."
  []
  (let [current (get-current-env)]
    (clear-env!)
    current))

(defn- replace-current-env! [current report-counters testing-contexts]
  (set-env!
   (test-env-value
    report-counters
    (:testing-vars current)
    testing-contexts
    (:reporter current))))

(defn inc-report-counter!
  "Increments report counter `name` and returns the updated environment."
  [name]
  (let [current (get-current-env)
        counters (:report-counters current)]
    (replace-current-env!
     current
     (assoc counters name (inc (get counters name 0)))
     (:testing-contexts current))))

(defn testing-contexts-str
  "Returns active testing contexts joined from outermost to innermost."
  []
  (apply str (interpose " " (reverse (:testing-contexts (get-current-env))))))

(defn- push-testing-context! [context]
  (let [current (get-current-env)]
    (replace-current-env!
     current
     (:report-counters current)
     (conj (:testing-contexts current) context))))

(defn- pop-testing-context! []
  (let [current (get-current-env)]
    (replace-current-env!
     current
     (:report-counters current)
     (pop (:testing-contexts current)))))

(defmacro testing
  "Evaluates `body` with `string` appended to the active testing context."
  [string & body]
  `(do
     (cljs.test/push-testing-context! ~string)
     (try
       ~@body
       (finally (cljs.test/pop-testing-context!)))))

(defprotocol ITestNamespaceValue
  (-test-namespace-value? [value]))

(extend-type :symbol
  ITestNamespaceValue
  (-test-namespace-value? [_value] true))

(extend-type :string
  ITestNamespaceValue
  (-test-namespace-value? [_value] false))

(defn ns?
  "Returns `true` when `value` is a namespace symbol value."
  [value]
  (ITestNamespaceValue/-test-namespace-value? value))

(def ^:private registered-tests
  (atom (hash-map)))

(defn- registered-test-value [name run]
  (record registered-test
    (registered-test-name name)
    (registered-test-run run)))

(defn- register-test!
  "Registers synchronous test thunk `run` under `namespace` and `name`."
  [namespace name run]
  (let [registered (registered-test-value name run)
        namespace-tests (get @registered-tests namespace (list))]
    (swap! registered-tests assoc namespace (conj namespace-tests registered))
    registered))

(defn- run-registered-test!
  "Runs `registered-test` and returns the updated test environment."
  [registered-test]
  (inc-report-counter! :test)
  (try
    ((:registered-test-run registered-test))
    (catch _
      (do
        (inc-report-counter! :error)
        false)))
  (get-current-env))

(defn- run-registered-tests!
  "Runs synchronous tests registered for `namespaces` in definition order."
  [namespaces]
  (clear-env!)
  (set-env! (empty-env))
  (doseq [namespace namespaces
          registered (reverse (get @registered-tests namespace (list)))]
    (run-registered-test! registered))
  (get-and-clear-env!))

(defn- run-single-test!
  "Runs one synchronous test thunk `run` named `name`."
  [_name run]
  (clear-env!)
  (set-env! (empty-env))
  (run-registered-test! (registered-test-value _name run))
  (get-and-clear-env!))

(defn- is-result [result]
  (if result
    (inc-report-counter! :pass)
    (inc-report-counter! :fail))
  result)

(defmacro try-expr
  "Evaluates boolean `form`, records its outcome, and catches errors."
  [msg form]
  `(try
     (cljs.test/is-result ~form)
     (catch _
       (do
         (cljs.test/inc-report-counter! :error)
         false))))

(defmacro is
  "Evaluates boolean `form`, records its outcome, and returns the result."
  ([form] `(cljs.test/is ~form nil))
  ([form msg] `(cljs.test/try-expr ~msg ~form)))

(defmacro are
  "Checks each substitution of `argv` into `expr` with [[is]]."
  [argv expr & args]
  (cons 'do
        (clojure.test/expand-are 'cljs.test/is argv expr args)))

(defmacro deftest
  "Defines and registers a synchronous test named `name`."
  [name & body]
  (assert (symbol? name) "deftest expects a symbol name")
  (let [namespace (:ns &env)
        test-name (str name)]
    `(do
       (defn ~name [] ~@body)
       (cljs.test/register-test!
        ~namespace
        ~test-name
        (fn []
          (~name)
          true)))))

(defmacro run-test
  "Runs the synchronous test named by `test-symbol` and returns its environment."
  [test-symbol]
  (assert (symbol? test-symbol) "run-test expects a test symbol")
  `(cljs.test/run-single-test!
    ~(str test-symbol)
    (fn []
      (~test-symbol)
      true)))

(defmacro run-tests
  "Runs registered synchronous tests for quoted `namespaces`."
  [& namespaces]
  (let [namespaces (if (empty? namespaces)
                     (list (list 'quote (:ns &env)))
                     namespaces)]
    (assert
     (= (count namespaces)
        (count
         (filter
          (fn [namespace]
            (and (seq? namespace)
                 (= 'quote (first namespace))
                 (symbol? (second namespace))))
          namespaces)))
     "run-tests expects each quoted namespace argument to be a quoted namespace symbol")
    `(cljs.test/run-registered-tests!
      (list ~@(map (fn [namespace] (str (second namespace))) namespaces)))))

(defn- default-fixture [fixture]
  (fixture))

(defn compose-fixtures
  "Composes fixture functions `f1` and `f2` into one fixture function.

  Function fixtures are incompatible with map fixtures."
  [f1 f2]
  (fn [fixture]
    (f1 (fn [] (f2 fixture)))))

(defn join-fixtures
  "Composes `fixtures` in order.

  Returns a valid identity fixture when `fixtures` is empty. Function fixtures
  are incompatible with map fixtures."
  [fixtures]
  (reduce compose-fixtures default-fixture fixtures))

(defn successful?
  "Returns `true` when `summary` reports no failures or errors."
  [summary]
  (and (zero? (:fail summary 0))
       (zero? (:error summary 0))))
