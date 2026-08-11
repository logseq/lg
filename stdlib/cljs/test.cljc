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
