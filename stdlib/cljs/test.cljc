; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.test source API.

(ns cljs.test)

(type-record map-fixture
  (fixture-before :fn<bool>)
  (fixture-after :fn<bool>))

(type-variant test-fixture
  (FunctionFixture :fn<fn<bool>;bool>)
  (MapFixture :map-fixture))

(type-record namespace-fixtures
  (once-fixtures :list<test-fixture>)
  (each-fixtures :list<test-fixture>))

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

(defn report
  "Dispatches report event `m` through the current `cljs.test` reporter."
  {:inline (fn [m]
             (list '__lg_cljs-test-report
                   (list ':reporter (list 'cljs.test/get-current-env))
                   m))}
  [m]
  (__lg_cljs-test-report (:reporter (get-current-env)) m))

(defn do-report
  "Reports event `m` through the current `cljs.test` reporter.

  LG keeps the shared Native/Melange implementation source-owned and delegates
  the open reporter event map to the same narrow boundary as [[report]]."
  [m]
  (report m))

(defmethod report [:cljs.test/default :pass] [_m]
  (inc-report-counter! :pass))

(defmethod report [:cljs.test/default :fail] [_m]
  (inc-report-counter! :fail))

(defmethod report [:cljs.test/default :error] [_m]
  (inc-report-counter! :error))

(defmethod report [:cljs.test/default :end-run-tests] [_m]
  nil)

(defn testing-contexts-str
  "Returns active testing contexts joined from outermost to innermost."
  []
  (apply str (interpose " " (reverse (:testing-contexts (get-current-env))))))

(defn testing-vars-str
  "Returns the active static test names followed by `location`."
  [location]
  (str
   "("
   (apply str
          (interpose " " (reverse (:testing-vars (get-current-env)))))
   ")"
   " ("
   (:file location)
   ":"
   (:line location)
   (if-some [column (:column location)] (str ":" column) "")
   ")"))

(defn- push-testing-context! [context]
  (let [current (get-current-env)]
    (replace-current-env!
     current
     (:report-counters current)
     (__lg_conj (:testing-contexts current) context))))

(defn- pop-testing-context! []
  (let [current (get-current-env)]
    (replace-current-env!
     current
     (:report-counters current)
     (pop (:testing-contexts current)))))

(defmacro testing
  "Evaluates `body` with `string` appended to the active testing context."
  [string & body]
  (let [last-form (last body)
        last-symbol (if (seq? last-form) (first last-form) nil)
        action-body (or (= 'async last-symbol)
                        (= 'cljs.test/async last-symbol)
                        (= 'testing last-symbol)
                        (= 'cljs.test/testing last-symbol))]
    (if action-body
      `(cljs.test/async-testing-action
        ~string
        (fn [] ~@body))
      `(do
         (cljs.test/push-testing-context! ~string)
         (try
           ~@body
           (finally (cljs.test/pop-testing-context!)))))))

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

(def ^:private ^:ref<map<string;namespace-fixtures>> registered-fixtures
  (atom (hash-map)))

(defn- registered-test-value [name run]
  (record registered-test
    (registered-test-name name)
    (registered-test-action (SynchronousTest run))))

(defn- registered-action-value [name action]
  (record registered-test
    (registered-test-name name)
    (registered-test-action action)))

(defn- register-test!
  "Registers synchronous test thunk `run` under `namespace` and `name`."
  [namespace name run]
  (let [registered (registered-test-value name run)
        namespace-tests (get @registered-tests namespace (list))]
    (swap! registered-tests assoc namespace (__lg_conj namespace-tests registered))
    registered))

(defn- register-test-action!
  "Registers closed test `action` under `namespace` and `name`."
  [namespace name action]
  (let [registered (registered-action-value name action)
        namespace-tests (get @registered-tests namespace (list))]
    (swap! registered-tests assoc namespace (__lg_conj namespace-tests registered))
    registered))

(defn- ^:namespace-fixtures namespace-fixtures-value
  [^:list<test-fixture> once-fixtures ^:list<test-fixture> each-fixtures]
  (record namespace-fixtures
    (once-fixtures once-fixtures)
    (each-fixtures each-fixtures)))

(defn- ^:test-fixture function-fixture-value
  [^:fn<fn<bool>;bool> fixture]
  (FunctionFixture fixture))

(defn- ^:test-fixture map-fixture-value
  [^:fn<bool> before ^:fn<bool> after]
  (MapFixture
   (record map-fixture
     (fixture-before before)
     (fixture-after after))))

(defn- ^:namespace-fixtures register-fixtures!
  "Registers synchronous `fixtures` of `kind` for `namespace`."
  [^string namespace ^:keyword kind ^:list<test-fixture> fixtures]
  (let [current (get @registered-fixtures namespace
                     (namespace-fixtures-value (list) (list)))
        updated
        (if (= kind :once)
          (namespace-fixtures-value
           (reduce (fn [^:list<test-fixture> result fixture]
                     (__lg_conj result fixture))
                   (:once-fixtures current)
                   fixtures)
           (:each-fixtures current))
          (namespace-fixtures-value
           (:once-fixtures current)
           (reduce (fn [^:list<test-fixture> result fixture]
                     (__lg_conj result fixture))
                   (:each-fixtures current)
                   fixtures)))]
    (swap! registered-fixtures assoc namespace updated)
    updated))

(declare run-map-fixtures run-function-fixtures)

(defn- ^boolean run-map-fixture
  [^test-fixture current
   ^:list<test-fixture> remaining
   ^:fn<bool> body]
  (match current
    (MapFixture fixture)
    (do
      ((:fixture-before fixture))
      (try
        (run-map-fixtures remaining body)
        (finally ((:fixture-after fixture)))))

    (FunctionFixture _)
    (raise (Failure "use-fixtures cannot mix function and map fixtures"))))

(defn- ^boolean run-map-fixtures
  [^:list<test-fixture> fixtures ^:fn<bool> body]
  (if (empty? fixtures)
    (body)
    (run-map-fixture (nth fixtures 0) (pop fixtures) body)))

(defn- ^boolean run-function-fixture
  [^test-fixture current
   ^:list<test-fixture> remaining
   ^:fn<bool> body]
  (match current
    (FunctionFixture fixture)
    (fixture (fn [] (run-function-fixtures remaining body)))

    (MapFixture _)
    (raise (Failure "use-fixtures cannot mix function and map fixtures"))))

(defn- ^boolean run-function-fixtures
  [^:list<test-fixture> fixtures ^:fn<bool> body]
  (if (empty? fixtures)
    (body)
    (run-function-fixture (nth fixtures 0) (pop fixtures) body)))

(defn- ^boolean run-fixture
  [^test-fixture current
   ^:list<test-fixture> fixtures
   ^:fn<bool> body]
  (match current
    (MapFixture _) (run-map-fixtures fixtures body)
    (FunctionFixture _) (run-function-fixtures fixtures body)))

(defn- ^boolean run-fixtures
  [^:list<test-fixture> fixtures ^:fn<bool> body]
  (if (empty? fixtures)
    (body)
    (run-fixture (nth fixtures 0) fixtures body)))

(defn- ^boolean map-fixture? [^test-fixture fixture]
  (match fixture
    (MapFixture _) true
    (FunctionFixture _) false))

(defn- validate-fixture-types!
  [^:list<test-fixture> once-fixtures ^:list<test-fixture> each-fixtures]
  (if (or (empty? once-fixtures)
          (empty? each-fixtures)
          (= (map-fixture? (nth once-fixtures 0))
             (map-fixture? (nth each-fixtures 0))))
    true
    (raise
     (Failure
      "use-fixtures :once and :each fixtures must have the same type"))))

(defn- ^boolean execute-registered-test!
  [^registered-test registered-test ^:list<test-fixture> fixtures]
  (let [current (get-current-env)]
    (set-env!
     (test-env-value
      (:report-counters current)
      (__lg_conj (:testing-vars current) (:registered-test-name registered-test))
      (:testing-contexts current)
      (:reporter current)))
    (try
      (do
        (inc-report-counter! :test)
        (match (:registered-test-action registered-test)
          (SynchronousTest run)
          (try
            (run-fixtures fixtures run)
            (catch _
              (do
                (inc-report-counter! :error)
                false)))
          _
          (raise
           (Failure
            "execute-registered-test! expects a synchronous test action"))))
      (finally
       (let [updated (get-current-env)]
         (set-env!
          (test-env-value
           (:report-counters updated)
           (:testing-vars current)
           (:testing-contexts updated)
           (:reporter updated))))))))

(defn- run-registered-test!
  "Runs `registered-test` and returns the updated test environment."
  [registered-test]
  (execute-registered-test! registered-test (list))
  (get-current-env))

(defn- begin-registered-test! [registered-test]
  (let [current (get-current-env)]
    (set-env!
     (test-env-value
      (:report-counters current)
      (__lg_conj (:testing-vars current) (:registered-test-name registered-test))
      (:testing-contexts current)
      (:reporter current)))
    (inc-report-counter! :test)
    current))

(defn- finish-registered-test! [previous]
  (let [updated (get-current-env)]
    (set-env!
     (test-env-value
      (:report-counters updated)
      (:testing-vars previous)
      (:testing-contexts updated)
      (:reporter updated)))
    true))

(defn- registered-test-execution [registered-test fixtures]
  (match (:registered-test-action registered-test)
    (SynchronousTest _)
    (SynchronousTest
     (fn [] (execute-registered-test! registered-test fixtures)))

    (AsyncTest start)
    (AsyncTest
     (fn [continue]
       (let [previous (begin-registered-test! registered-test)
             finished (atom false)
             finish
             (fn []
               (if @finished
                 (continue)
                 (do
                   (reset! finished true)
                   (finish-registered-test! previous)
                   (continue))))]
         (try
           (start finish)
           (catch _
             (do
               (inc-report-counter! :error)
               (finish))))
         true)))

    (DeferredTest create)
    (AsyncTest
     (fn [continue]
       (let [previous (begin-registered-test! registered-test)
             finished (atom false)
             finish
             (fn []
               (if @finished
                 (continue)
                 (do
                   (reset! finished true)
                   (finish-registered-test! previous)
                   (continue))))]
         (try
           (run-block-then (seq (list (create))) finish)
           (catch _
             (do
               (inc-report-counter! :error)
               (finish))))
         true)))

    (TestBlock actions)
    (TestBlock actions)))

(defn- namespace-test-action [namespace]
  (let [fixtures (get @registered-fixtures namespace
                      (namespace-fixtures-value (list) (list)))
        once-fixtures (reverse (:once-fixtures fixtures))
        each-fixtures (reverse (:each-fixtures fixtures))
        registered (reverse (get @registered-tests namespace (list)))
        contains-async
        (reduce
         (fn [found test]
           (or
            found
            (match (:registered-test-action test)
              (SynchronousTest _) false
              _ true)))
         false
         registered)]
    (validate-fixture-types! once-fixtures each-fixtures)
    (if contains-async
      (if (and (empty? once-fixtures) (empty? each-fixtures))
        (TestBlock
         (reverse
          (reduce
           (fn [actions test]
             (__lg_conj actions (registered-test-execution test (list))))
           (list)
           registered)))
        (raise
         (Failure
          "async tests currently require namespaces without fixtures")))
      (SynchronousTest
       (fn []
         (run-fixtures
          once-fixtures
          (fn []
            (doseq [test registered]
              (execute-registered-test! test each-fixtures))
            true)))))))

(defn- ^boolean run-namespace-tests! [^string namespace]
  (run-block (list (namespace-test-action namespace))))

(defn- run-registered-tests!
  "Runs registered tests for `namespaces` in definition order."
  [namespaces]
  (clear-env!)
  (set-env! (empty-env))
  (let [summary (atom (get-current-env))]
    (run-block-then
     (map namespace-test-action namespaces)
     (fn []
       (reset! summary (get-and-clear-env!))
       true))
    @summary))

(defn- run-single-test!
  "Runs one synchronous test thunk `run` named `name`."
  [_name run]
  (clear-env!)
  (set-env! (empty-env))
  (run-registered-test! (registered-test-value _name run))
  (get-and-clear-env!))

(defn- run-block-then [actions finished]
  (let [remaining (seq actions)]
    (if (empty? remaining)
      (finished)
      (match (first remaining)
        (Some (SynchronousTest run))
        (do
          (run)
          (run-block-then (rest remaining) finished))

        (Some (AsyncTest start))
        (let [completed (atom false)
              continue
              (fn []
                (if @completed
                  (do
                    (println
                     "WARNING: Async test called done more than one time.")
                    false)
                  (do
                    (reset! completed true)
                    (run-block-then (rest remaining) finished))))]
          (start continue)
          true)

        (Some (DeferredTest create))
        (run-block-then
         (concat (list (create)) (rest remaining))
         finished)

        (Some (TestBlock injected))
        (run-block-then (concat injected (rest remaining)) finished)

        None (finished)))))

(defn run-block
  "Runs closed synchronous, asynchronous, and injected test actions in order."
  [actions]
  (run-block-then (seq actions) (fn [] true)))

(defn async?
  "Returns `true` when `action` is an asynchronous test action."
  [action]
  (match action
    (AsyncTest _) true
    _ false))

(defn synchronous-test-action
  "Wraps synchronous test thunk `run` in the closed runner action type."
  [run]
  (SynchronousTest run))

(defn- async-test-action [start]
  (AsyncTest start))

(defn- deferred-test-action [create]
  (DeferredTest create))

(defn- async-testing-action [context create]
  (AsyncTest
   (fn [continue]
     (push-testing-context! context)
     (try
       (run-block-then
        (seq (list (create)))
        (fn []
          (pop-testing-context!)
          (continue)))
       (catch error
         (do
           (pop-testing-context!)
           (raise error)
           false)))
     true)))

(defn block
  "Wraps `actions` as a block injected before the remaining actions."
  [actions]
  (TestBlock (reverse (reduce __lg_conj (list) actions))))

(defmacro async
  "Wraps `body` as a CPS test action that binds completion callback `done`."
  [done & body]
  (assert (symbol? done) "async expects a symbol completion callback")
  `(cljs.test/async-test-action
    (fn [~done]
      ~@body
      true)))

(defn- registered-test-step [name run]
  (synchronous-test-action
   (fn []
     (execute-registered-test!
      (registered-test-value name run)
      (list)))))

(defn test-var-block
  "Returns a synchronous block for test thunk `run`."
  [run]
  (list (registered-test-step "<anonymous>" run)))

(defn test-var
  "Runs one statically resolved synchronous test thunk."
  [run]
  (run-block (test-var-block run)))

(defn test-vars-block
  "Returns a synchronous block for `runs` in input order."
  [runs]
  (map
   (fn [run] (registered-test-step "<anonymous>" run))
   runs))

(defn test-vars
  "Runs statically resolved synchronous test thunks in input order."
  [runs]
  (run-block (test-vars-block runs)))

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
  "Defines and registers a synchronous or direct asynchronous test named `name`."
  [name & body]
  (assert (symbol? name) "deftest expects a symbol name")
  (let [namespace (:ns &env)
        test-name (str name)
        last-form (last body)
        last-symbol (if (seq? last-form) (first last-form) nil)
        action-test (or (= 'async last-symbol)
                        (= 'cljs.test/async last-symbol)
                        (= 'testing last-symbol)
                        (= 'cljs.test/testing last-symbol))]
    (if action-test
      `(do
         (defn ~name [] ~@body)
         (cljs.test/register-test-action!
          ~namespace
          ~test-name
          (cljs.test/deferred-test-action (fn [] (~name)))))
      `(do
         (defn ~name [] ~@body)
         (cljs.test/register-test!
          ~namespace
          ~test-name
          (fn []
            (~name)
            true))))))

(defmacro use-fixtures
  "Registers synchronous `fixtures` of `kind` for the current namespace.

  `kind` must be `:once` or `:each`. Fixtures in one call must all be
  functions or all be maps with optional `:before` and `:after` functions."
  [kind & fixtures]
  (assert (or (= kind :once) (= kind :each))
          "use-fixtures expects :once or :each")
  (let [map-fixtures (filter (fn [fixture] (map? fixture)) fixtures)]
    (assert (or (empty? map-fixtures)
                (= (count map-fixtures) (count fixtures)))
            "use-fixtures cannot use mixed function and map fixtures")
    `(cljs.test/register-fixtures!
      ~(:ns &env)
      ~kind
      (list
       ~@(map
          (fn [fixture]
            (if (map? fixture)
              (let [before (:before fixture)
                    after (:after fixture)]
                `(cljs.test/map-fixture-value
                  ~(if before `(fn [] (~before) true) `(fn [] true))
                  ~(if after `(fn [] (~after) true) `(fn [] true))))
              `(cljs.test/function-fixture-value
                (fn [body]
                  (~fixture (fn [] (body) true))
                  true))))
          fixtures)))))

(defmacro run-test
  "Runs the synchronous test named by `test-symbol` and returns its environment."
  [test-symbol]
  (assert (symbol? test-symbol) "run-test expects a test symbol")
  `(cljs.test/run-single-test!
    ~(str test-symbol)
    (fn []
      (~test-symbol)
      true)))

(defmacro test-all-vars-block
  "Returns the registered test action for quoted namespace `namespace`."
  [namespace]
  (assert
   (and (seq? namespace)
        (= 'quote (first namespace))
        (symbol? (second namespace)))
   "test-all-vars-block expects a quoted namespace symbol")
  `(let [had-env#
         (match cljs.test/*current-env*
           None false
           (Some _) true)]
     (list
      (cljs.test/synchronous-test-action
       (fn []
         (if had-env#
           true
           (do
             (cljs.test/set-env! (cljs.test/empty-env))
             true))))
      (cljs.test/namespace-test-action ~(str (second namespace)))
      (cljs.test/synchronous-test-action
       (fn []
         (if had-env#
           true
           (do
             (cljs.test/clear-env!)
             true)))))))

(defmacro test-all-vars
  "Runs all statically registered tests in quoted `namespace`."
  [namespace]
  (assert
   (and (seq? namespace)
        (= 'quote (first namespace))
        (symbol? (second namespace)))
   "test-all-vars expects a quoted namespace symbol")
  `(cljs.test/run-block (cljs.test/test-all-vars-block ~namespace)))

(defmacro test-ns-block
  "Returns environment setup and registered test actions for `namespace`."
  [env namespace]
  (assert
   (and (seq? namespace)
        (= 'quote (first namespace))
        (symbol? (second namespace)))
   "test-ns-block expects a quoted namespace symbol")
  `(list
    (cljs.test/synchronous-test-action
     (fn []
       (cljs.test/set-env! ~env)
       true))
    (cljs.test/namespace-test-action ~(str (second namespace)))))

(defmacro test-ns
  "Runs all statically registered tests in quoted `namespace`."
  ([namespace]
   (assert
    (and (seq? namespace)
         (= 'quote (first namespace))
         (symbol? (second namespace)))
    "test-ns expects a quoted namespace symbol")
   `(cljs.test/test-ns (cljs.test/empty-env) ~namespace))
  ([env namespace]
   (assert
    (and (seq? namespace)
         (= 'quote (first namespace))
         (symbol? (second namespace)))
    "test-ns expects a quoted namespace symbol")
   `(cljs.test/run-block
     (list
      (cljs.test/synchronous-test-action
       (fn []
         (cljs.test/set-env! ~env)
         true))
      (cljs.test/namespace-test-action ~(str (second namespace)))
      (cljs.test/synchronous-test-action
       (fn []
         (cljs.test/clear-env!)
         true))))))

(defmacro run-tests-block
  "Returns a test block for quoted namespaces with optional initial `env`."
  [env-or-namespace & namespaces]
  (let [quoted-first (and (seq? env-or-namespace)
                          (= 'quote (first env-or-namespace))
                          (symbol? (second env-or-namespace)))
        env (if quoted-first `(cljs.test/empty-env) env-or-namespace)
        namespaces (if quoted-first
                     (cons env-or-namespace namespaces)
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
     "run-tests-block expects each namespace argument to be a quoted namespace symbol")
    `(list
      (cljs.test/synchronous-test-action
       (fn []
         (cljs.test/set-env! ~env)
         true))
      ~@(map
         (fn [namespace]
           `(cljs.test/namespace-test-action ~(str (second namespace))))
         namespaces)
      (cljs.test/synchronous-test-action
       (fn []
         (cljs.test/clear-env!)
         true)))))

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
