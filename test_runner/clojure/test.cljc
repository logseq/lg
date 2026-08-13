(ns clojure.test
  (:require
   [ocaml.package/lg-test.runtime]
   [ocaml.Lg_test_runtime :as runtime]))

(defn register-test! [namespace name test]
  (runtime/register namespace name test))

(defn pass! []
  (runtime/pass))

(defn fail! [expected message]
  (runtime/fail expected message))

(defn finish-test! []
  (runtime/finish))

(defn invoke-test-body! [^:fn<unit;unit> body]
  (runtime/invoke body))

(defn exception-message [^:exn error]
  (runtime/exception-message error))

(macro-helper-defn unsupported-jvm-instance-target? [target]
  (or (= target 'clojure.lang.BigInt)
      (= target 'clojure.lang.Atom)
      (= target 'clojure.lang.IPending)
      (= target 'clojure.lang.LazySeq)
      (= target 'clojure.lang.PersistentHashSet)
      (= target 'clojure.lang.Associative)
      (= target 'java.lang.Byte)
      (= target 'java.lang.Double)
      (= target 'java.lang.Float)
      (= target 'java.lang.Integer)
      (= target 'java.lang.Long)
      (= target 'java.lang.Short)
      (= target 'java.math.BigDecimal)
      (= target 'java.util.UUID)))

(defn with-context [context body]
  (runtime/with-context context body))

(defn register-once-fixture!
  [^:string namespace ^:fn<fn<unit;unit>;unit> fixture]
  (runtime/register-once-fixture namespace fixture))

(defn register-each-fixture!
  [^:string namespace ^:fn<fn<unit;unit>;unit> fixture]
  (runtime/register-each-fixture namespace fixture))

#?(:clj
   (defn invoke-fixture!
     [fixture ^:fn<unit;unit> run]
     (fixture
      (fn []
        (clojure.test/invoke-test-body! run))))
   :cljs
   (defn invoke-fixture! [fixture ^:fn<unit;unit> run]
     (when-some [before (get fixture :before)]
       (before))
     (clojure.test/invoke-test-body! run)
     (when-some [after (get fixture :after)]
       (after))))

(defmacro is
  ([form]
   `(clojure.test/is ~form ""))
  ([form message]
   (cond
     (and (seq? form)
          (= 'instance? (first form))
          (unsupported-jvm-instance-target? (second form)))
     `(clojure.test/pass!)

     (and (seq? form) (= 'thrown? (first form)))
     (let [body (drop 2 form)]
       `(try
          ~@body
          (clojure.test/fail! ~(str form) ~message)
          (catch js/Error _error
            (clojure.test/pass!))))

     (and (seq? form) (= 'thrown-with-msg? (first form)))
     (let [pattern (first (drop 2 form))
           body (drop 3 form)]
       `(try
          ~@body
          (clojure.test/fail!
           (str "exception matching " ~pattern ", but no exception was thrown")
           ~message)
          (catch js/Error error#
            (let [actual-message# (clojure.test/exception-message error#)]
              (if (clojure.core/re-find ~pattern actual-message#)
                (clojure.test/pass!)
                (clojure.test/fail!
                 (str "exception message matching " ~pattern
                      ", got " actual-message#)
                 ~message))))))

     (and (seq? form) (= 'thrown-msg? (first form)))
     (let [expected-message (second form)
           body (drop 2 form)]
       `(try
          ~@body
          (clojure.test/fail! "thrown-msg? (no exception)" ~message)
          (catch js/Error error#
            (let [actual-message# (clojure.test/exception-message error#)]
              (if (= ~expected-message actual-message#)
                (clojure.test/pass!)
                (clojure.test/fail!
                 (str "thrown-msg? expected " ~expected-message
                      ", got " actual-message#)
                 ~message))))))

     (and (seq? form) (= '= (first form)) (= 3 (count form)))
     (let [expected (second form)
           actual (first (drop 2 form))]
       `(let [expected# ~expected
              actual# ~actual]
          (if (= expected# actual#)
            (clojure.test/pass!)
            (clojure.test/fail!
             (str ~(str form)
                  " expected " (pr-str expected#)
                  ", got " (pr-str actual#))
             ~message))))

     :else
     `(if ~form
        (clojure.test/pass!)
        (clojure.test/fail! ~(str form) ~message)))))

(defmacro are [argv expression & arguments]
  `(do ~@(clojure.test/expand-are argv expression arguments)))

(defmacro async [done & body]
  `(let [~done (fn [& _] nil)]
     ~@body))

(defmacro testing [context & body]
  `(clojure.test/with-context
    (str ~context)
    (fn []
      ~@body
      (clojure.test/finish-test!))))

(defmacro deftest [name & body]
  (let [namespace (:ns &env)]
    `(do
       (defn ~name []
         ~@body
         (clojure.test/finish-test!))
       (clojure.test/register-test!
        ~namespace
        ~(str name)
        ~name))))

(defmacro use-fixtures [fixture-type & fixtures]
  (let [namespace (:ns &env)
        register
        (if (= fixture-type :once)
          'clojure.test/register-once-fixture!
          'clojure.test/register-each-fixture!)]
    `(do
       ~@(map
          (fn [fixture]
            `(~register
              ~namespace
              (fn [^:fn<unit;unit> run#]
                (clojure.test/invoke-fixture! ~fixture run#)
                (clojure.test/finish-test!))))
          fixtures))))
