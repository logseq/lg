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

(defn with-context [context body]
  (runtime/with-context context body))

(defn register-once-fixture!
  [^:string namespace ^:fn<fn<unit;unit>;unit> fixture]
  (runtime/register-once-fixture namespace fixture))

(defn register-each-fixture!
  [^:string namespace ^:fn<fn<unit;unit>;unit> fixture]
  (runtime/register-each-fixture namespace fixture))

(defn invoke-fixture!
  [^:fn<fn<unit;unit>;unit> fixture ^:fn<unit;unit> run]
  (fixture
   (fn []
     (clojure.test/invoke-test-body! run))))

(defmacro is
  ([form]
   `(clojure.test/is ~form ""))
  ([form message]
   (cond
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

     :else
     `(if ~form
        (clojure.test/pass!)
        (clojure.test/fail! ~(str form) ~message)))))

(defmacro are [argv expression & arguments]
  `(do ~@(clojure.test/expand-are argv expression arguments)))

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
