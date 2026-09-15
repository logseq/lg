(ns lg-test.compatibility-test
  (:require
   [clojure.test :refer [are deftest is run-tests testing]]
   [ocaml.Lg_test_runtime :as runtime]))

(def fixture-events (atom (subvec [:seed] 1)))
(def each-count (atom 0))

(defn once-fixture [run]
  (swap! fixture-events conj :before)
  (run)
  (swap! fixture-events conj :after))

(defn each-fixture [run]
  (swap! each-count inc)
  (run))

(def once-fixture-value
  #?(:clj once-fixture
     :cljs {:before #(swap! fixture-events conj :before)
            :after #(swap! fixture-events conj :after)}))

(def each-fixture-value
  #?(:clj each-fixture
     :cljs {:before #(swap! each-count inc)}))

(clojure.test/use-fixtures :once once-fixture-value)
(clojure.test/use-fixtures :each each-fixture-value)

(deftest arithmetic
  (testing "basic arithmetic"
    (is (= [:before] @fixture-events))
    (is (= 1 @each-count))
    (is (= 4 (+ 2 2)) "addition")
    (are [expected actual]
         (= expected actual)
         2 (+ 1 1)
         6 (* 2 3)))
  (testing :keyword-context
    (is true)))

(deftest exceptions
  (testing "portable thrown assertion"
    (is (= 2 @each-count))
    (is (thrown? Exception
                 (throw (ex-info "boom" {}))))
    (is (thrown-with-msg? ExceptionInfo #"bo+m"
                          (throw (ex-info "boom" {}))))
    (is (thrown-msg? "exact boom"
                     (throw (ex-info "exact boom" {}))))))

(deftest tag-and-quoted-symbol-assertions
  (let [evaluations (atom 0)]
    (runtime/begin-case)
    (is (= (tag String "left")
           (do (swap! evaluations inc) (tag String "right"))))
    (is (= (tag Object 1) (tag Object 2)))
    (is (= 'String 'Object))
    (are [expected actual] (= expected actual)
         (tag String "same") (tag String "same")
         (tag String "left") (tag String "right"))
    (let [[assertions failures] (runtime/finish-case)]
      (runtime/begin-case)
      (is (= 1 @evaluations))
      (is (= 5 assertions))
      (is (= 4 (count failures))))))

(run-tests)
