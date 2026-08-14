(ns clojure.core-test.run-bang
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists run!
  (deftest test-run!
    (testing "run! returns nil and invokes callbacks sequentially"
      (let [values (volatile! [])]
        (is (nil? (run! (fn [value] (vswap! values conj value)) [1 2 3])))
        (is (= [1 2 3] @values))))

    (testing "empty inputs do not invoke the callback"
      (let [calls (volatile! 0)]
        (run! (fn [_] (vswap! calls inc)) nil)
        (run! (fn [_] (vswap! calls inc)) [])
        (is (zero? @calls))))

    (testing "nil elements are values rather than sequence termination"
      (let [calls (volatile! 0)]
        (run! (fn [_] (vswap! calls inc)) [nil 1])
        (is (= 2 @calls))))

    (testing "a reduced callback result terminates traversal"
      (let [calls (volatile! 0)]
        (is (nil?
             (run! (fn [_]
                     (vswap! calls inc)
                     (reduced :done))
                   [1 2 3])))
        (is (= 1 @calls))))))
