(ns clojure.core-test.bound-fn
  (:require [clojure.test :refer [deftest is testing]]))

(def ^:dynamic *value* :unset)

(deftest test-bound-fn-captures-active-bindings
  (testing "a function created without overrides observes the call site"
    (let [function (bound-fn [] *value*)]
      (is (= :unset (function)))
      (binding [*value* :call-site]
        (is (= :call-site (function))))))
  (testing "a function created inside binding preserves that binding"
    (let [function (binding [*value* :captured]
                     (bound-fn [] *value*))]
      (is (= :captured (function)))
      (binding [*value* :later]
        (is (= :captured (function)))))))

(deftest test-bound-fn-nested-capture
  (binding [*value* :first]
    (let [first-function (bound-fn [] *value*)]
      (binding [*value* :second]
        (let [second-function (bound-fn [] *value*)]
          (is (= :first (first-function)))
          (is (= :second (second-function))))))))
