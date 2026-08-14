(ns clojure.core-test.bound-fn-star
  (:require [clojure.test :refer [deftest is testing]]))

(def ^:dynamic *value* :unset)

(defn current-value []
  *value*)

(deftest test-bound-fn-star-captures-active-bindings
  (testing "a wrapper created without overrides observes the call site"
    (let [function (bound-fn* current-value)]
      (is (= :unset (function)))
      (binding [*value* :call-site]
        (is (= :call-site (function))))))
  (testing "a wrapper created inside binding preserves that binding"
    (let [function (binding [*value* :captured]
                     (bound-fn* current-value))]
      (is (= :captured (function)))
      (binding [*value* :later]
        (is (= :captured (function)))))))

(deftest test-bound-fn-star-nested-capture
  (binding [*value* :first]
    (let [first-function (bound-fn* current-value)]
      (binding [*value* :second]
        (let [second-function (bound-fn* current-value)]
          (is (= :first (first-function)))
          (is (= :second (second-function))))))))
