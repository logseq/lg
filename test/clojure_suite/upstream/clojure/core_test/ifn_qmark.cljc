(ns clojure.core-test.ifn-qmark
  (:require [clojure.test :refer [deftest is testing]]))

(defn local-function [value]
  value)

(deftest test-ifn-functions
  (is (ifn? local-function))
  (is (ifn? identity))
  (is (ifn? (fn [value] value)))
  (is (ifn? #(inc %))))

(deftest test-ifn-callable-collections
  (is (ifn? {:a 1}))
  (is (ifn? #{:a :b}))
  (is (ifn? [:a :b]))
  (is (ifn? :keyword))
  (is (ifn? 'symbol)))

(deftest test-ifn-non-callable-values
  (is (not (ifn? nil)))
  (is (not (ifn? 42)))
  (is (not (ifn? "string")))
  (is (not (ifn? \a))))
