(ns clojure.core-test.var-qmark
  (:require [clojure.test :refer [deftest is testing]]))

(def local-value :local/value)

(deftest test-var?
  (testing "var syntax"
    (is (var? #'local-value))
    (is (var? (var local-value)))
    (is (var? #'var?)))

  (testing "resolved values are not vars"
    (is (not (var? local-value)))
    (is (not (var? var?)))
    (is (not (var? 'local-value)))
    (is (not (var? "local-value")))
    (is (not (var? 42)))
    (is (not (var? nil)))))
