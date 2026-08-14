(ns clojure.core-test.binding
  (:require [clojure.test :refer [deftest is testing]]))

(def ^:dynamic *value* :unset)
(def ^:dynamic *snapshot* :unset)

(defn ^:dynamic *step* [^:int value]
  (inc value))

(defn current-value []
  *value*)

(deftest test-binding-static-vars
  (is (= :unset *value*))
  (is (= :set (binding [*value* :set] *value*)))
  (is (= :set (binding [*value* :set] (current-value))))
  (is (= 0 (binding [*step* dec] (*step* 1))))
  (is (= 2 (*step* 1)))
  (is (= :unset *value*)))

(deftest test-binding-nesting-and-restoration
  (binding [*value* :outer]
    (is (= :outer (current-value)))
    (binding [*value* :inner]
      (is (= :inner (current-value))))
    (is (= :outer (current-value))))
  (is (= :unset (current-value))))

(deftest test-binding-values-are-sequential-and-by-value
  (binding [*snapshot* *value*
            *value* :later]
    (is (= :unset *snapshot*))
    (is (= :later *value*)))
  (testing "a nested binding expression is evaluated before installation"
    (binding [*snapshot* (binding [*value* :captured] (current-value))]
      (is (= :captured *snapshot*))
      (is (= :unset *value*)))))
