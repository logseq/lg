(ns clojure.core-test.compare
  (:require [clojure.test :refer [deftest is]]))

(deftest test-compare-integers
  (is (neg? (compare 0 10)))
  (is (zero? (compare 0 0)))
  (is (pos? (compare 10 0))))

(deftest test-compare-characters
  (is (neg? (compare \a \b))))

(deftest test-compare-strings
  (is (zero? (compare "cat" "cat")))
  (is (neg? (compare "cat" "dog"))))

(deftest test-compare-symbols
  (is (neg? (compare 'app/cat 'app/dog))))

(deftest test-compare-keywords
  (is (neg? (compare :app/cat :app/dog))))

(deftest test-compare-vectors
  (is (zero? (compare (vec (range 0)) (vec (range 0)))))
  (is (neg? (compare (vec (range 0)) [1 2])))
  (is (pos? (compare [3] [1])))
  (is (neg? (compare [1 2] [1 3]))))

(deftest test-compare-nested-vectors
  (is (neg? (compare [[1]] [[2]])))
  (is (zero? (compare [[:app/a]] [[:app/a]]))))
