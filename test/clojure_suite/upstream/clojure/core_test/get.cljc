(ns clojure.core-test.get
  (:require [clojure.test :as t :refer [are deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists get
  (deftest test-string-get
    (are [expected index] (= expected (get "ab" index))
      \a 0
      \b 1
      nil -1
      nil 2)

    (is (= nil (get "" 0)))
    (is (= \a (get "ab" 0 nil)))
    (is (= nil (get "ab" 2 nil)))
    (is (= \a (get "ab" 0 \z)))
    (is (= \z (get "ab" -1 \z)))
    (is (= \z (get "ab" 2 \z)))
    (is (= \b (get-in ["ab"] [0 1])))))
