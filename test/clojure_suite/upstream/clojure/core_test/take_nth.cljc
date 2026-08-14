(ns clojure.core-test.take-nth
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists take-nth
  (deftest test-take-nth
    (is (= (range 10) (take-nth 1 (range 10))))
    (is (= (range 0 10 2) (take-nth 2 (range 10))))
    (is (= (range 0 10 3) (take-nth 3 (range 10))))
    (is (= '(\C \o \u \e \R \c \s) (take-nth 2 "Clojure Rocks")))
    (is (= '() (take-nth 2 nil)))

    (is (= [0 2 4 6 8]
           (transduce (take-nth 2) conj [] (range 10))))
    (is (= [0 2 4 6 8]
           (transduce (take-nth -2) conj [] (range 10))))
    (is (= []
           (transduce (take-nth 0) conj [] (range 10))))))
