(ns clojure.core-test.drop-last
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))

(when-var-exists drop-last
  (deftest test-drop-last
    (let [values (drop-last [1 2 3])]
      (is (p/lazy-seq? values))
      (is (not (realized? values)))
      (is (= '(1 2) values)))

    (is (= '(0 1 2 3) (drop-last (range 5))))
    (is (= '(1 2 3 4) (drop-last '(1 2 3 4 5))))
    (is (= '(\a \b \c \d) (drop-last "abcde")))
    (is (= '() (drop-last [1])))
    (is (= '() (drop-last nil)))

    (is (= '(0 1 2 3 4) (drop-last 0 (range 5))))
    (is (= '(0 1 2) (drop-last 2 (range 5))))
    (is (= '(0 1 2 3 4) (drop-last -1 (range 5))))
    (is (= '() (drop-last 100 (range 5))))))
