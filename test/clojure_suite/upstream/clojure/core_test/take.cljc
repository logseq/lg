(ns clojure.core-test.take
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))

(when-var-exists take
  (deftest test-take
    (let [values (take 3 (range 10))]
      (is (p/lazy-seq? values))
      (is (not (realized? values)))
      (is (= '(0 1 2) values)))

    (is (= '() (take 0 (range 10))))
    (is (= '(0) (take 1 (range 10))))
    (is (= (range 10) (take 100 (range 10))))
    (is (= (range 5) (take 5 (range))))
    (is (= '() (take 5 nil)))
    (is (= '() (take 5 [])))

    (let [xf (take 2)]
      (is (= [0 1] (into [] xf (range 5))))
      (is (= [0 1] (into [] xf (range 5)))))
    (is (= [] (into [] (take 0) (range 10))))))
