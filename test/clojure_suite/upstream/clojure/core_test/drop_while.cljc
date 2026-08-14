(ns clojure.core-test.drop-while
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))

(when-var-exists drop-while
  (deftest test-drop-while
    (let [values (drop-while #(< % 5) (range))]
      (is (p/lazy-seq? values))
      (is (not (realized? values)))
      (is (= 5 (first values))))

    (is (= (range 5 10) (drop-while #(< % 5) (range 10))))
    (is (= [3 4] (drop-while #(< % 3) [0 1 2 3 4])))
    (is (= '() (drop-while (constantly false) nil)))
    (is (= '() (drop-while (constantly true) (range 5))))

    (is (= [5 6 7 8 9]
           (into [] (drop-while #(< % 5)) (range 10))))
    (let [xf (drop-while neg?)]
      (is (= [0 1 2] (sequence xf [-2 -1 0 1 2])))
      (is (= [0 1 2] (sequence xf [-1 0 1 2]))))))
