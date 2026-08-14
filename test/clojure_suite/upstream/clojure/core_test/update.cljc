(ns clojure.core-test.update
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists update
  (deftest test-update
    (testing "map values"
      (is (= {:k 5} (update {:k 5} :k identity)))
      (is (= {:k 6} (update {:k 5} :k inc)))
      (is (= {:k 15} (update {:k 5} :k * 3)))
      (is (= {:k 60} (update {:k 5} :k * 3 4)))
      (is (= {:k 300} (update {:k 5} :k * 3 4 5)))
      (is (= {:k 1800} (update {:k 5} :k * 3 4 5 6)))
      (is (= {:k 12600} (update {:k 5} :k * 3 4 5 6 7))))

    (testing "the current value is the first callback argument"
      (is (= {:k 2} (update {:k 5} :k - 3)))
      (is (= {:k -2} (update {:k 5} :k - 3 4))))

    (testing "vector values"
      (is (= [1 1] (update [0 1] 0 inc)))
      (is (= [0 2] (update [0 1] 1 inc))))

    (testing "nested collection values"
      (is (= {:a [0 1 2 3] :b [4]}
             (update {:a [0 1 2] :b [4]} :a conj 3))))))
