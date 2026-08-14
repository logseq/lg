(ns clojure.core-test.nfirst
  (:require clojure.core
            [clojure.test :as t :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))

(when-var-exists nfirst
  (deftest test-nfirst
    (testing "common"
      (is (= nil (nfirst '())))
      (is (= nil (nfirst [])))
      (is (= nil (nfirst {})))
      (is (= nil (nfirst #{})))
      (is (= nil (nfirst nil)))
      (is (= '(:b) (nfirst {:a :b})))
      (is (= '(1) (nfirst [[0 1] [2 3]])))
      (is (= '(1) (nfirst '([0 1] [2 3]))))
      (is (= '(1 2 3 4) (nfirst (repeat (range 0 5)))))
      (is (= '(\b) (nfirst ["ab" "cd"])))
      (is (= '(\b \c \d) (nfirst ["abcd"])))
      (is (= '(\b \c \d) (nfirst #{"abcd"}))))))
