(ns clojure.core-test.derive
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists derive
  (deftest test-derive
    (testing "derive creates direct and transitive relationships"
      (let [one (derive (make-hierarchy) ::rect ::shape)
            two (derive one ::square ::rect)]
        (is (= #{::shape} (parents one ::rect)))
        (is (= #{::rect} (parents two ::square)))
        (is (= #{::rect ::shape} (ancestors two ::square)))
        (is (= #{::rect ::square} (descendants two ::shape)))
        (is (isa? two ::square ::shape))))

    (testing "derive is idempotent for an existing direct edge"
      (let [hierarchy (derive (make-hierarchy) ::rect ::shape)]
        (is (= hierarchy (derive hierarchy ::rect ::shape)))))

    (testing "derive supports multiple parents without losing closure"
      (let [hierarchy (-> (make-hierarchy)
                          (derive ::b ::a)
                          (derive ::c ::a)
                          (derive ::d ::b)
                          (derive ::d ::c))]
        (is (= #{::b ::c} (parents hierarchy ::d)))
        (is (= #{::a ::b ::c} (ancestors hierarchy ::d)))
        (is (= #{::b ::c ::d} (descendants hierarchy ::a)))))

    (testing "derive accepts named symbols in a local hierarchy"
      (let [hierarchy (derive (make-hierarchy) 'app/child 'app/parent)]
        (is (isa? hierarchy 'app/child 'app/parent))))

    (testing "global derive returns nil and updates the global hierarchy"
      (is (nil? (derive ::global-child ::global-parent)))
      (is (isa? ::global-child ::global-parent))
      (is (nil? (underive ::global-child ::global-parent)))
      (is (not (isa? ::global-child ::global-parent))))))
