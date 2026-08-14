(ns clojure.core-test.underive
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists underive
  (def shape-hierarchy
    (-> (make-hierarchy)
        (derive ::circle ::shape)
        (derive ::rect ::shape)
        (derive ::square ::rect)))

  (def diamond-hierarchy
    (-> (make-hierarchy)
        (derive ::b ::a)
        (derive ::c ::a)
        (derive ::d ::b)
        (derive ::d ::c)))

  (deftest test-underive
    (testing "removing a leaf edge rebuilds closure"
      (let [hierarchy (underive shape-hierarchy ::square ::rect)]
        (is (nil? (parents hierarchy ::square)))
        (is (= #{::circle ::rect} (descendants hierarchy ::shape)))
        (is (not (isa? hierarchy ::square ::shape)))))

    (testing "removing a middle edge preserves remaining direct edges"
      (let [hierarchy (underive shape-hierarchy ::rect ::shape)]
        (is (= #{::rect} (parents hierarchy ::square)))
        (is (= #{::rect} (ancestors hierarchy ::square)))
        (is (= #{::circle} (descendants hierarchy ::shape)))
        (is (not (isa? hierarchy ::square ::shape)))))

    (testing "removing one side of a diamond preserves the alternate path"
      (let [one (underive diamond-hierarchy ::d ::b)
            two (underive one ::d ::c)]
        (is (= #{::c} (parents one ::d)))
        (is (= #{::a ::c} (ancestors one ::d)))
        (is (isa? one ::d ::a))
        (is (nil? (parents two ::d)))
        (is (not (isa? two ::d ::a)))))

    (testing "removing an absent direct edge is idempotent"
      (is (= shape-hierarchy
             (underive shape-hierarchy ::square ::shape))))

    (testing "global underive returns nil and updates the global hierarchy"
      (derive ::global-child ::global-parent)
      (is (isa? ::global-child ::global-parent))
      (is (nil? (underive ::global-child ::global-parent)))
      (is (not (isa? ::global-child ::global-parent))))))
