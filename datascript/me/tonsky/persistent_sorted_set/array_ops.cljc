(ns me.tonsky.persistent-sorted-set.array-ops
  (:require [me.tonsky.persistent-sorted-set.arrays :as arrays]))

(defn cut-n-splice [arr cut-from cut-to splice-from splice-to values]
  (let [values-length (arrays/alength values)
        left-length (- splice-from cut-from)
        right-length (- cut-to splice-to)
        values-end (+ left-length values-length)
        result-length (+ left-length values-length right-length)]
    (if (= 0 result-length)
      (arrays/empty-array)
      (let [initial
            (cond
              (> left-length 0) (arrays/aget arr cut-from)
              (> values-length 0) (arrays/aget values 0)
              :else (arrays/aget arr splice-to))
            result (Array.make result-length initial)]
        (arrays/acopy arr cut-from splice-from result 0)
        (arrays/acopy values 0 values-length result left-length)
        (arrays/acopy arr splice-to cut-to result values-end)
        result))))

(defn splice [arr splice-from splice-to values]
  (cut-n-splice arr 0 (arrays/alength arr) splice-from splice-to values))

(defn insert [arr idx values]
  (cut-n-splice arr 0 (arrays/alength arr) idx idx values))

(defn merge-n-split [left right]
  (let [left-length (arrays/alength left)
        right-length (arrays/alength right)
        total-length (+ left-length right-length)
        result-left-length (arrays/half total-length)
        result-right-length (- total-length result-left-length)
        combined-at
        (fn [idx]
          (if (< idx left-length)
            (arrays/aget left idx)
            (arrays/aget right (- idx left-length))))
        result-left (Array.make result-left-length (combined-at 0))
        result-right
        (Array.make
          result-right-length
          (combined-at result-left-length))]
    (if (<= left-length result-left-length)
      (do
        (arrays/acopy left 0 left-length result-left 0)
        (arrays/acopy
          right 0 (- result-left-length left-length) result-left left-length)
        (arrays/acopy
          right (- result-left-length left-length) right-length result-right 0))
      (do
        (arrays/acopy left 0 result-left-length result-left 0)
        (arrays/acopy left result-left-length left-length result-right 0)
        (arrays/acopy
          right 0 right-length result-right (- left-length result-left-length))))
    (arrays/array result-left result-right)))

(defn eq-arr [cmp left left-from left-to right right-from right-to]
  (let [length (- left-to left-from)]
    (and
      (= length (- right-to right-from))
      (loop [idx 0]
        (cond
          (= idx length)
          true

          (not (= 0
                  #?(:melange
                     (uncurried-compare
                       cmp
                       (arrays/aget left (+ idx left-from))
                       (arrays/aget right (+ idx right-from)))
                     :default
                     (cmp
                       (arrays/aget left (+ idx left-from))
                       (arrays/aget right (+ idx right-from))))))
          false

          :else
          (recur (inc idx)))))))

(defn check-n-splice [cmp arr from to new-arr]
  (if (eq-arr cmp arr from to new-arr 0 (arrays/alength new-arr))
    arr
    (splice arr from to new-arr)))
