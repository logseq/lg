(ns source-core-merge-with-bad-key-type)

(merge-with (fn [left right] (+ left right)) {:value 1} {"value" 2})
