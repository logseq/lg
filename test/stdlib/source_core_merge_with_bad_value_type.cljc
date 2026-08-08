(ns source-core-merge-with-bad-value-type)

(merge-with (fn [left _right] left) {:value 1} {:value "two"})
