(ns source-core-merge-with-bad-combiner)

(merge-with (fn [left right] (str left right)) {:value 1} {:value 2})
