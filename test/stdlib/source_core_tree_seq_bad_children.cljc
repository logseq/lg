(ns source-core-tree-seq-bad-children)

(tree-seq (fn [_value] true) inc 1)
