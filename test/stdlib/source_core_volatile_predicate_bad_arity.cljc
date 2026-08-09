(ns source-core-volatile-predicate-bad-arity)

(volatile? (volatile! 1) :extra)
