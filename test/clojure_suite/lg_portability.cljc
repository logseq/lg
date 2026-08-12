(ns clojure.core-test.portability)

(defmacro when-var-exists [_var-sym & body]
  `(do ~@body))

(defn sleep [_ms]
  nil)

(defn lazy-seq? [_x]
  true)

(defmacro thrown? [& body]
  `(try
     ~@body
     false
     (catch js/Error _error#
       true)))
