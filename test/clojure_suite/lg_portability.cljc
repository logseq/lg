(ns clojure.core-test.portability)

(defmacro when-var-exists [_var-sym & body]
  `(do ~@body))

(defmacro thrown? [& body]
  `(try
     ~@body
     false
     (catch js/Error _error#
       true)))
