(ns clojure.core-test.portability)

(macro-helper-defn unsupported-lg-suite-var? [var-sym]
  (or (= var-sym 'bound-fn)
      (= var-sym 'bound-fn*)
      (= var-sym 'denominator)
      (= var-sym 'eval)
      (= var-sym 'intern)
      (= var-sym 'numerator)
      (= var-sym 'promise)
      (= var-sym 'rationalize)
      (= var-sym 'missing-lg-suite-var)))

(defmacro when-var-exists [var-sym & body]
  (if (unsupported-lg-suite-var? var-sym)
    `(println "SKIP -" '~var-sym)
    `(do ~@body)))

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
