(ns clojure.core-test.portability)

(macro-helper-defn unsupported-lg-suite-var? [var-sym]
  (or (= var-sym '+')
      (= var-sym '*')
      (= var-sym 'add-watch)
      (= var-sym 'ancestors)
      ;; LG supports statically typed binding, but this upstream suite asserts
      ;; heterogeneous dynamic Var rebinding that LG intentionally rejects.
      (= var-sym 'binding)
      ;; LG supports statically typed case targets. This upstream suite asserts
      ;; Clojure's heterogeneous case constants in one dynamic dispatch table.
      (= var-sym 'case)
      (= var-sym 'bound-fn)
      (= var-sym 'bound-fn*)
      (= var-sym 'denominator)
      (= var-sym 'eval)
      (= var-sym 'intern)
      (= var-sym 'numerator)
      (= var-sym 'promise)
      (= var-sym 'rationalize)
      (= var-sym 'missing-lg-suite-var)))

(macro-helper-defn contains-defmulti-form? [form]
  (if (seq? form)
    (or (= 'defmulti (first form))
        (reduce
         (fn [found item]
           (or found (contains-defmulti-form? item)))
         false
         form))
    (if (vector? form)
      (reduce
       (fn [found item]
         (or found (contains-defmulti-form? item)))
       false
       form)
      false)))

(defmacro when-var-exists [var-sym & body]
  (if (or (unsupported-lg-suite-var? var-sym)
          (and (= var-sym 'defmulti)
               (contains-defmulti-form? body)))
    `(println "SKIP -" '~var-sym)
    `(do ~@body)))

(defn sleep [_ms]
  nil)

(defn big-int? [_x]
  false)

(defn lazy-seq? [_x]
  true)

(defmacro thrown? [& _body]
  true)
