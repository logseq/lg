(ns datascript.inline
  (:refer-clojure :exclude [update]))

(defmacro update [m k f & more]
  `(let [m# ~m
         k# ~k]
     (assoc m# k# (~f (get m# k#) ~@more))))
