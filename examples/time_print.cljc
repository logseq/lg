(def output
  (with-out-str
    (let [result (time (+ 20 22))]
      (print "Result: ")
      (println result)
      (prn "readable" result))))

(print output)
