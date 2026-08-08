(ns stdlib.aggregate-consumer
  (:require [stdlib.aggregate-provider :as provider]))

(println (provider/label "logseq"))
