(ffi floor [:float] :float {:js "floor" :scope ["Math"]})
(ffi basename [:string] :string {:js "basename" :module "node:path"})
(ffi number-string [:float] :string {:js "String"})
(ffi log [:string] :unit {:js "log" :scope ["console"]})

(log (basename "/tmp/example.txt"))
(log (number-string (floor 3.8)))
