(ns cognitect.transit)

(defn writer
  ([_type]
   nil)
  ([_stream _type]
   nil))

(defn write [_writer value]
  value)

(defn reader
  ([_type]
   nil)
  ([_stream _type]
   nil))

(defn read
  ([_reader]
   nil)
  ([_reader value]
   value))
