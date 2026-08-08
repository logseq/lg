(ns clojure.set)

; LG keeps the public implementation in source. These helper signatures expose
; the static set element relation that ClojureScript obtains from dynamic sets.
(signature clojure.set/bubble-max-key [value]
  :fn<fn<set<value>;int>;seq<set<value>>;seq<set<value>>>)

(signature clojure.set/union-two [value]
  :fn<set<value>;set<value>;set<value>>)

(signature clojure.set/intersection-two [value]
  :fn<set<value>;set<value>;set<value>>)

(signature clojure.set/difference-two [value]
  :fn<set<value>;set<value>;set<value>>)

(signature clojure.set/subset? [value]
  :fn<set<value>;set<value>;bool>)

(signature clojure.set/superset? [value]
  :fn<set<value>;set<value>;bool>)

(signature clojure.set/select [value result]
  :fn<fn<value;truthy<result>>;set<value>;set<value>>)

(signature clojure.set/map-invert [key value]
  :fn<map<key;value>;map<value;key>>)

(signature clojure.set/remove-renamed-keys [key value]
  :fn<map<key;value>;map<key;key>;map<key;value>>)

(signature clojure.set/rename-keys [key value]
  :fn<map<key;value>;map<key;key>;map<key;value>>)
