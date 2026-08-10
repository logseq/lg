(ns clojure.zip)

(type-record zip-context [node]
  (zip-branch-fn :fn<node;bool>)
  (zip-children-fn :fn<node;vector<node>>)
  (zip-make-node-fn :fn<node;vector<node>;node>))

(type-record zip-path [node]
  (zip-left :vector<node>)
  (zip-parent-nodes :vector<node>)
  (zip-parent-path :option<zip-path<node>>)
  (zip-right :vector<node>)
  (zip-changed :bool))

(type-record location [node]
  (zip-current :node)
  (zip-path-value :option<zip-path<node>>)
  (zip-context-value :zip-context<node>)
  (zip-at-end :bool))

(signature clojure.zip/zipper [node]
  :fn<fn<node;bool>;fn<node;seqable<node>>;fn<node;vector<node>;node>;node;location<node>>)
(signature clojure.zip/seq-zip
  :fn<Lg_edn_backend.t;location<Lg_edn_backend.t>>)
(signature clojure.zip/vector-zip
  :fn<Lg_edn_backend.t;location<Lg_edn_backend.t>>)
(signature clojure.zip/xml-zip
  :fn<Lg_edn_backend.t;location<Lg_edn_backend.t>>)
(signature clojure.zip/node [node] :fn<location<node>;node>)
(signature clojure.zip/branch? [node] :fn<location<node>;bool>)
(signature clojure.zip/children [node] :fn<location<node>;vector<node>>)
(signature clojure.zip/make-node [node]
  :fn<location<node>;node;seqable<node>;node>)
(signature clojure.zip/path [node] :fn<location<node>;vector<node>>)
(signature clojure.zip/lefts [node] :fn<location<node>;vector<node>>)
(signature clojure.zip/rights [node] :fn<location<node>;vector<node>>)
(signature clojure.zip/down [node]
  :fn<location<node>;option<location<node>>>)
(signature clojure.zip/up [node]
  :fn<location<node>;option<location<node>>>)
(signature clojure.zip/root [node] :fn<location<node>;node>)
(signature clojure.zip/right [node]
  :fn<location<node>;option<location<node>>>)
(signature clojure.zip/rightmost [node] :fn<location<node>;location<node>>)
(signature clojure.zip/left [node]
  :fn<location<node>;option<location<node>>>)
(signature clojure.zip/leftmost [node] :fn<location<node>;location<node>>)
(signature clojure.zip/insert-left [node]
  :fn<location<node>;node;location<node>>)
(signature clojure.zip/insert-right [node]
  :fn<location<node>;node;location<node>>)
(signature clojure.zip/replace [node]
  :fn<location<node>;node;location<node>>)
(signature clojure.zip/edit [node]
  :fn<location<node>;fn<node;node>;location<node>>)
(signature clojure.zip/insert-child [node]
  :fn<location<node>;node;location<node>>)
(signature clojure.zip/append-child [node]
  :fn<location<node>;node;location<node>>)
(signature clojure.zip/next [node] :fn<location<node>;location<node>>)
(signature clojure.zip/prev [node]
  :fn<location<node>;option<location<node>>>)
(signature clojure.zip/end? [node] :fn<location<node>;bool>)
(signature clojure.zip/remove [node] :fn<location<node>;location<node>>)
