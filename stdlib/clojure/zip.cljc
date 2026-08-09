; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port preserves ClojureScript's zipper navigation and edit order.

(ns clojure.zip
  (:refer-clojure :exclude [replace remove next])
  (:require [ocaml.Lg_runtime.Runtime_zip :as runtime]))

(defn- location-value [current path-value context at-end]
  (record location
    (zip-current current)
    (zip-path-value path-value)
    (zip-context-value context)
    (zip-at-end at-end)))

(defn zipper [branch-fn children-fn make-node-fn root]
  (let [context
        (record zip-context
          (zip-branch-fn branch-fn)
          (zip-children-fn (fn [value] (vec (children-fn value))))
          (zip-make-node-fn
           (fn [value child-values]
             (make-node-fn value child-values))))]
    (location-value root None context false)))

(defn seq-zip [root]
  (zipper (fn [value] (runtime/is-sequential value))
          (fn [value] (runtime/sequence-children value))
          (fn [value child-values]
            (runtime/make-sequence-node value child-values))
          root))

(defn vector-zip [root]
  (zipper (fn [value] (runtime/is-vector value))
          (fn [value] (runtime/sequence-children value))
          (fn [value child-values]
            (runtime/make-vector-node value child-values))
          root))

(defn xml-zip [root]
  (zipper (fn [value] (runtime/is-xml-branch value))
          (fn [value] (runtime/xml-children value))
          (fn [value child-values]
            (runtime/make-xml-node value child-values))
          root))

(defn node [loc] (:zip-current loc))

(defn branch? [loc]
  (let [context (:zip-context-value loc)
        branch-fn (:zip-branch-fn context)]
    (branch-fn (node loc))))

(defn children [loc]
  (if (branch? loc)
    (let [context (:zip-context-value loc)
          children-fn (:zip-children-fn context)]
      (children-fn (node loc)))
    (raise (Failure "called children on a leaf node"))))

(defn make-node [loc value child-values]
  (let [context (:zip-context-value loc)
        make-node-fn (:zip-make-node-fn context)]
    (make-node-fn value (vec child-values))))

(defn path [loc]
  (match (:zip-path-value loc)
    (Some value) (:zip-parent-nodes value)
    None []))

(defn lefts [loc]
  (match (:zip-path-value loc)
    (Some value) (:zip-left value)
    None []))

(defn rights [loc]
  (match (:zip-path-value loc)
    (Some value) (:zip-right value)
    None []))

(defn down [loc]
  (if (branch? loc)
    (let [child-values (children loc)]
      (if (pos? (count child-values))
        (let [child (nth child-values 0)
              remaining (vec (rest child-values))
              old-path (:zip-path-value loc)
              parent-nodes
              (match old-path
                (Some value) (conj (:zip-parent-nodes value) (node loc))
                None [(node loc)])
              new-path
              (record zip-path
                (zip-left [])
                (zip-parent-nodes parent-nodes)
                (zip-parent-path old-path)
                (zip-right remaining)
                (zip-changed false))]
          (Some (location-value child (Some new-path) (:zip-context-value loc) false)))
        None))
    None))

(defn up [loc]
  (match (:zip-path-value loc)
    None None
    (Some value)
    (let [parent-nodes (:zip-parent-nodes value)
          left-values (:zip-left value)
          right-values (:zip-right value)]
      (let [parent (nth parent-nodes (dec (count parent-nodes)))
            parent-value
            (if (:zip-changed value)
              (make-node loc parent
                         (concat left-values
                                 (cons (node loc) right-values)))
              parent)
            parent-path
            (match (:zip-parent-path value)
              (Some parent-state)
              (Some
               (record zip-path
                 (zip-left (:zip-left parent-state))
                 (zip-parent-nodes (:zip-parent-nodes parent-state))
                 (zip-parent-path (:zip-parent-path parent-state))
                 (zip-right (:zip-right parent-state))
                 (zip-changed (or (:zip-changed parent-state) (:zip-changed value)))))
              None None)]
        (Some (location-value parent-value parent-path (:zip-context-value loc) false))))))

(defn root [loc]
  (loop [current loc]
    (if (:zip-at-end current)
      (node current)
      (if-some [parent (up current)]
        (recur parent)
        (node current)))))

(defn- replace-path [loc value left-values right-values changed]
  (location-value
   value
   (match (:zip-path-value loc)
     (Some path-value)
     (Some
      (record zip-path
        (zip-left left-values)
        (zip-parent-nodes (:zip-parent-nodes path-value))
        (zip-parent-path (:zip-parent-path path-value))
        (zip-right right-values)
        (zip-changed changed)))
     None None)
   (:zip-context-value loc) false))

(defn right [loc]
  (match (:zip-path-value loc)
    None None
    (Some value)
    (let [right-values (:zip-right value)
          left-values (:zip-left value)]
      (if (pos? (count right-values))
        (Some
         (replace-path loc (nth right-values 0)
                       (conj left-values (node loc))
                       (vec (rest right-values)) (:zip-changed value)))
        None))))

(defn rightmost [loc]
  (match (:zip-path-value loc)
    None loc
    (Some value)
    (let [stored-right (:zip-right value)
          left-values (:zip-left value)]
      (if (pos? (count stored-right))
        (let [right-values (vec stored-right)]
          (replace-path loc (nth right-values (dec (count right-values)))
                        (into (conj left-values (node loc))
                              (butlast right-values))
                        [] (:zip-changed value)))
        loc))))

(defn left [loc]
  (match (:zip-path-value loc)
    None None
    (Some value)
    (let [left-values (:zip-left value)
          right-values (:zip-right value)]
      (if (pos? (count left-values))
        (Some
         (replace-path loc (nth left-values (dec (count left-values)))
                       (vec (butlast left-values))
                       (vec (cons (node loc) right-values))
                       (:zip-changed value)))
        None))))

(defn leftmost [loc]
  (match (:zip-path-value loc)
    None loc
    (Some value)
    (let [left-values (:zip-left value)
          right-values (:zip-right value)]
      (if (pos? (count left-values))
        (replace-path
         loc (nth left-values 0) []
         (vec (concat (rest left-values) [(node loc)] right-values))
         (:zip-changed value))
        loc))))

(defn insert-left [loc item]
  (match (:zip-path-value loc)
    None (raise (Failure "Insert at top"))
    (Some value)
    (replace-path loc (node loc) (conj (:zip-left value) item)
                  (:zip-right value) true)))

(defn insert-right [loc item]
  (match (:zip-path-value loc)
    None (raise (Failure "Insert at top"))
    (Some value)
    (replace-path loc (node loc) (:zip-left value)
                  (vec (cons item (:zip-right value))) true)))

(defn replace [loc value]
  (replace-path loc value (lefts loc) (rights loc) true))

(defn edit [loc f]
  (replace loc (f (node loc))))

(defn insert-child [loc item]
  (replace loc (make-node loc (node loc) (cons item (children loc)))))

(defn append-child [loc item]
  (replace loc (make-node loc (node loc) (concat (children loc) [item]))))

(defn next [loc]
  (if (:zip-at-end loc)
    loc
    (match (down loc)
      (Some child) child
      None
      (match (right loc)
        (Some sibling) sibling
        None
        (loop [parent loc]
          (if-some [ancestor (up parent)]
            (if-some [sibling (right ancestor)]
              sibling
              (recur ancestor))
            (location-value (node parent) None (:zip-context-value loc) true)))))))

(defn prev [loc]
  (match (left loc)
    (Some sibling)
    (loop [value sibling]
      (if-some [child (down value)]
        (recur (rightmost child))
        value))
    None (up loc)))

(defn end? [loc] (:zip-at-end loc))

(defn remove [loc]
  (match (:zip-path-value loc)
    None (raise (Failure "Remove at top"))
    (Some value)
    (let [left-values (:zip-left value)
          right-values (:zip-right value)
          parent-nodes (:zip-parent-nodes value)
          parent-path (:zip-parent-path value)
          context (:zip-context-value loc)]
      (if (pos? (count left-values))
        (loop [previous
               (replace-path loc
                             (nth left-values (dec (count left-values)))
                             (vec (butlast left-values))
                             right-values true)]
          (if-some [child (down previous)]
            (recur (rightmost child))
            previous))
        (let [parent (nth parent-nodes (dec (count parent-nodes)))
              rebuilt (make-node loc parent right-values)]
          (location-value rebuilt parent-path context false))))))
