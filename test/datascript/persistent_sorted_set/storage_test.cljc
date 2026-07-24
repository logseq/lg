(ns me.tonsky.persistent-sorted-set.test.storage
  (:require
   [me.tonsky.persistent-sorted-set.arrays :as arrays]
   [me.tonsky.persistent-sorted-set :as pss]))

(defn int-compare [left right]
  (compare left right))

(defn make-memory-storage []
  (let [disk (arrays/make-array 512 None)
        next-address (atom 1)
        reads (atom 0)
        writes (atom 0)
        accessed (atom 0)
        deleted (atom 0)
        storage
        (pss/make-storage
         (fn [address]
           (reset! reads (inc (deref reads)))
           (arrays/aget disk address))
         (fn [_address]
           (Stdlib.ignore (reset! accessed (inc (deref accessed)))))
         (fn [node previous-address]
           (let [address
                 (if-some [existing previous-address]
                   existing
                   (let [fresh (deref next-address)]
                     (reset! next-address (inc fresh))
                     fresh))
                 snapshot
                 (if (= 0 (pss/node-child-count node))
                   (pss/new-restored-leaf
                    (arrays/aclone (pss/node-keys node))
                    address)
                   (pss/new-restored-node
                    (arrays/aclone (pss/node-keys node))
                    (arrays/aclone (pss/node-addresses node))
                    address))]
             (arrays/aset disk address (Some snapshot))
             (reset! writes (inc (deref writes)))
             address))
         (fn [addresses]
           (Stdlib.ignore
            (reset!
             deleted
             (+ (deref deleted) (arrays/alength addresses))))))]
    (tuple storage reads writes accessed deleted)))

(defn range-set [^:int size]
  (pss/from-sorted-array
   int-compare
   (arrays/into-array (range 0 size))))

(defn addressed? [address]
  (match address
    (Some _value) true
    None false))

(def clear-tree-reference!
  (fn [^:weak<tree<int>> reference]
    (weak-clear! reference)))

(defn all-addressed? [addresses]
  (loop [idx 0]
    (if (= idx (arrays/alength addresses))
      true
      (if (addressed? (arrays/aget addresses idx))
        (recur (inc idx))
        false))))

(let [children-33 (pss/node-children (pss/set-root (range-set 33)))
      children-65 (pss/node-children (pss/set-root (range-set 65)))]
  (println
   (str "partition:"
        (= 16 (pss/node-len (arrays/aget children-33 0))) ":"
        (= 17 (pss/node-len (arrays/aget children-33 1))) ":"
        (= 24 (pss/node-len (arrays/aget children-65 0))) ":"
        (= 24 (pss/node-len (arrays/aget children-65 1))) ":"
        (= 17 (pss/node-len (arrays/aget children-65 2))))))

(match (make-memory-storage)
  (tuple storage reads writes accessed deleted)
  (let [original (range-set 100)
        root-address (pss/store original storage)
        first-write-count (deref writes)
        repeated-address (pss/store original storage)
        restored (pss/restore-by int-compare root-address storage 1 100)]
    (println
     (str "store:"
          (= 5 first-write-count) ":"
          (= root-address repeated-address) ":"
          (= first-write-count (deref writes))))
    (println
     (str "restore-lazy:"
          (= 0 (deref reads)) ":"
          (= 100 (pss/set-count restored))))
    (println
     (str "lookup-cache:"
          (= (Some 42) (pss/set-lookup restored 42)) ":"
          (= 2 (deref reads)) ":"
          (= (Some 42) (pss/set-lookup restored 42)) ":"
          (= 2 (deref reads)) ":"
          (= 1 (deref accessed))))
    (println
     (str "walk:"
          (= 4950 (pss/set-reduce restored (fn [left right] (+ left right)) 0)) ":"
          (= 5 (deref reads)) ":"
          (= 4950 (pss/set-reduce restored (fn [left right] (+ left right)) 0)) ":"
          (= 5 (deref reads))))
    (let [updated (pss/set-conj restored 100)
          writes-before (deref writes)]
      (pss/store updated storage)
      (println
       (str "incremental:"
            (= 2 (- (deref writes) writes-before)) ":"
            (= (Some 100) (pss/set-lookup updated 100)) ":"
            (nil? (pss/set-lookup restored 100)) ":"
            (= 0 (deref deleted)))))))

(match (make-memory-storage)
  (tuple storage _reads _writes _accessed _deleted)
  (let [original (range-set 100)
        _root-address (pss/store original storage)
        updated (pss/set-conj original 100)]
    (println
     (str "addresses-preserved:"
          (all-addressed?
           (pss/node-addresses (pss/set-root updated)))))))

(match (make-memory-storage)
  (tuple storage _reads _writes _accessed deleted)
  (let [original (range-set 33)
        _root-address (pss/store original storage)
        updated (pss/set-disj original 16)]
    (println
     (str "delete-removed:"
          (= 2 (deref deleted)) ":"
          (nil? (pss/set-lookup updated 16)) ":"
          (= (Some 16) (pss/set-lookup original 16))))))

(match (make-memory-storage)
  (tuple storage _reads _writes _accessed deleted)
  (let [original (range-set 33)
        _root-address (pss/store original storage)
        updated (pss/set-disj original 0)]
    (println
     (str "store-attaches:"
          (= 0 (deref deleted)) ":"
          (nil? (pss/set-lookup updated 0)) ":"
          (= (Some 0) (pss/set-lookup original 0))))))

(match (make-memory-storage)
  (tuple storage _reads _writes _accessed deleted)
  (let [original (range-set 33)
        root-address (pss/store original storage)
        restored (pss/restore-by int-compare root-address storage 1 33)
        updated (pss/set-disj restored 0)]
    (println
     (str "delete:"
          (= 0 (deref deleted)) ":"
          (nil? (pss/set-lookup updated 0)) ":"
          (= (Some 0) (pss/set-lookup restored 0))))))

(match (make-memory-storage)
  (tuple storage reads writes _accessed _deleted)
  (let [empty (pss/empty-set int-compare)
        root-address (pss/store empty storage)
        restored (pss/restore-by int-compare root-address storage 0 0)]
    (println
     (str "empty:"
          (= 1 (deref writes)) ":"
          (= 0 (deref reads)) ":"
          (= 0 (pss/set-count restored)) ":"
          (nil? (pss/set-lookup restored 0)) ":"
          (= 1 (deref reads))))))

(match (make-memory-storage)
  (tuple storage reads _writes _accessed _deleted)
  (let [original
        (pss/with-ref-type
         (range-set 100)
         (Lg_runtime.Runtime_ref_type.Strong))
        root-address (pss/store original storage)
        restored (pss/restore-by
                  int-compare root-address storage 1 100
                  (Lg_runtime.Runtime_ref_type.Strong))
        root (pss/set-root restored)
        first-child
        (pss/node-child root 0 (pss/set-storage restored))
        reads-after-first-child (deref reads)
        second-child
        (pss/node-child root 0 (pss/set-storage restored))
        second-read-cached (= reads-after-first-child (deref reads))
        updated (pss/set-conj restored 100)]
    (println
     (str "strong-cache:"
          (= (Some first-child) (arrays/aget (:children root) 0)) ":"
          (nil? (arrays/aget (:_weak-children root) 0)) ":"
          (= first-child second-child) ":"
          second-read-cached ":"
          (= (Lg_runtime.Runtime_ref_type.Strong)
             (pss/set-ref-type updated))))))

(match (make-memory-storage)
  (tuple storage reads _writes _accessed _deleted)
  (let [original (range-set 100)
        root-address (pss/store original storage)
        restored (pss/restore-by
                  int-compare root-address storage 1 100
                  (Lg_runtime.Runtime_ref_type.Weak))
        root (pss/set-root restored)
        first-child (pss/node-child root 0 (Some storage))]
    (match (arrays/aget (:_weak-children root) 0)
      (Some child-reference)
      (clear-tree-reference! child-reference)
      None
      (Stdlib.failwith "restored child cache is missing"))
    (let [restored-child (pss/node-child root 0 (Some storage))]
      (match (deref (:_weak-root restored))
        (Some root-reference)
        (clear-tree-reference! root-reference)
        None
        (Stdlib.failwith "restored root cache is missing"))
      (let [restored-root (pss/set-root restored)]
        (println
         (str "weak-cache:"
              (= (Some 0)
                 (pss/node-lookup first-child int-compare 0 None)) ":"
              (= (Some 0)
                 (pss/node-lookup restored-child int-compare 0 None)) ":"
              (= (Some 99)
                 (pss/node-lookup
                  restored-root int-compare 99 (Some storage))) ":"
              (= 5 (deref reads))))))))
