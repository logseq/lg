(ns me.tonsky.persistent-sorted-set.test.leaf
  (:require
   [me.tonsky.persistent-sorted-set.arrays :as arrays]
   [me.tonsky.persistent-sorted-set :as pss]))

(defn int-compare [^:int left ^:int right]
  (compare left right))

(def leaf (pss/new-leaf (arrays/array 1 3 5)))
(println
 (str "lookup:"
      (= 3 (pss/node-len leaf)) ":"
      (= 5 (pss/node-lim-key leaf)) ":"
      (= (Some 3) (pss/node-lookup leaf int-compare 3 None)) ":"
      (= None (pss/node-lookup leaf int-compare 4 None))))

(if-some [inserted-nodes (pss/node-conj leaf int-compare 4 None)]
  (let [inserted (arrays/aget inserted-nodes 0)]
    (println
     (str "insert:"
          (= 1 (arrays/alength inserted-nodes)) ":"
          (= 4 (pss/node-len inserted)) ":"
          (= (Some 4) (pss/node-lookup inserted int-compare 4 None)) ":"
          (nil? (pss/node-conj inserted int-compare 4 None)))))
  (println "insert:false:false:false:false"))

(def full-leaf (pss/new-leaf (arrays/into-array (range 0 32))))
(if-some [split-nodes (pss/node-conj full-leaf int-compare 40 None)]
  (let [split-left (arrays/aget split-nodes 0)
        split-right (arrays/aget split-nodes 1)
        root (pss/new-node
              (arrays/amap (fn [node] (pss/node-lim-key node)) split-nodes)
              split-nodes)]
    (println
     (str "split:"
          (= 2 (arrays/alength split-nodes)) ":"
          (= 16 (pss/node-len split-left)) ":"
          (= 17 (pss/node-len split-right)) ":"
          (= 15 (pss/node-lim-key split-left)) ":"
          (= 40 (pss/node-lim-key split-right)) ":"
          (= (Some 40) (pss/node-lookup split-right int-compare 40 None))))
    (if-some [updated-roots (pss/node-conj root int-compare 41 None)]
      (let [updated-root (arrays/aget updated-roots 0)]
        (println
         (str "branch:"
              (= 2 (pss/node-len root)) ":"
              (= (Some 1) (pss/node-lookup root int-compare 1 None)) ":"
              (= (Some 40) (pss/node-lookup root int-compare 40 None)) ":"
              (= (Some 41) (pss/node-lookup updated-root int-compare 41 None)))))
      (println "branch:false:false:false:false")))
  (println "split:false:false:false:false:false:false"))

(def full-children
  (arrays/amap
   (fn [group]
     (pss/new-leaf
      (arrays/into-array
       (range (* group 32) (* (inc group) 32)))))
   (arrays/into-array (range 0 32))))
(def wide-root
  (pss/new-node
   (arrays/amap (fn [node] (pss/node-lim-key node)) full-children)
   full-children))
(if-some [wide-roots (pss/node-conj wide-root int-compare 2000 None)]
  (let [wide-left (arrays/aget wide-roots 0)
        wide-right (arrays/aget wide-roots 1)]
    (println
     (str "wide:"
          (= 2 (arrays/alength wide-roots)) ":"
          (= 16 (pss/node-len wide-left)) ":"
          (= 17 (pss/node-len wide-right)) ":"
          (= (Some 0) (pss/node-lookup wide-left int-compare 0 None)) ":"
          (= (Some 2000) (pss/node-lookup wide-right int-compare 2000 None)))))
  (println "wide:false:false:false:false:false"))

(def delete-root (pss/new-leaf (arrays/into-array (range 0 20))))
(if-some [deleted-root (pss/node-disj delete-root int-compare 10 true None None None)]
  (let [result (arrays/aget deleted-root 0)]
    (println
     (str "delete-root:"
          (= 1 (arrays/alength deleted-root)) ":"
          (= 19 (pss/node-len result)) ":"
          (= None (pss/node-lookup result int-compare 10 None)) ":"
          (= (Some 10) (pss/node-lookup delete-root int-compare 10 None)) ":"
          (nil? (pss/node-disj result int-compare 100 true None None None)))))
  (println "delete-root:false:false:false:false:false"))

(def merge-left (pss/new-leaf (arrays/into-array (range 0 16))))
(def merge-node (pss/new-leaf (arrays/into-array (range 16 33))))
(if-some [merged
          (pss/node-disj
           merge-node int-compare 16 false (Some merge-left) None None)]
  (let [result (arrays/aget merged 0)]
    (println
     (str "delete-merge:"
          (= 1 (arrays/alength merged)) ":"
          (= 32 (pss/node-len result)) ":"
          (= (Some 0) (pss/node-lookup result int-compare 0 None)) ":"
          (= (Some 32) (pss/node-lookup result int-compare 32 None)))))
  (println "delete-merge:false:false:false:false"))

(def redistribute-left (pss/new-leaf (arrays/into-array (range 0 17))))
(def redistribute-node (pss/new-leaf (arrays/into-array (range 17 34))))
(if-some [redistributed
          (pss/node-disj
           redistribute-node
           int-compare
           17
           false
           (Some redistribute-left)
           None
           None)]
  (println
   (str "delete-redistribute:"
        (= 2 (arrays/alength redistributed)) ":"
        (= 16 (pss/node-len (arrays/aget redistributed 0))) ":"
        (= 17 (pss/node-len (arrays/aget redistributed 1))) ":"
        (= (Some 33) (pss/node-lookup (arrays/aget redistributed 1) int-compare 33 None))))
  (println "delete-redistribute:false:false:false:false"))

(def merge-right-node (pss/new-leaf (arrays/into-array (range 0 17))))
(def merge-right (pss/new-leaf (arrays/into-array (range 17 33))))
(if-some [merged
          (pss/node-disj
           merge-right-node int-compare 16 false None (Some merge-right) None)]
  (let [result (arrays/aget merged 0)]
    (println
     (str "delete-merge-right:"
          (= 1 (arrays/alength merged)) ":"
          (= 32 (pss/node-len result)) ":"
          (= (Some 0) (pss/node-lookup result int-compare 0 None)) ":"
          (= (Some 32) (pss/node-lookup result int-compare 32 None)))))
  (println "delete-merge-right:false:false:false:false"))

(def redistribute-right-node (pss/new-leaf (arrays/into-array (range 0 17))))
(def redistribute-right (pss/new-leaf (arrays/into-array (range 17 34))))
(if-some [redistributed
          (pss/node-disj
           redistribute-right-node
           int-compare
           16
           false
           None
           (Some redistribute-right)
           None)]
  (println
   (str "delete-redistribute-right:"
        (= 2 (arrays/alength redistributed)) ":"
        (= 16 (pss/node-len (arrays/aget redistributed 0))) ":"
        (= 17 (pss/node-len (arrays/aget redistributed 1))) ":"
        (= (Some 33) (pss/node-lookup (arrays/aget redistributed 1) int-compare 33 None))))
  (println "delete-redistribute-right:false:false:false:false"))

(def stable-left (pss/new-leaf (arrays/into-array (range 0 17))))
(def stable-node (pss/new-leaf (arrays/into-array (range 17 35))))
(def stable-right (pss/new-leaf (arrays/into-array (range 35 52))))
(if-some [stable
          (pss/node-disj
           stable-node
           int-compare
           17
           false
           (Some stable-left)
           (Some stable-right)
           None)]
  (println
   (str "delete-stable:"
        (= 3 (arrays/alength stable)) ":"
        (= 17 (pss/node-len (arrays/aget stable 0))) ":"
        (= 17 (pss/node-len (arrays/aget stable 1))) ":"
        (= 17 (pss/node-len (arrays/aget stable 2)))))
  (println "delete-stable:false:false:false:false"))

(def delete-branch-left (pss/new-leaf (arrays/into-array (range 0 16))))
(def delete-branch-right (pss/new-leaf (arrays/into-array (range 16 32))))
(def delete-branch
  (pss/new-node
   (arrays/array
    (pss/node-lim-key delete-branch-left)
    (pss/node-lim-key delete-branch-right))
   (arrays/array delete-branch-left delete-branch-right)))
(if-some [deleted-branch
          (pss/node-disj delete-branch int-compare 20 true None None None)]
  (let [result (arrays/aget deleted-branch 0)]
    (println
     (str "delete-branch:"
          (= 1 (arrays/alength deleted-branch)) ":"
          (= 1 (pss/node-len result)) ":"
          (= (Some 0) (pss/node-lookup result int-compare 0 None)) ":"
          (= (Some 31) (pss/node-lookup result int-compare 31 None)) ":"
          (= None (pss/node-lookup result int-compare 20 None)))))
  (println "delete-branch:false:false:false:false:false"))

(def traversal-left (pss/new-leaf (arrays/into-array (range 0 4))))
(def traversal-middle (pss/new-leaf (arrays/into-array (range 4 7))))
(def traversal-right (pss/new-leaf (arrays/into-array (range 7 12))))
(def traversal-root
  (pss/new-node
   (arrays/array 3 6 11)
   (arrays/array traversal-left traversal-middle traversal-right)))
(def left-last
  (pss/path-set (pss/path-set pss/empty-path 1 0) 0 3))
(def middle-first
  (pss/path-set (pss/path-set pss/empty-path 1 1) 0 0))
(def middle-key
  (pss/path-set (pss/path-set pss/empty-path 1 1) 0 2))
(println
 (str "traversal:"
      (= 6 (pss/node-value-at-path traversal-root middle-key 1 None)) ":"
      (= 11
         (pss/node-value-at-path
          traversal-root
          (pss/rightmost-path traversal-root pss/empty-path 1 None)
          1 None)) ":"
      (= 4
         (pss/node-value-at-path
          traversal-root
          (pss/next-path traversal-root left-last 1 None)
          1 None)) ":"
      (= 3
         (pss/node-value-at-path
          traversal-root
          (pss/prev-path traversal-root middle-first 1 None)
          1 None)) ":"
      (pss/path-eq
       (pss/path-dec pss/empty-path)
       (pss/prev-path traversal-root pss/empty-path 1 None))))
(if-some [seek-five (pss/seek-path traversal-root 5 int-compare 1 None)]
  (println
   (str "seek:"
        (= 5 (pss/node-value-at-path traversal-root seek-five 1 None)) ":"
        (= None (pss/seek-path traversal-root 100 int-compare 1 None)) ":"
        (= 6
           (pss/node-value-at-path
            traversal-root
            (pss/rseek-path traversal-root 5 int-compare 1 None)
            1 None)) ":"
        (pss/path-eq
         (pss/next-path
          traversal-root
          (pss/rightmost-path traversal-root pss/empty-path 1 None)
          1 None)
         (pss/rseek-path traversal-root 100 int-compare 1 None))))
  (println "seek:false:false:false:false"))

(def traversal-deep-left (pss/new-leaf (arrays/into-array (range 12 16))))
(def traversal-deep-middle (pss/new-leaf (arrays/into-array (range 16 19))))
(def traversal-deep-right (pss/new-leaf (arrays/into-array (range 19 24))))
(def traversal-deep-branch
  (pss/new-node
   (arrays/array 15 18 23)
   (arrays/array
    traversal-deep-left traversal-deep-middle traversal-deep-right)))
(def traversal-deep-root
  (pss/new-node
   (arrays/array 11 23)
   (arrays/array traversal-root traversal-deep-branch)))
(def traversal-eleven
  (pss/path-set
   (pss/path-set (pss/path-set pss/empty-path 2 0) 1 2)
   0
   4))
(def traversal-twelve
  (pss/path-set
   (pss/path-set (pss/path-set pss/empty-path 2 1) 1 0)
   0
   0))
(println
 (str "traversal-deep:"
      (= 12
         (pss/node-value-at-path
          traversal-deep-root
          (pss/next-path traversal-deep-root traversal-eleven 2 None)
          2 None)) ":"
      (= 11
         (pss/node-value-at-path
          traversal-deep-root
          (pss/prev-path traversal-deep-root traversal-twelve 2 None)
          2 None)) ":"
      (= 23
         (pss/node-value-at-path
          traversal-deep-root
          (pss/rightmost-path traversal-deep-root pss/empty-path 2 None)
          2 None)) ":"
      (if-some [seek-eighteen
                (pss/seek-path
                 traversal-deep-root 18 int-compare 2 None)]
        (= 18
           (pss/node-value-at-path
            traversal-deep-root seek-eighteen 2 None))
        false)))

(def traversal-forward (pss/node-seq traversal-deep-root 2 None))
(def traversal-reverse (pss/node-rseq traversal-deep-root 2 None))
(println
 (str "sequence:"
      (= 24 (count traversal-forward)) ":"
      (= 276 (reduce + 0 traversal-forward)) ":"
      (= 0 (first traversal-forward)) ":"
      (= 23 (first traversal-reverse)) ":"
      (= 0 (last traversal-reverse))))
(if-some [range-left
          (pss/seek-path traversal-deep-root 5 int-compare 2 None)]
  (let [range-right
        (pss/rseek-path traversal-deep-root 18 int-compare 2 None)
        values
        (pss/node-seq-between
         traversal-deep-root range-left range-right 2 None)
        reverse-values
        (pss/node-rseq-between
         traversal-deep-root range-left range-right 2 None)]
    (println
     (str "sequence-range:"
          (= 14 (count values)) ":"
          (= 161 (reduce + 0 values)) ":"
          (= 5 (first values)) ":"
          (= 18 (first reverse-values)) ":"
          (= 5 (last reverse-values)))))
  (println "sequence-range:false:false:false:false:false"))

(def ^:vector<int> small-values [5 1 3 3 9 7])
(def ^:pss/btset<int;unit;unit> small-set
  (pss/from-sequential int-compare small-values))
(println
 (str "set-build:"
      (= 5 (pss/set-count small-set)) ":"
      (= 25 (reduce + 0 (pss/set-seq small-set))) ":"
      (pss/set-contains? small-set 3) ":"
      (not (pss/set-contains? small-set 4))))
(println
 (str "set-est-count:"
      (if-some [iterator (pss/set-iter small-set)]
        (= 5 (pss/est-count iterator))
        false)))
(println
 (str "set-constructors:"
      (= [1 2 3]
         (vec (pss/set-seq (pss/sorted-set-by int-compare 3 1 2 2)))) ":"
      (= [1 2 3]
         (vec (pss/set-seq (pss/sorted-set 3 1 2 2))))))
(def added-set (pss/set-conj small-set 4))
(def duplicate-set (pss/set-conj added-set 4))
(def removed-set (pss/set-disj added-set 3))
(def missing-removed-set (pss/set-disj removed-set 100))
(println
 (str "set-persistent:"
      (= 6 (pss/set-count added-set)) ":"
      (not (pss/set-contains? small-set 4)) ":"
      (= 6 (pss/set-count duplicate-set)) ":"
      (= 5 (pss/set-count removed-set)) ":"
      (pss/set-contains? added-set 3) ":"
      (not (pss/set-contains? missing-removed-set 3))))

(def ^:pss/btset<int;unit;unit>
  large-set
  (pss/from-sequential int-compare (range 0 2000)))
(def large-added (pss/set-conj large-set 3000))
(def large-removed (pss/set-disj large-set 1000))
(println
 (str "set-large:"
      (= 2000 (pss/set-count large-set)) ":"
      (= 2 (pss/set-shift large-set)) ":"
      (= 0 (first (pss/set-seq large-set))) ":"
      (= 1999 (last (pss/set-seq large-set))) ":"
      (= 3000 (last (pss/set-seq large-added))) ":"
      (not (pss/set-contains? large-removed 1000)) ":"
      (= 1999 (pss/set-count large-removed))))
(if-some [large-slice (pss/set-slice large-set 995 1005)]
  (if-some [large-rslice (pss/set-rslice large-set 1005 995)]
    (println
     (str "set-slice:"
          (= 11 (count large-slice)) ":"
          (= 995 (first large-slice)) ":"
          (= 1005 (last large-slice)) ":"
          (= 1999 (first (pss/set-rseq large-set))) ":"
          (= 0 (last (pss/set-rseq large-set))) ":"
          (= 1005 (first large-rslice)) ":"
          (= 995 (last large-rslice))))
    (println "set-slice:false:false:false:false:false:false:false"))
  (println "set-slice:false:false:false:false:false:false:false"))
(println
 (str "set-singleton-slice:"
      (if-some [singleton (pss/set-slice large-set 1000 1000)]
        (and
         (= 1 (count singleton))
         (= 1000 (first singleton)))
        false)
      ":"
      (nil? (pss/set-slice large-set 1001 1000))))

(def ^:pss/btset<int;unit;unit> inserted-set
  (loop [set (pss/empty-set int-compare)
         value 0]
    (if (= value 1000)
      set
      (recur (pss/set-conj set value) (inc value)))))
(println
 (str "set-insert-stress:"
      (= 1000 (pss/set-count inserted-set)) ":"
      (= 2 (pss/set-shift inserted-set)) ":"
      (= 0 (first (pss/set-seq inserted-set))) ":"
      (= 999 (last (pss/set-seq inserted-set)))))

(def trimmed-set
  (loop [set large-set
         value 0]
    (if (= value 1900)
      set
      (recur (pss/set-disj set value) (inc value)))))
(def emptied-set
  (loop [set trimmed-set
         value 1900]
    (if (= value 2000)
      set
      (recur (pss/set-disj set value) (inc value)))))
(println
 (str "set-delete-stress:"
      (= 100 (pss/set-count trimmed-set)) ":"
      (= 1 (pss/set-shift trimmed-set)) ":"
      (= 1900 (first (pss/set-seq trimmed-set))) ":"
      (= 1999 (last (pss/set-seq trimmed-set))) ":"
      (= 0 (pss/set-count emptied-set)) ":"
      (= 0 (pss/set-shift emptied-set)) ":"
      (empty? (pss/set-seq emptied-set))))

(defn consume-iterator [iterator count total]
  (if-some [next-iterator (pss/iter-next iterator)]
    (consume-iterator
     next-iterator
     (inc count)
     (+ total (pss/iter-first iterator)))
    (tuple
     (inc count)
     (+ total (pss/iter-first iterator)))))

(if-some [iterator (pss/set-iter large-set)]
  (let [result (consume-iterator iterator 0 0)]
    (match result
      (tuple count total)
      (println
       (str "set-iterator:"
            (= 2000 count) ":"
            (= 1999000 total) ":"
            (= 0 (pss/iter-first iterator))))))
  (println "set-iterator:false:false:false"))

(defn decade-compare [left right]
  (compare (quot left 10) (quot right 10)))

(defn semantic-test-btset-by []
  (let [set
        (loop [set (pss/empty-set decade-compare)
               value 0]
          (if (= value 100)
            set
            (recur (pss/set-conj-with set value int-compare) (inc value))))]
    (if-some [values (pss/set-slice set 30 30)]
      (= (range 30 40) values)
      false)))

(defn test-slice []
  (if-some [values (pss/set-slice large-set 995 1005)]
    (and
     (= (vec (range 995 1006)) (vec values))
     (nil? (pss/set-slice large-set 1005 995)))
    false))

(defn test-reduces []
  (= 1999000 (pss/set-reduce large-set + 0)))

(defn iter-over-transient []
  (if-some [iterator (pss/set-iter small-set)]
    (let [updated (pss/set-conj small-set 4)]
      (and
       (= [1 3 5 7 9] (vec (pss/iterator-seq iterator)))
       (pss/set-contains? updated 4)))
    false))

(defn seek-for-seq-test []
  (= 500 (first (pss/seek (pss/set-seq large-set) 500 int-compare))))

(defn test-small []
  (= [1 3 5 7 9] (vec (pss/set-seq small-set))))

(defn stresstest-btset []
  (and
   (= 1000 (pss/set-count inserted-set))
   (= 0 (first (pss/set-seq inserted-set)))
   (= 999 (last (pss/set-seq inserted-set)))))

(defn stresstest-slice []
  (if-some [values (pss/set-slice inserted-set 250 750)]
    (= (vec (range 250 751)) (vec values))
    false))

(defn stresstest-rslice []
  (if-some [values (pss/set-rslice inserted-set 750 250)]
    (= (reverse (vec (range 250 751))) (vec values))
    false))

(defn stresstest-seek []
  (= (Some 750) (pss/seek-first inserted-set 750 int-compare)))

(defn test-overflow []
  (and
   (= 2 (pss/set-shift large-set))
   (= 1999 (last (pss/set-seq large-set)))))

(println
 (str "upstream-core-tests:"
      (semantic-test-btset-by) ":"
      (test-slice) ":"
      (test-reduces) ":"
      (iter-over-transient) ":"
      (seek-for-seq-test) ":"
      (test-small) ":"
      (stresstest-btset) ":"
      (stresstest-slice) ":"
      (stresstest-rslice) ":"
      (stresstest-seek) ":"
      (test-overflow)))
