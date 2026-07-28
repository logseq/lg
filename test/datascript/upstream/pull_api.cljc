(ns ^:no-doc datascript.pull-api
  (:require
   [clojure.string :as str]
   [datascript.pull-parser :as dpp]
   [datascript.db :as db]
   [me.tonsky.persistent-sorted-set :as set]))

(type-variant pulled-value
  (PulledScalar :Datascript_runtime.Data_value.t)
  (PulledEntity
   :map<Datascript_runtime.Data_value.t;pulled-value>)
  (PulledMany :vector<pulled-value>))

(type-alias pull-visitor
  :fn<keyword;option<int>;option<keyword>;option<int>;unit>)

(type-record PullContext
  (db :datascript.db/database-view)
  (visitor :option<pull-visitor>))

(type-record PullOptions
  (visitor :option<pull-visitor>))

(type-record ParsedPullOptions
  (context :datascript.pull-api/PullContext)
  (pattern :datascript.pull-parser/PullPattern))

(signature datascript.pull-api/pull-options
  :fn<pull-visitor;PullOptions>)

(defn ^PullOptions pull-options
  [visitor]
  (record PullOptions
    (visitor (Some visitor))))

(defn ^PullOptions default-pull-options []
  (record PullOptions
    (visitor None)))

(type-record DatomCursor
  (current :option<datascript.db/Datom>)
  (remaining :seq<datascript.db/Datom>))

(type-record ResultState
  (value :option<pulled-value>)
  (datoms :option<DatomCursor>))

(type-record MultivalAttrState
  (values :vector<pulled-value>)
  (attr :datascript.pull-parser/pull-attr)
  (datoms :DatomCursor))

(type-record MultivalRefAttrState
  (seen :set<int>)
  (recursion-limits :map<int;int>)
  (values :vector<pulled-value>)
  (pattern :datascript.pull-parser/PullPattern)
  (attr :datascript.pull-parser/pull-attr)
  (datoms :DatomCursor))

(type-record ReverseAttrsState
  (seen :set<int>)
  (recursion-limits :map<int;int>)
  (values :map<Datascript_runtime.Data_value.t;pulled-value>)
  (pattern :datascript.pull-parser/PullPattern)
  (attr :option<datascript.pull-parser/pull-attr>)
  (attrs :vector<datascript.pull-parser/pull-attr>)
  (attr-index :int)
  (id :int))

(type-record AttrsState
  (seen :set<int>)
  (recursion-limits :map<int;int>)
  (values :map<Datascript_runtime.Data_value.t;pulled-value>)
  (pattern :datascript.pull-parser/PullPattern)
  (attr :option<datascript.pull-parser/pull-attr>)
  (resume-attr :option<datascript.pull-parser/pull-attr>)
  (attrs :vector<datascript.pull-parser/pull-attr>)
  (attr-index :int)
  (datoms :option<DatomCursor>)
  (id :int))

(type-variant frame
  (ResultFrame :datascript.pull-api/ResultState)
  (MultivalAttrFrame :datascript.pull-api/MultivalAttrState)
  (MultivalRefAttrFrame :datascript.pull-api/MultivalRefAttrState)
  (ReverseAttrsFrame :datascript.pull-api/ReverseAttrsState)
  (AttrsFrame :datascript.pull-api/AttrsState))

(defprotocol IFrame
  (-merge [this result] :datascript.pull-api/frame)
  (-run
   [this context]
   :vector<datascript.pull-api/frame>)
  (-str [this] :string))

(defn pulled-to-data
  [value]
  (match value
    (PulledScalar value) value
    (PulledMany values)
    (Datascript_runtime.Data_value.vector_of_vector_with
     pulled-to-data
     values)
    (PulledEntity values)
    (Datascript_runtime.Data_value.map_of_data_map_with
     pulled-to-data
     values)))

(signature datascript.pull-api/assoc-pulled-value
  :fn<map<Datascript_runtime.Data_value.t;pulled-value>;Datascript_runtime.Data_value.t;pulled-value;map<Datascript_runtime.Data_value.t;pulled-value>>)

(defn assoc-pulled-value
  [values key value]
  (assoc values key value))

(signature datascript.pull-api/assoc-pulled-data
  :fn<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>;Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t;map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>)

(defn assoc-pulled-data
  [values key value]
  (assoc values key value))

(defn cursor-datom
  [cursor]
  (.-current cursor))

(defn next-cursor [cursor]
  (match (seq-uncons (.-remaining cursor))
    None
    (record DatomCursor
      (current None)
      (remaining (.-remaining cursor)))
    (Some entry)
    (record DatomCursor
      (current (Some (tuple-get entry 0)))
      (remaining (tuple-get entry 1)))))

(defn non-empty-cursor [cursor]
  (match (cursor-datom cursor)
    None None
    (Some _) (Some cursor)))

(defn frame-result
  [value datoms]
  (record ResultState
    (value value)
    (datoms datoms)))

(defn non-empty-many
  [values]
  (if (empty? values)
    None
    (Some (PulledMany values))))

(defn finish-multival-attr
  [values cursor]
  (ResultFrame
   (frame-result
    (non-empty-many values)
    (Some cursor))))

(defn cursor-matches-attr?
  [cursor attr]
  (match (cursor-datom cursor)
    None false
    (Some datom) (= (.-a datom) attr)))

(defn cursor-datom-exn
  [cursor]
  (match (cursor-datom cursor)
    None
    (Stdlib.invalid_arg "Datom cursor is exhausted")
    (Some datom) datom))

(defn skip-multival-attr
  [values data cursor]
  (loop [cursor cursor]
    (if (cursor-matches-attr? cursor (.-name data))
      (recur (next-cursor cursor))
      (ResultFrame
       (frame-result
        (Some (PulledMany values))
        (Some cursor))))))

(defn run-multival-attr
  [values attr cursor]
  (let [data (dpp/attr-data attr)]
    (loop [values values
           cursor cursor]
      (if-not
          (cursor-matches-attr? cursor (.-name data))
        (finish-multival-attr values cursor)
        (let [datom (cursor-datom-exn cursor)
              limit-reached
              (match (.-limit data)
                None false
                (Some limit)
                (>= (count values) limit))]
          (if limit-reached
            (skip-multival-attr values data cursor)
            (recur
             (conj values (PulledScalar (.-v datom)))
             (next-cursor cursor))))))))

(defn run-multival-attr-frame [state]
  (run-multival-attr
   (.-values state)
   (.-attr state)
   (.-datoms state)))

(defn visit
  [context kind entity attr value]
  (match (.-visitor context)
    None (Stdlib.ignore 0)
    (Some visitor)
    (Stdlib.ignore (visitor kind entity attr value))))

(defn cursor-from-seq
  [datoms]
  (match (seq-uncons datoms)
    None None
    (Some entry)
    (Some
     (record DatomCursor
       (current (Some (tuple-get entry 0)))
       (remaining (tuple-get entry 1))))))

(defn cursor-from-datoms
  [datoms]
  (match datoms
    None None
    (Some datoms) (cursor-from-seq datoms)))

(defn attr-in-range?
  [attr from to]
  (and
   (not (neg? (compare attr from)))
   (not (pos? (compare attr to)))))

(defn pull-forward-cursor
  [database pattern id]
  (if (:wildcard pattern)
    (cursor-from-datoms
     (db/database-view-search
      database
      (Some id)
      None
      None
      None))
    (match (:first-attr pattern)
      None None
      (Some first-attr)
      (match (:last-attr pattern)
        None None
        (Some last-attr)
        (let [from (.-name (dpp/attr-data first-attr))
              to (.-name (dpp/attr-data last-attr))]
          (match database
            (db/DatabaseView unfiltered)
            (cursor-from-datoms
             (set/slice
              (.-eavt unfiltered)
              (db/datom-bound
               (Some id)
               (Some from)
               None
               None
               db/e0
               db/tx0)
              (db/datom-bound
               (Some id)
               (Some to)
               None
               None
               db/e0
               db/txmax)))
            (db/FilteredDatabaseView _)
            (if-some
              [datoms
               (db/database-view-search
                database
                (Some id)
                None
                None
                None)]
              (cursor-from-seq
               (filter
                (fn [datom]
                  (attr-in-range?
                   (.-a datom)
                   from
                   to))
                datoms))
              None)))))))

(signature datascript.pull-api/attrs-state
  :fn<PullContext;set<int>;map<int;int>;datascript.pull-parser/PullPattern;int;AttrsState>)

(defn attrs-state
  [context seen recursion-limits pattern id]
  (let [database (.-db context)
        datoms (pull-forward-cursor database pattern id)]
    (when (:wildcard pattern)
      (visit
       context :db.pull/wildcard (Some id) None None))
    (record AttrsState
      (seen seen)
      (recursion-limits recursion-limits)
      (values {})
      (pattern pattern)
      (attr (first (:attrs pattern)))
      (resume-attr None)
      (attrs (:attrs pattern))
      (attr-index 1)
      (datoms datoms)
      (id id))))

(signature datascript.pull-api/attrs-frame
  :fn<PullContext;set<int>;map<int;int>;datascript.pull-parser/PullPattern;int;frame>)

(defn attrs-frame
  [context seen recursion-limits pattern id]
  (AttrsFrame
   (attrs-state
    context seen recursion-limits pattern id)))

(defn attr-pattern
  [attr]
  (match (dpp/attr-pattern attr)
    (Some pattern) pattern
    None
    (Stdlib.invalid_arg
     "Pull reference attribute requires a nested pattern")))

(defn auto-expanding?
  [attr]
  (let [data (dpp/attr-data attr)]
    (or
     (.-recursive data)
     (and
      (.-component data)
      (dpp/attr-pattern-wildcard attr)))))

(defn cycle-entity [id]
  (let [values {}]
    (PulledEntity
     (assoc-pulled-value
      values
      (Datascript_runtime.Data_value.Keyword ":db/id")
      (PulledScalar
       (Datascript_runtime.Data_value.Int id))))))

(signature datascript.pull-api/expanding-ref-frame
  :fn<PullContext;set<int>;map<int;int>;datascript.pull-parser/PullPattern;datascript.pull-parser/pull-attr;int;frame>)

(defn expanding-ref-frame
  [context seen recursion-limits pattern attr id]
  (attrs-frame
   context
   (conj seen id)
   recursion-limits
   (if (.-recursive (dpp/attr-data attr))
     pattern
     (attr-pattern attr))
   id))

(defn ref-frame
  [context seen recursion-limits pattern attr id]
  (let [data (dpp/attr-data attr)]
    (if-not (auto-expanding? attr)
      (attrs-frame
       context seen recursion-limits (attr-pattern attr) id)
      (if (contains? seen id)
        (ResultFrame
         (frame-result
          (Some
           (cycle-entity id))
          None))
        (let [recursion-key (.-recursion-key data)]
          (match (get recursion-limits recursion-key)
          (Some remaining)
          (if (<= remaining 0)
            (ResultFrame (frame-result None None))
            (expanding-ref-frame
             context
             seen
             (assoc recursion-limits recursion-key (dec remaining))
             pattern
             attr
             id))
          None
          (expanding-ref-frame
           context
           seen
           (match (.-recursion-limit data)
             None recursion-limits
             (Some limit)
             (assoc recursion-limits recursion-key (dec limit)))
           pattern
           attr
           id)))))))

(defn ref-datom-id
  [data datom]
  (if (.-reverse data)
    (Some (.-e datom))
    (Datascript_runtime.Data_value.ref_value (.-v datom))))

(defn next-multival-ref-state
  [state]
  (record MultivalRefAttrState
    (seen (.-seen state))
    (recursion-limits (.-recursion-limits state))
    (values (.-values state))
    (pattern (.-pattern state))
    (attr (.-attr state))
    (datoms (next-cursor (.-datoms state)))))

(defn finish-multival-ref
  [state]
  (ResultFrame
   (frame-result
    (non-empty-many (.-values state))
    (Some (.-datoms state)))))

(defn skip-multival-ref
  [state]
  (let [data (dpp/attr-data (.-attr state))]
    (match (cursor-datom (.-datoms state))
      None
      (ResultFrame
       (frame-result
        (Some
         (PulledMany (.-values state)))
        (Some (.-datoms state))))
      (Some datom)
      (if (= (.-a datom) (.-name data))
        (skip-multival-ref (next-multival-ref-state state))
        (ResultFrame
         (frame-result
          (Some
           (PulledMany (.-values state)))
          (Some (.-datoms state))))))))

(signature datascript.pull-api/run-multival-ref-frame
  :fn<PullContext;MultivalRefAttrState;vector<frame>>)

(defn run-multival-ref-frame
  [context state]
  (let [data (dpp/attr-data (.-attr state))]
    (match (cursor-datom (.-datoms state))
      None
      [(finish-multival-ref state)]

      (Some datom)
      (if-not (= (.-a datom) (.-name data))
        [(finish-multival-ref state)]
        (let [limit-reached
              (match (.-limit data)
                None false
                (Some limit)
                (>= (count (.-values state)) limit))]
          (if limit-reached
            [(skip-multival-ref state)]
            (match (ref-datom-id data datom)
              None
              (Stdlib.invalid_arg
               "Pull ref attribute contains a non-reference value")
              (Some id)
              [(MultivalRefAttrFrame
                state)
               (ref-frame
                context
                (.-seen state)
                (.-recursion-limits state)
                (.-pattern state)
                (.-attr state)
                id)])))))))

(defn attr-at-index
  [attrs index]
  (if (< index (count attrs))
    (Some (nth attrs index))
    None))

(defn next-parent-datoms
  [child-datoms parent-datoms]
  (match child-datoms
    (Some datoms) (Some datoms)
    None
    (match parent-datoms
      None None
      (Some datoms)
      (non-empty-cursor (next-cursor datoms)))))

(defn apply-attr-xform
  [attr value]
  (match (.-xform (dpp/attr-data attr))
    None value
    (Some xform)
    (let [input
          (match value
            None None
            (Some value)
            (Some (pulled-to-data value)))]
      (match (xform input)
        None None
        (Some value)
        (Some (PulledScalar value))))))

(defn merge-attr-value
  [values attr value]
  (match (apply-attr-xform attr value)
    None values
    (Some value)
    (assoc-pulled-value
     values (.-alias (dpp/attr-data attr)) value)))

(defn merge-attrs-result
  [state result]
  (match (.-attr state)
    None
    (Stdlib.invalid_arg
     "AttrsFrame cannot merge without a current attribute")
    (Some attr)
    (let [attrs (.-attrs state)
          index (.-attr-index state)
          resume-attr (.-resume-attr state)
          next-attr
          (match resume-attr
            (Some attr) (Some attr)
            None (attr-at-index attrs index))
          next-index
          (match resume-attr
            (Some _) index
            None (inc index))]
      (AttrsFrame
       (record AttrsState
         (seen (.-seen state))
         (recursion-limits (.-recursion-limits state))
         (values
          (merge-attr-value
           (.-values state)
           attr
           (.-value result)))
         (pattern (.-pattern state))
         (attr next-attr)
         (resume-attr None)
         (attrs attrs)
         (attr-index next-index)
         (datoms
          (next-parent-datoms
           (.-datoms result)
           (.-datoms state)))
         (id (.-id state)))))))

(signature datascript.pull-api/merge-multival-ref-result
  :fn<MultivalRefAttrState;ResultState;frame>)

(defn merge-multival-ref-result
  [state result]
  (MultivalRefAttrFrame
   (record MultivalRefAttrState
     (seen (.-seen state))
     (recursion-limits (.-recursion-limits state))
     (values
      (match (.-value result)
        None (.-values state)
        (Some value) (conj (.-values state) value)))
     (pattern (.-pattern state))
     (attr (.-attr state))
     (datoms (next-cursor (.-datoms state))))))

(defn merge-reverse-result
  [state result]
  (match (.-attr state)
    None
    (Stdlib.invalid_arg
     "ReverseAttrsFrame cannot merge without a current attribute")
    (Some attr)
    (let [attrs (.-attrs state)
          index (.-attr-index state)]
      (ReverseAttrsFrame
       (record ReverseAttrsState
         (seen (.-seen state))
         (recursion-limits (.-recursion-limits state))
         (values
          (merge-attr-value
           (.-values state)
           attr
           (.-value result)))
         (pattern (.-pattern state))
         (attr (attr-at-index attrs index))
         (attrs attrs)
         (attr-index (inc index))
         (id (.-id state)))))))

(defn merge-frame [parent result]
  (match parent
    (AttrsFrame state)
    (merge-attrs-result state result)
    (MultivalRefAttrFrame state)
    (merge-multival-ref-result state result)
    (ReverseAttrsFrame state)
    (merge-reverse-result state result)
    _
    (Stdlib.invalid_arg
     "Frame does not accept a child result")))

(defn attrs-state-with
  [state values attr resume-attr attr-index datoms]
  (record AttrsState
    (seen (.-seen state))
    (recursion-limits (.-recursion-limits state))
    (values values)
    (pattern (.-pattern state))
    (attr attr)
    (resume-attr resume-attr)
    (attrs (.-attrs state))
    (attr-index attr-index)
    (datoms datoms)
    (id (.-id state))))

(defn advance-attrs-state
  [state values datoms]
  (let [index (.-attr-index state)]
    (attrs-state-with
     state
     values
     (attr-at-index (.-attrs state) index)
     None
     (inc index)
     datoms)))

(defn reverse-attrs-frame [state]
  (let [attrs (:reverse-attrs (.-pattern state))]
    (ReverseAttrsFrame
     (record ReverseAttrsState
       (seen (.-seen state))
       (recursion-limits (.-recursion-limits state))
       (values (.-values state))
       (pattern (.-pattern state))
       (attr (first attrs))
       (attrs attrs)
       (attr-index 1)
       (id (.-id state))))))

(defn start-attr-child
  [context state attr resume-attr cursor]
  (let [data (dpp/attr-data attr)
        parent
        (AttrsFrame
         (attrs-state-with
          state
          (.-values state)
          (Some attr)
          resume-attr
          (.-attr-index state)
          (Some cursor)))]
    (if (.-multival data)
      (if (.-ref data)
        [parent
         (MultivalRefAttrFrame
          (record MultivalRefAttrState
            (seen (.-seen state))
            (recursion-limits (.-recursion-limits state))
            (values [])
            (pattern (.-pattern state))
            (attr attr)
            (datoms cursor)))]
        [parent
         (MultivalAttrFrame
          (record MultivalAttrState
            (values [])
            (attr attr)
            (datoms cursor)))])
      (if (.-ref data)
        (match (cursor-datom cursor)
          None
          (Stdlib.invalid_arg
           "Pull ref attribute has no datom")
          (Some datom)
          (match (ref-datom-id data datom)
            None
            (Stdlib.invalid_arg
             "Pull ref attribute contains a non-reference value")
            (Some id)
            [parent
             (ref-frame
              context
              (.-seen state)
              (.-recursion-limits state)
              (.-pattern state)
              attr
              id)]))
        (Stdlib.invalid_arg
         "Scalar attribute does not start a child frame")))))

(defn add-scalar-datom
  [values attr datom]
  (merge-attr-value
   values
   attr
   (Some (PulledScalar (.-v datom)))))

(defn add-default
  [values attr]
  (let [data (dpp/attr-data attr)]
    (match (.-default data)
      None values
      (Some value)
      (assoc-pulled-value
       values
       (.-alias data)
       (PulledScalar value)))))

(defn add-missing-value
  [values attr]
  (let [data (dpp/attr-data attr)]
    (match (.-default data)
      (Some value)
      (assoc-pulled-value
       values
       (.-alias data)
       (PulledScalar value))
      None
      (merge-attr-value values attr None))))

(defn missing-attr-state
  [context state attr]
  (let [data (dpp/attr-data attr)]
    (visit
     context
     :db.pull/attr
     (Some (.-id state))
     (Some (.-name data))
     None)
    (advance-attrs-state
     state
     (add-missing-value (.-values state) attr)
     (.-datoms state))))

(declare run-attrs-frame)

(defn run-wildcard-attr
  [context state explicit-attr cursor]
  (match (cursor-datom cursor)
    None
    (run-attrs-frame
     context
     (attrs-state-with
      state
      (.-values state)
      explicit-attr
      None
      (.-attr-index state)
      None))
    (Some datom)
    (let [attr (dpp/attribute (.-db context) (.-a datom))
          data (dpp/attr-data attr)]
      (visit
       context
       :db.pull/attr
       (Some (.-id state))
       (Some (.-name data))
       None)
      (if (or (.-multival data) (.-ref data))
        (start-attr-child
         context state attr explicit-attr cursor)
        (run-attrs-frame
         context
         (attrs-state-with
          state
          (add-scalar-datom (.-values state) attr datom)
          explicit-attr
          None
          (.-attr-index state)
          (non-empty-cursor (next-cursor cursor))))))))

(defn run-attrs-frame
  [context state]
  (match (.-attr state)
    None
    (match (.-datoms state)
      None [(reverse-attrs-frame state)]
      (Some cursor)
      (if (:wildcard (.-pattern state))
        (run-wildcard-attr context state None cursor)
        (run-attrs-frame
         context
         (attrs-state-with
          state
          (.-values state)
          None
          None
          (.-attr-index state)
          (non-empty-cursor (next-cursor cursor))))))

    (Some attr)
    (let [data (dpp/attr-data attr)]
      (if (= (.-name data) :db/id)
        (run-attrs-frame
         context
         (advance-attrs-state
          state
          (merge-attr-value
           (.-values state)
           attr
           (Some
            (PulledScalar
             (Datascript_runtime.Data_value.Int
              (.-id state)))))
          (.-datoms state)))
        (match (.-datoms state)
          None
          (run-attrs-frame
           context
           (missing-attr-state context state attr))

          (Some cursor)
          (match (cursor-datom cursor)
            None
            (run-attrs-frame
             context
             (missing-attr-state context state attr))
            (Some datom)
            (let [comparison
                  (String.compare
                   (.-name data)
                   (.-a datom))]
              (cond
                (> comparison 0)
                (if (:wildcard (.-pattern state))
                  (run-wildcard-attr
                   context state (Some attr) cursor)
                  (run-attrs-frame
                   context
                   (attrs-state-with
                    state
                    (.-values state)
                    (Some attr)
                    None
                    (.-attr-index state)
                    (non-empty-cursor
                     (next-cursor cursor)))))

                (< comparison 0)
                (run-attrs-frame
                 context
                 (missing-attr-state context state attr))

                :else
                (do
                  (visit
                   context
                   :db.pull/attr
                   (Some (.-id state))
                   (Some (.-name data))
                   None)
                  (if (or (.-multival data) (.-ref data))
                    (start-attr-child
                     context state attr None cursor)
                    (run-attrs-frame
                     context
                     (advance-attrs-state
                      state
                      (add-scalar-datom
                       (.-values state)
                       attr
                       datom)
                      (non-empty-cursor
                       (next-cursor cursor))))))))))))))

(defn advance-reverse-state
  [state values]
  (let [attrs (.-attrs state)
        index (.-attr-index state)]
    (record ReverseAttrsState
      (seen (.-seen state))
      (recursion-limits (.-recursion-limits state))
      (values values)
      (pattern (.-pattern state))
      (attr (attr-at-index attrs index))
      (attrs attrs)
      (attr-index (inc index))
      (id (.-id state)))))

(defn run-reverse-attrs-frame
  [context state]
  (match (.-attr state)
    None
    [(ResultFrame
      (frame-result
       (if (empty? (.-values state))
         None
         (Some (PulledEntity (.-values state))))
       None))]

    (Some attr)
    (let [data (dpp/attr-data attr)
          cursor
          (cursor-from-datoms
           (db/database-view-search
            (.-db context)
            None
            (Some (.-name data))
            (Some
             (Datascript_runtime.Data_value.Ref
              (.-id state)))
            None))]
      (visit
       context
       :db.pull/reverse
       None
       (Some (.-name data))
       (Some (.-id state)))
      (match cursor
        None
        [(ReverseAttrsFrame
          (advance-reverse-state
           state
           (add-default (.-values state) attr)))]
        (Some cursor)
        (let [parent (ReverseAttrsFrame state)]
          (if (.-component data)
            [parent
             (ref-frame
              context
              (.-seen state)
              (.-recursion-limits state)
              (.-pattern state)
              attr
              (.-e (cursor-datom-exn cursor)))]
            [parent
             (MultivalRefAttrFrame
              (record MultivalRefAttrState
                (seen (.-seen state))
                (recursion-limits
                 (.-recursion-limits state))
                (values [])
                (pattern (.-pattern state))
                (attr attr)
                (datoms cursor)))]))))))

(defn run-frame
  [context current]
  (match current
    (AttrsFrame state)
    (run-attrs-frame context state)
    (MultivalAttrFrame state)
    [(run-multival-attr-frame state)]
    (MultivalRefAttrFrame state)
    (run-multival-ref-frame context state)
    (ReverseAttrsFrame state)
    (run-reverse-attrs-frame context state)
    (ResultFrame _)
    (Stdlib.invalid_arg
     "ResultFrame cannot be run")))

(defn- frame-result-state-exn [result]
  (match result
    (ResultFrame state) state
    _
    (Stdlib.invalid_arg
     "Frame merge requires a ResultFrame")))

(defn- pull-attr-string
  [attr]
  (let [data (dpp/attr-data attr)
        alias (.-alias data)]
    (match alias
      (Datascript_runtime.Data_value.Nil)
      (str (.-name data))
      _
      (Datascript_runtime.Data_value.to_edn_string alias))))

(defn- remaining-attrs-string
  [attrs index]
  (if (< index (count attrs))
    (str/join
     " "
     (mapv
      pull-attr-string
      (subvec attrs index)))
    ""))

(defn- frame-string [current]
  (match current
    (ResultFrame state)
    (str
     "ResultFrame<value="
     (match (.-value state)
       None ""
       (Some value)
       (Datascript_runtime.Data_value.to_edn_string
        (pulled-to-data value)))
     ">")
    (MultivalAttrFrame state)
    (str
     "MultivalAttrFrame<attr="
     (pull-attr-string (.-attr state))
     ">")
    (MultivalRefAttrFrame state)
    (str
     "MultivalAttrFrame<attr="
     (pull-attr-string (.-attr state))
     ">")
    (ReverseAttrsFrame state)
    (str
     "ReverseAttrsFrame<id="
     (.-id state)
     ", attr="
     (match (.-attr state)
       None ""
       (Some attr) (pull-attr-string attr))
     ", attrs="
     (remaining-attrs-string
      (.-attrs state)
      (.-attr-index state))
     ">")
    (AttrsFrame state)
    (str
     "AttrsFrame<id="
     (.-id state)
     ", attr="
     (match (.-attr state)
       None ""
       (Some attr) (pull-attr-string attr))
     ", attrs="
     (remaining-attrs-string
      (.-attrs state)
      (.-attr-index state))
     ">")))

(extend-type datascript.pull-api/frame
  IFrame
  (-merge [current result]
    (merge-frame
     current
     (frame-result-state-exn result)))
  (-run [current context]
    (run-frame context current))
  (-str [current]
    (frame-string current)))

(defn pulled-map-to-data
  [values]
  (reduce-kv
   (fn
     [result key value]
     (assoc-pulled-data
      result key (pulled-to-data value)))
   {}
   values))

(defn ^frame first-frame [^:list<frame> stack]
  (peek stack))

(defn ^:list<frame> rest-frames [^:list<frame> stack]
  (pop stack))

(defn ^:option<ResultState> result-frame-state [^frame current]
  (match current
    (ResultFrame result) (Some result)
    _ None))

(defn ^ResultState compact-child-result [^ResultState result]
  (record ResultState
    (value
     (match (.-value result)
       None None
       (Some (PulledScalar value))
       (Some (PulledScalar value))
       (Some value)
       (Some (PulledScalar (pulled-to-data value)))))
    (datoms (.-datoms result))))

(defn ^:list<frame> push-frame
  [^:list<frame> stack ^frame value]
  (conj stack value))

(defn ^:option<pulled-value> run-stack
  [^PullContext context ^:list<frame> stack]
  (let [current (first-frame stack)
        stack-before-current (rest-frames stack)]
    (match (result-frame-state current)
      (Some result)
      (if (empty? stack-before-current)
        (.-value result)
        (let [parent (first-frame stack-before-current)
              stack-before-parent (rest-frames stack-before-current)]
          (run-stack
           context
           (conj
            stack-before-parent
            (merge-frame
             parent
             (compact-child-result result))))))

      None
      (run-stack
       context
       (reduce
        push-frame
        stack-before-current
        (run-frame context current))))))

(defn
  ^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
  pull-parsed-with-options
  [^datascript.db/database-view database
   ^datascript.pull-parser/PullPattern pattern
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^PullOptions options]
  (if-some [eid (db/database-view-entid database entity-ref)]
    (let [context
          (record PullContext
            (db database)
            (visitor (.-visitor options)))
          root
          (attrs-frame
           context
           (set-of :int)
           {}
           pattern
           eid)]
      (match (run-stack context (list root))
        None None
        (Some (PulledEntity values))
        (Some (pulled-map-to-data values))
        (Some _)
        (Stdlib.invalid_arg
         "Root pull result is not an entity")))
    None))

(defn ^ParsedPullOptions parse-opts
  ([^datascript.db/database-view database
    ^:vector<datascript.pull-parser/pull-source-item> pattern]
   (parse-opts database pattern (default-pull-options)))
  ([^datascript.db/database-view database
    ^:vector<datascript.pull-parser/pull-source-item> pattern
    ^PullOptions options]
   (record ParsedPullOptions
     (context
      (record PullContext
        (db database)
        (visitor (.-visitor options))))
     (pattern (dpp/parse-pattern-view database pattern)))))

(defn
  ^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
  pull-impl
  [^ParsedPullOptions parsed
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (let [context (.-context parsed)]
    (pull-parsed-with-options
     (.-db context)
     (.-pattern parsed)
     entity-ref
     (record PullOptions
       (visitor (.-visitor context))))))

(defn
  ^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
  pull-parsed
  [^datascript.db/database-view database
   ^datascript.pull-parser/PullPattern pattern
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (pull-parsed-with-options
   database pattern entity-ref (default-pull-options)))

(defn
  ^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
  pull-source-with-options
  [^datascript.db/database-view database
   ^:vector<datascript.pull-parser/pull-source-item> pattern
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^PullOptions options]
  (pull-parsed-with-options
   database
   (dpp/parse-pattern-view database pattern)
   entity-ref
   options))

(defn
  ^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
  pull-source
  [^datascript.db/database-view database
   ^:vector<datascript.pull-parser/pull-source-item> pattern
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (pull-parsed
   database
   (dpp/parse-pattern-view database pattern)
   entity-ref))

(defn pull
  {:inline
   (fn [database pattern entity-id & options]
     (let [unquote-form
           (fn [form]
             (if (and
                  (seq? form)
                  (= 'quote (first form)))
               (second form)
               form))
           source-item
           (fn source-item [item]
             (let [item (unquote-form item)]
               (if
                 (or
                  (map? item)
                  (and
                   (vector? item)
                   (= :datascript.pull/map-entry
                      (first item))))
                 (if (and (map? item) (next item))
                   (list
                    'datascript.pull-parser/source-group
                    (list
                     'vec
                     (cons
                      'list
                      (map
                       (fn [entry]
                         (source-item
                          [:datascript.pull/map-entry entry]))
                       item))))
               (let [entry
                     (if (map? item)
                       (first item)
                       (second item))
                     source-attr-form
                     (unquote-form (first entry))
                     source-option
                     (fn [option value]
                       (if (= option :as)
                         (list
                          'datascript.pull-parser/option-alias
                          value)
                         (if (= option :limit)
                           (if (nil? value)
                             (list
                              'datascript.pull-parser/option-unlimited)
                             (list
                              'datascript.pull-parser/option-limit
                              value))
                           (if (= option :xform)
                             (list
                              'datascript.pull-parser/option-xform
                              value)
                             (let [default-form
                                   (if (nil? value)
                                     (list
                                      'Datascript_runtime.Data_value.Nil)
                                     (if (string? value)
                                       (list
                                        'Datascript_runtime.Data_value.String
                                        value)
                                       (if (keyword? value)
                                         (list
                                          'Datascript_runtime.Data_value.Keyword
                                          (str value))
                                         (if (= value true)
                                           (list
                                            'Datascript_runtime.Data_value.Bool
                                            true)
                                           (if (= value false)
                                             (list
                                              'Datascript_runtime.Data_value.Bool
                                              false)
                                             (list
                                              'Datascript_runtime.Data_value.Int
                                              value))))))]
                               (list
                                'datascript.pull-parser/option-default
                                default-form))))))
                     source-attr
                     (if (vector? source-attr-form)
                       (first source-attr-form)
                       (if (seq? source-attr-form)
                         (second source-attr-form)
                         source-attr-form))
                     source-options
                     (if (vector? source-attr-form)
                       (loop [remaining (next source-attr-form)
                              result []]
                         (if (empty? remaining)
                           result
                           (recur
                            (nnext remaining)
                            (conj
                             result
                             (source-option
                              (first remaining)
                              (second remaining))))))
                       (if (seq? source-attr-form)
                         [(source-option
                           (if (or
                                (= 'limit (first source-attr-form))
                                (= "limit" (first source-attr-form)))
                             :limit
                             :default)
                           (first (nnext source-attr-form)))]
                         []))
                     source-pattern (unquote-form (second entry))]
                 (if (vector? source-pattern)
                   (if (empty? source-options)
                     (list
                      'datascript.pull-parser/source-nested
                      source-attr
                      (list
                       'vec
                       (cons
                        'list
                        (map source-item source-pattern))))
                     (list
                      'datascript.pull-parser/source-nested-options
                      source-attr
                      (list
                       'vec
                       (cons 'list source-options))
                      (list
                       'vec
                       (cons
                        'list
                        (map source-item source-pattern)))))
                   (if (empty? source-options)
                     (list
                      'datascript.pull-parser/source-recursion
                      source-attr
                      (if (or
                           (= source-pattern '...)
                           (= source-pattern "..."))
                        'None
                        (list 'Some source-pattern)))
                     (list
                      'datascript.pull-parser/source-recursion-options
                      source-attr
                      (list
                       'vec
                       (cons 'list source-options))
                      (if (or
                           (= source-pattern '...)
                           (= source-pattern "..."))
                        'None
                        (list 'Some source-pattern)))))))
               (if (seq? item)
                 (let [operation (first item)
                       source-attr (second item)
                       value (first (nnext item))]
                   (if (or
                        (= operation 'limit)
                        (= operation "limit"))
                     (list
                      'datascript.pull-parser/source-options
                      source-attr
                      (list
                       'vector
                       (if (nil? value)
                         (list
                          'datascript.pull-parser/option-unlimited)
                         (list
                          'datascript.pull-parser/option-limit
                          value))))
                     (let [default-form
                           (if (nil? value)
                             (list
                              'Datascript_runtime.Data_value.Nil)
                             (if (string? value)
                               (list
                                'Datascript_runtime.Data_value.String
                                value)
                               (if (keyword? value)
                                 (list
                                  'Datascript_runtime.Data_value.Keyword
                                  (str value))
                                 (if (= value true)
                                   (list
                                    'Datascript_runtime.Data_value.Bool
                                    true)
                                   (if (= value false)
                                     (list
                                      'Datascript_runtime.Data_value.Bool
                                      false)
                                     (list
                                      'Datascript_runtime.Data_value.Int
                                      value))))))]
                       (list
                        'datascript.pull-parser/source-default
                        source-attr
                        default-form))))
               (if (vector? item)
                 (let [source-attr (first item)
                       source-option
                       (fn [option value]
                         (if (= option :as)
                           (list
                            'datascript.pull-parser/option-alias
                            value)
                           (if (= option :limit)
                             (if (nil? value)
                               (list
                                'datascript.pull-parser/option-unlimited)
                               (list
                                'datascript.pull-parser/option-limit
                                value))
                             (if (= option :xform)
                               (list
                                'datascript.pull-parser/option-xform
                                value)
                               (let [default-form
                                     (if (nil? value)
                                       (list
                                        'Datascript_runtime.Data_value.Nil)
                                       (if (string? value)
                                         (list
                                          'Datascript_runtime.Data_value.String
                                          value)
                                         (if (keyword? value)
                                           (list
                                            'Datascript_runtime.Data_value.Keyword
                                            (str value))
                                           (if (= value true)
                                             (list
                                              'Datascript_runtime.Data_value.Bool
                                              true)
                                             (if (= value false)
                                               (list
                                                'Datascript_runtime.Data_value.Bool
                                                false)
                                               (if
                                                (or
                                                 (symbol? value)
                                                 (seq? value))
                                                value
                                                (list
                                                 'Datascript_runtime.Data_value.Int
                                                 value)))))))]
                                 (list
                                  'datascript.pull-parser/option-default
                                  default-form))))))
                       option-forms
                       (loop [remaining (next item)
                              result []]
                         (if (empty? remaining)
                           result
                           (recur
                            (nnext remaining)
                            (conj
                             result
                             (source-option
                              (first remaining)
                              (second remaining))))))]
                   (list
                    'datascript.pull-parser/source-options
                    source-attr
                    (list
                     'vec
                     (cons 'list option-forms))))
               (if (or
                    (= item :*)
                    (= item '*)
                    (= item "*"))
                 'datascript.pull-parser/source-wildcard
                 (list
                  'datascript.pull-parser/source-attribute
                  item)))))))]
       (let [pattern (unquote-form pattern)
             source
             (if (vector? pattern)
               (vec (map source-item pattern))
               pattern)
             entity-ref
             (if (vector? entity-id)
               (let [lookup-attr (first entity-id)
                     lookup-value (second entity-id)
                     value-form
                     (if (nil? lookup-value)
                       (list
                        'Datascript_runtime.Data_value.Nil)
                       (if (string? lookup-value)
                         (list
                          'Datascript_runtime.Data_value.String
                          lookup-value)
                         (if (keyword? lookup-value)
                           (list
                            'Datascript_runtime.Data_value.Keyword
                            (str lookup-value))
                           (if (or
                                (symbol? lookup-value)
                                (seq? lookup-value))
                             lookup-value
                             (list
                              'Datascript_runtime.Data_value.Int
                              lookup-value)))))]
                 (list
                  'Datascript_runtime.Data_value.Lookup_ref
                  (str lookup-attr)
                  value-form))
               (if (keyword? entity-id)
                 (list
                  'Datascript_runtime.Data_value.Ident
                  (str entity-id))
                 (list
                  'Datascript_runtime.Data_value.Entity_id
                  entity-id)))]
         (if (empty? options)
           (list
            'datascript.pull-api/pull-source
            (list
             'datascript.db/database-view
             database)
            source
            entity-ref)
           (list
            'datascript.pull-api/pull-source-with-options
            (list
             'datascript.db/database-view
             database)
            source
            entity-ref
            (first options))))))}
  ([^datascript.db/database-view database
    ^:vector<datascript.pull-parser/pull-source-item> pattern
    ^:Datascript_runtime.Data_value.entity_ref entity-ref]
   (pull-source database pattern entity-ref))
  ([^datascript.db/database-view database
    ^:vector<datascript.pull-parser/pull-source-item> pattern
    ^:Datascript_runtime.Data_value.entity_ref entity-ref
    ^PullOptions options]
   (pull-source-with-options database pattern entity-ref options)))

(defn
  ^:vector<option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>>
  pull-many-parsed
  [^datascript.db/database-view database
   ^datascript.pull-parser/PullPattern pattern
   ^:vector<Datascript_runtime.Data_value.entity_ref> entity-refs]
  (mapv
   (fn [entity-ref]
     (pull-parsed database pattern entity-ref))
   entity-refs))

(defn ^:vector<Datascript_runtime.Data_value.entity_ref>
  entity-ids-to-refs
  [^:vector<int> entity-ids]
  (mapv
   (fn [entity-id]
     (Datascript_runtime.Data_value.Entity_id entity-id))
   entity-ids))

(defn
  ^:vector<option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>>
  pull-many-source
  [^datascript.db/database-view database
   ^:vector<datascript.pull-parser/pull-source-item> source
  ^:vector<Datascript_runtime.Data_value.entity_ref> entity-refs]
  (let [pattern (dpp/parse-pattern-view database source)]
    (pull-many-parsed database pattern entity-refs)))

(defn
  ^:vector<option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>>
  pull-many-source-with-options
  [^datascript.db/database-view database
   ^:vector<datascript.pull-parser/pull-source-item> source
   ^:vector<Datascript_runtime.Data_value.entity_ref> entity-refs
   ^PullOptions options]
  (let [pattern (dpp/parse-pattern-view database source)]
    (mapv
     (fn [entity-ref]
       (pull-parsed-with-options
        database pattern entity-ref options))
     entity-refs)))

(defn pull-many
  {:inline
   (fn [database pattern entity-ids & options]
     (let [unquote-form
           (fn [form]
             (if (and
                  (seq? form)
                  (= 'quote (first form)))
               (second form)
               form))
           source-item
           (fn source-item [item]
             (let [item (unquote-form item)]
               (if
                 (or
                  (map? item)
                  (and
                   (vector? item)
                   (= :datascript.pull/map-entry
                      (first item))))
                 (if (and (map? item) (next item))
                   (list
                    'datascript.pull-parser/source-group
                    (list
                     'vec
                     (cons
                      'list
                      (map
                       (fn [entry]
                         (source-item
                          [:datascript.pull/map-entry entry]))
                       item))))
                   (let [entry
                         (if (map? item)
                           (first item)
                           (second item))
                         source-attr-form
                         (unquote-form (first entry))
                         source-option
                         (fn [option value]
                           (if (= option :as)
                             (list
                              'datascript.pull-parser/option-alias
                              value)
                             (if (= option :limit)
                               (if (nil? value)
                                 (list
                                  'datascript.pull-parser/option-unlimited)
                                 (list
                                  'datascript.pull-parser/option-limit
                                  value))
                               (if (= option :xform)
                                 (list
                                  'datascript.pull-parser/option-xform
                                  value)
                                 (let [default-form
                                       (if (nil? value)
                                         (list
                                          'Datascript_runtime.Data_value.Nil)
                                         (if (string? value)
                                           (list
                                            'Datascript_runtime.Data_value.String
                                            value)
                                           (if (keyword? value)
                                             (list
                                              'Datascript_runtime.Data_value.Keyword
                                              (str value))
                                             (if (= value true)
                                               (list
                                                'Datascript_runtime.Data_value.Bool
                                                true)
                                               (if (= value false)
                                                 (list
                                                  'Datascript_runtime.Data_value.Bool
                                                  false)
                                                 (list
                                                  'Datascript_runtime.Data_value.Int
                                                  value))))))]
                                   (list
                                    'datascript.pull-parser/option-default
                                    default-form))))))
                         source-attr
                         (if (vector? source-attr-form)
                           (first source-attr-form)
                           (if (seq? source-attr-form)
                             (second source-attr-form)
                             source-attr-form))
                         source-options
                         (if (vector? source-attr-form)
                           (loop [remaining (next source-attr-form)
                                  result []]
                             (if (empty? remaining)
                               result
                               (recur
                                (nnext remaining)
                                (conj
                                 result
                                 (source-option
                                  (first remaining)
                                  (second remaining))))))
                           (if (seq? source-attr-form)
                             [(source-option
                               (if (or
                                    (= 'limit (first source-attr-form))
                                    (= "limit" (first source-attr-form)))
                                 :limit
                                 :default)
                               (first (nnext source-attr-form)))]
                             []))
                         source-pattern (unquote-form (second entry))]
                     (if (vector? source-pattern)
                       (if (empty? source-options)
                         (list
                          'datascript.pull-parser/source-nested
                          source-attr
                          (list
                           'vec
                           (cons
                            'list
                            (map source-item source-pattern))))
                         (list
                          'datascript.pull-parser/source-nested-options
                          source-attr
                          (list
                           'vec
                           (cons 'list source-options))
                          (list
                           'vec
                           (cons
                            'list
                            (map source-item source-pattern)))))
                       (if (empty? source-options)
                         (list
                          'datascript.pull-parser/source-recursion
                          source-attr
                          (if (or
                               (= source-pattern '...)
                               (= source-pattern "..."))
                            'None
                            (list 'Some source-pattern)))
                         (list
                          'datascript.pull-parser/source-recursion-options
                          source-attr
                          (list
                           'vec
                           (cons 'list source-options))
                          (if (or
                               (= source-pattern '...)
                               (= source-pattern "..."))
                            'None
                            (list 'Some source-pattern)))))))
               (if (seq? item)
                 (let [operation (first item)
                       source-attr (second item)
                       value (first (nnext item))]
                   (if (or
                        (= operation 'limit)
                        (= operation "limit"))
                     (list
                      'datascript.pull-parser/source-options
                      source-attr
                      (list
                       'vector
                       (if (nil? value)
                         (list
                          'datascript.pull-parser/option-unlimited)
                         (list
                          'datascript.pull-parser/option-limit
                          value))))
                     (let [default-form
                           (if (nil? value)
                             (list
                              'Datascript_runtime.Data_value.Nil)
                             (if (string? value)
                               (list
                                'Datascript_runtime.Data_value.String
                                value)
                               (if (keyword? value)
                                 (list
                                  'Datascript_runtime.Data_value.Keyword
                                  (str value))
                                 (if (= value true)
                                   (list
                                    'Datascript_runtime.Data_value.Bool
                                    true)
                                   (if (= value false)
                                     (list
                                      'Datascript_runtime.Data_value.Bool
                                      false)
                                     (list
                                      'Datascript_runtime.Data_value.Int
                                      value))))))]
                       (list
                        'datascript.pull-parser/source-default
                        source-attr
                        default-form))))
               (if (vector? item)
                 (let [source-attr (first item)
                       source-option
                       (fn [option value]
                         (if (= option :as)
                           (list
                            'datascript.pull-parser/option-alias
                            value)
                           (if (= option :limit)
                             (if (nil? value)
                               (list
                                'datascript.pull-parser/option-unlimited)
                               (list
                                'datascript.pull-parser/option-limit
                                value))
                             (if (= option :xform)
                               (list
                                'datascript.pull-parser/option-xform
                                value)
                               (let [default-form
                                     (if (nil? value)
                                       (list
                                        'Datascript_runtime.Data_value.Nil)
                                       (if (string? value)
                                         (list
                                          'Datascript_runtime.Data_value.String
                                          value)
                                         (if (keyword? value)
                                           (list
                                            'Datascript_runtime.Data_value.Keyword
                                            (str value))
                                           (if (= value true)
                                             (list
                                              'Datascript_runtime.Data_value.Bool
                                              true)
                                             (if (= value false)
                                               (list
                                                'Datascript_runtime.Data_value.Bool
                                                false)
                                               (if
                                                (or
                                                 (symbol? value)
                                                 (seq? value))
                                                value
                                                (list
                                                 'Datascript_runtime.Data_value.Int
                                                 value)))))))]
                                 (list
                                  'datascript.pull-parser/option-default
                                  default-form))))))
                       option-forms
                       (loop [remaining (next item)
                              result []]
                         (if (empty? remaining)
                           result
                           (recur
                            (nnext remaining)
                            (conj
                             result
                             (source-option
                              (first remaining)
                              (second remaining))))))]
                   (list
                    'datascript.pull-parser/source-options
                    source-attr
                    (list
                     'vec
                     (cons 'list option-forms))))
               (if (or
                    (= item :*)
                    (= item '*)
                    (= item "*"))
                 'datascript.pull-parser/source-wildcard
                 (list
                  'datascript.pull-parser/source-attribute
                  item)))))))
           entity-ref
           (fn [entity-id]
             (if (vector? entity-id)
               (let [lookup-attr (first entity-id)
                     lookup-value (second entity-id)
                     value-form
                     (if (nil? lookup-value)
                       (list
                        'Datascript_runtime.Data_value.Nil)
                       (if (string? lookup-value)
                         (list
                          'Datascript_runtime.Data_value.String
                          lookup-value)
                         (if (keyword? lookup-value)
                           (list
                            'Datascript_runtime.Data_value.Keyword
                            (str lookup-value))
                           (if (or
                                (symbol? lookup-value)
                                (seq? lookup-value))
                             lookup-value
                             (list
                              'Datascript_runtime.Data_value.Int
                              lookup-value)))))]
                 (list
                  'Datascript_runtime.Data_value.Lookup_ref
                  (str lookup-attr)
                  value-form))
               (if (keyword? entity-id)
                 (list
                  'Datascript_runtime.Data_value.Ident
                  (str entity-id))
                 (list
                  'Datascript_runtime.Data_value.Entity_id
                  entity-id))))]
       (let [pattern (unquote-form pattern)
             source
             (if (vector? pattern)
               (list
                'vec
                (cons 'list (map source-item pattern)))
               pattern)
             entity-refs
             (if (vector? entity-ids)
               (list
                'vec
                (cons
                 'list
                 (reduce
                  (fn [refs entity-id]
                    (conj refs (entity-ref entity-id)))
                  []
                  entity-ids)))
               (list
                'datascript.pull-api/entity-ids-to-refs
                entity-ids))]
         (if (empty? options)
           (list
            'datascript.pull-api/pull-many-source
            (list
             'datascript.db/database-view
             database)
            source
            entity-refs)
           (list
            'datascript.pull-api/pull-many-source-with-options
            (list
             'datascript.db/database-view
             database)
            source
            entity-refs
            (first options))))))}
  ([^datascript.db/database-view database
    ^:vector<datascript.pull-parser/pull-source-item> source
    ^:vector<Datascript_runtime.Data_value.entity_ref> entity-refs]
   (pull-many-source database source entity-refs))
  ([^datascript.db/database-view database
    ^:vector<datascript.pull-parser/pull-source-item> source
    ^:vector<Datascript_runtime.Data_value.entity_ref> entity-refs
    ^PullOptions options]
   (pull-many-source-with-options
    database source entity-refs options)))
