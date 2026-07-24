(ns ^:no-doc datascript.pull-api
  (:require
   [datascript.pull-parser :as dpp]
   [datascript.db :as db]
   [me.tonsky.persistent-sorted-set :as set]))

(type-variant pulled-value
  (PulledScalar :Datascript_runtime.Data_value.t)
  (PulledEntity :map<keyword;pulled-value>)
  (PulledMany :vector<pulled-value>))

(type-variant pull-visit
  (VisitAttr :int :keyword)
  (VisitWildcard :int)
  (VisitReverse :keyword :int))

(type-record PullContext
  (db :datascript.db/DB)
  (visitor :option<fn<pull-visit;unit>>))

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
  (values :map<keyword;pulled-value>)
  (pattern :datascript.pull-parser/PullPattern)
  (attr :option<datascript.pull-parser/pull-attr>)
  (attrs :vector<datascript.pull-parser/pull-attr>)
  (attr-index :int)
  (id :int))

(type-record AttrsState
  (seen :set<int>)
  (recursion-limits :map<int;int>)
  (values :map<keyword;pulled-value>)
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

(defn ^:Datascript_runtime.Data_value.t pulled-to-data
  [^pulled-value value]
  (match value
    (PulledScalar value) value
    (PulledMany values)
    (Datascript_runtime.Data_value.vector_of_vector_with
     pulled-to-data
     values)
    (PulledEntity values)
    (Datascript_runtime.Data_value.map_of_keyword_map_with
     pulled-to-data
     values)))

(signature datascript.pull-api/assoc-pulled-value
  :fn<map<keyword;pulled-value>;keyword;pulled-value;map<keyword;pulled-value>>)

(defn ^:map<keyword;pulled-value> assoc-pulled-value
  [^:map<keyword;pulled-value> values
   ^:keyword key
   ^pulled-value value]
  (Lg_runtime.Runtime_map.assoc_small_string values key value))

(signature datascript.pull-api/assoc-pulled-data
  :fn<map<keyword;Datascript_runtime.Data_value.t>;keyword;Datascript_runtime.Data_value.t;map<keyword;Datascript_runtime.Data_value.t>>)

(defn ^:map<keyword;Datascript_runtime.Data_value.t> assoc-pulled-data
  [^:map<keyword;Datascript_runtime.Data_value.t> values
   ^:keyword key
   ^:Datascript_runtime.Data_value.t value]
  (Lg_runtime.Runtime_map.assoc_small_string values key value))

(defn ^:option<datascript.db/Datom> cursor-datom
  [^DatomCursor cursor]
  (.-current cursor))

(defn ^DatomCursor next-cursor [^DatomCursor cursor]
  (match (seq-uncons (.-remaining cursor))
    None
    (record DatomCursor
      (current None)
      (remaining (.-remaining cursor)))
    (Some entry)
    (record DatomCursor
      (current (Some (tuple-get entry 0)))
      (remaining (tuple-get entry 1)))))

(defn ^:option<DatomCursor> non-empty-cursor [^DatomCursor cursor]
  (match (cursor-datom cursor)
    None None
    (Some _) (Some cursor)))

(defn ^ResultState frame-result
  [^:option<pulled-value> value ^:option<DatomCursor> datoms]
  (record ResultState
    (value value)
    (datoms datoms)))

(defn ^:option<pulled-value> non-empty-many
  [^:vector<pulled-value> values]
  (if (empty? values)
    None
    (Some (PulledMany values))))

(defn ^frame finish-multival-attr
  [^:vector<pulled-value> values
   ^DatomCursor cursor]
  (ResultFrame
   (frame-result
    (non-empty-many values)
    (non-empty-cursor cursor))))

(defn ^boolean cursor-matches-attr?
  [^DatomCursor cursor ^:keyword attr]
  (match (cursor-datom cursor)
    None false
    (Some datom) (= (.-a datom) attr)))

(defn ^datascript.db/Datom cursor-datom-exn
  [^DatomCursor cursor]
  (match (cursor-datom cursor)
    None
    (Stdlib.invalid_arg "Datom cursor is exhausted")
    (Some datom) datom))

(defn ^frame skip-multival-attr
  [^:vector<pulled-value> values
   ^datascript.pull-parser/PullAttrData data
   ^DatomCursor cursor]
  (loop [cursor cursor]
    (if (cursor-matches-attr? cursor (.-name data))
      (recur (next-cursor cursor))
      (ResultFrame
       (frame-result
        (Some (PulledMany values))
        (non-empty-cursor cursor))))))

(defn ^frame run-multival-attr
  [^:vector<pulled-value> values
   ^datascript.pull-parser/pull-attr attr
   ^DatomCursor cursor]
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

(defn ^frame run-multival-attr-frame [^MultivalAttrState state]
  (run-multival-attr
   (.-values state)
   (.-attr state)
   (.-datoms state)))

(defn visit [^PullContext context ^pull-visit event]
  :unit
  (match (.-visitor context)
    None (Stdlib.ignore 0)
    (Some visitor) (Stdlib.ignore (visitor event))))

(defn ^:option<DatomCursor> cursor-from-seq
  [^:seq<datascript.db/Datom> datoms]
  (match (seq-uncons datoms)
    None None
    (Some entry)
    (Some
     (record DatomCursor
       (current (Some (tuple-get entry 0)))
       (remaining (tuple-get entry 1))))))

(defn ^:option<DatomCursor> cursor-from-datoms
  [^:option<seq<datascript.db/Datom>> datoms]
  (match datoms
    None None
    (Some datoms) (cursor-from-seq datoms)))

(defn ^AttrsState attrs-state
  [^PullContext context
   ^:set<int> seen
   ^:map<int;int> recursion-limits
   ^datascript.pull-parser/PullPattern pattern
   ^int id]
  (let [database (.-db context)
        datoms
        (if (:wildcard pattern)
          (set/slice
           (.-eavt database)
           (db/datom-bound
            (Some id) None None None db/e0 db/tx0)
           (db/datom-bound
            (Some id) None None None db/e0 db/txmax))
          (match (:first-attr pattern)
            None None
            (Some first-attr)
            (match (:last-attr pattern)
              None None
              (Some last-attr)
              (let [from
                    (.-name (dpp/attr-data first-attr))
                    to
                    (.-name (dpp/attr-data last-attr))]
                (set/slice
                 (.-eavt database)
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
                  db/txmax))))))]
    (when (:wildcard pattern)
      (visit context (VisitWildcard id)))
    (record AttrsState
      (seen seen)
      (recursion-limits recursion-limits)
      (values {})
      (pattern pattern)
      (attr (first (:attrs pattern)))
      (resume-attr None)
      (attrs (:attrs pattern))
      (attr-index 1)
      (datoms (cursor-from-datoms datoms))
      (id id))))

(defn ^datascript.pull-parser/PullPattern attr-pattern
  [^datascript.pull-parser/pull-attr attr]
  (match (dpp/attr-pattern attr)
    (Some pattern) pattern
    None
    (Stdlib.invalid_arg
     "Pull reference attribute requires a nested pattern")))

(defn ^boolean auto-expanding?
  [^datascript.pull-parser/pull-attr attr]
  (let [data (dpp/attr-data attr)]
    (or
     (.-recursive data)
     (and
      (.-component data)
      (dpp/attr-pattern-wildcard attr)))))

(defn ^pulled-value cycle-entity [^int id]
  (let [^:map<keyword;pulled-value> values {}]
    (PulledEntity
     (assoc-pulled-value
      values
      :db/id
      (PulledScalar
       (Datascript_runtime.Data_value.Int id))))))

(defn ^frame expanding-ref-frame
  [^PullContext context
   ^:set<int> seen
   ^:map<int;int> recursion-limits
   ^datascript.pull-parser/PullPattern pattern
   ^datascript.pull-parser/pull-attr attr
   ^int id]
  (AttrsFrame
   (attrs-state
    context
    (conj seen id)
    recursion-limits
    (if (.-recursive (dpp/attr-data attr))
      pattern
      (attr-pattern attr))
    id)))

(defn ^frame ref-frame
  [^PullContext context
   ^:set<int> seen
   ^:map<int;int> recursion-limits
   ^datascript.pull-parser/PullPattern pattern
   ^datascript.pull-parser/pull-attr attr
   ^int id]
  (let [data (dpp/attr-data attr)]
    (if-not (auto-expanding? attr)
      (AttrsFrame
       (attrs-state
        context seen recursion-limits (attr-pattern attr) id))
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

(defn ^:option<int> ref-datom-id
  [^datascript.pull-parser/PullAttrData data
   ^datascript.db/Datom datom]
  (if (.-reverse data)
    (Some (.-e datom))
    (Datascript_runtime.Data_value.ref_value (.-v datom))))

(defn ^MultivalRefAttrState next-multival-ref-state
  [^MultivalRefAttrState state]
  (record MultivalRefAttrState
    (seen (.-seen state))
    (recursion-limits (.-recursion-limits state))
    (values (.-values state))
    (pattern (.-pattern state))
    (attr (.-attr state))
    (datoms (next-cursor (.-datoms state)))))

(defn ^frame finish-multival-ref
  [^MultivalRefAttrState state]
  (ResultFrame
   (frame-result
    (non-empty-many (.-values state))
    (non-empty-cursor (.-datoms state)))))

(defn ^frame skip-multival-ref
  [^MultivalRefAttrState state]
  (let [data (dpp/attr-data (.-attr state))]
    (match (cursor-datom (.-datoms state))
      None
      (ResultFrame
       (frame-result
        (Some
         (PulledMany (.-values state)))
        None))
      (Some datom)
      (if (= (.-a datom) (.-name data))
        (skip-multival-ref (next-multival-ref-state state))
        (ResultFrame
         (frame-result
          (Some
           (PulledMany (.-values state)))
          (Some (.-datoms state))))))))

(defn ^:vector<frame> run-multival-ref-frame
  [^PullContext context ^MultivalRefAttrState state]
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

(defn ^:option<datascript.pull-parser/pull-attr> attr-at-index
  [^:vector<datascript.pull-parser/pull-attr> attrs ^int index]
  (if (< index (count attrs))
    (Some (nth attrs index))
    None))

(defn ^:option<DatomCursor> next-parent-datoms
  [^:option<DatomCursor> child-datoms
   ^:option<DatomCursor> parent-datoms]
  (match child-datoms
    (Some datoms) (Some datoms)
    None
    (match parent-datoms
      None None
      (Some datoms)
      (non-empty-cursor (next-cursor datoms)))))

(defn ^:option<pulled-value> apply-attr-xform
  [^datascript.pull-parser/pull-attr attr
   ^:option<pulled-value> value]
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

(defn ^:map<keyword;pulled-value> merge-attr-value
  [^:map<keyword;pulled-value> values
   ^datascript.pull-parser/pull-attr attr
   ^:option<pulled-value> value]
  (match (apply-attr-xform attr value)
    None values
    (Some value)
    (assoc-pulled-value
     values (.-alias (dpp/attr-data attr)) value)))

(defn ^frame merge-attrs-result
  [^AttrsState state ^ResultState result]
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

(defn ^frame merge-multival-ref-result
  [^MultivalRefAttrState state ^ResultState result]
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

(defn ^frame merge-reverse-result
  [^ReverseAttrsState state ^ResultState result]
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

(defn ^frame merge-frame [^frame parent ^ResultState result]
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

(defn ^AttrsState attrs-state-with
  [^AttrsState state
   ^:map<keyword;pulled-value> values
   ^:option<datascript.pull-parser/pull-attr> attr
   ^:option<datascript.pull-parser/pull-attr> resume-attr
   ^int attr-index
   ^:option<DatomCursor> datoms]
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

(defn ^AttrsState advance-attrs-state
  [^AttrsState state
   ^:map<keyword;pulled-value> values
   ^:option<DatomCursor> datoms]
  (let [index (.-attr-index state)]
    (attrs-state-with
     state
     values
     (attr-at-index (.-attrs state) index)
     None
     (inc index)
     datoms)))

(defn ^frame reverse-attrs-frame [^AttrsState state]
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

(defn ^:vector<frame> start-attr-child
  [^PullContext context
   ^AttrsState state
   ^datascript.pull-parser/pull-attr attr
   ^:option<datascript.pull-parser/pull-attr> resume-attr
   ^DatomCursor cursor]
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

(defn ^:map<keyword;pulled-value> add-scalar-datom
  [^:map<keyword;pulled-value> values
   ^datascript.pull-parser/pull-attr attr
   ^datascript.db/Datom datom]
  (merge-attr-value
   values
   attr
   (Some (PulledScalar (.-v datom)))))

(defn ^:map<keyword;pulled-value> add-default
  [^:map<keyword;pulled-value> values
   ^datascript.pull-parser/pull-attr attr]
  (let [data (dpp/attr-data attr)]
    (match (.-default data)
      None values
      (Some value)
      (assoc-pulled-value
       values
       (.-alias data)
       (PulledScalar value)))))

(defn ^:map<keyword;pulled-value> add-missing-value
  [^:map<keyword;pulled-value> values
   ^datascript.pull-parser/pull-attr attr]
  (let [data (dpp/attr-data attr)]
    (match (.-default data)
      (Some value)
      (assoc-pulled-value
       values
       (.-alias data)
       (PulledScalar value))
      None
      (merge-attr-value values attr None))))

(defn ^AttrsState missing-attr-state
  [^PullContext context
   ^AttrsState state
   ^datascript.pull-parser/pull-attr attr]
  (let [data (dpp/attr-data attr)]
    (visit
     context
     (VisitAttr (.-id state) (.-name data)))
    (advance-attrs-state
     state
     (add-missing-value (.-values state) attr)
     (.-datoms state))))

(declare run-attrs-frame)

(defn ^:vector<frame> run-wildcard-attr
  [^PullContext context
   ^AttrsState state
   ^:option<datascript.pull-parser/pull-attr> explicit-attr
   ^DatomCursor cursor]
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

(defn ^:vector<frame> run-attrs-frame
  [^PullContext context ^AttrsState state]
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
                   (VisitAttr (.-id state) (.-name data)))
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

(defn ^ReverseAttrsState advance-reverse-state
  [^ReverseAttrsState state
   ^:map<keyword;pulled-value> values]
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

(defn ^:vector<frame> run-reverse-attrs-frame
  [^PullContext context ^ReverseAttrsState state]
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
           (db/-search
            (.-db context)
            None
            (Some (.-name data))
            (Some
             (Datascript_runtime.Data_value.Ref
              (.-id state)))
            None))]
      (visit
       context
       (VisitReverse (.-name data) (.-id state)))
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

(defn ^:vector<frame> run-frame
  [^PullContext context ^frame current]
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

(defn ^:map<keyword;Datascript_runtime.Data_value.t> pulled-map-to-data
  [^:map<keyword;pulled-value> values]
  (reduce-kv
   (fn [^:map<keyword;Datascript_runtime.Data_value.t> result
        ^:keyword key
        ^pulled-value value]
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

(defn ^:option<map<keyword;Datascript_runtime.Data_value.t>> pull
  [^datascript.db/DB database
   ^datascript.pull-parser/PullPattern pattern
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (if-some [eid (db/entid database entity-ref)]
    (let [context
          (record PullContext
            (db database)
            (visitor None))
          root
          (AttrsFrame
           (attrs-state
            context
            (set-of :int)
            {}
            pattern
            eid))]
      (match (run-stack context (list root))
        None None
        (Some (PulledEntity values))
        (Some (pulled-map-to-data values))
        (Some _)
        (Stdlib.invalid_arg
         "Root pull result is not an entity")))
    None))

(defn ^:vector<option<map<keyword;Datascript_runtime.Data_value.t>>> pull-many
  [^datascript.db/DB database
   ^datascript.pull-parser/PullPattern pattern
   ^:vector<Datascript_runtime.Data_value.entity_ref> entity-refs]
  (mapv
   (fn [^:Datascript_runtime.Data_value.entity_ref entity-ref]
     (pull database pattern entity-ref))
   entity-refs))
