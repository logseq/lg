(ns datascript.datafy)

(type-variant navigation-key
  (NavigationAttribute :keyword)
  (NavigationIndex :int))

(type-record datafied-entity
  (database :datascript.db/database-view)
  (values
   :map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>))

(type-record datafied-entities
  (database :datascript.db/database-view)
  (values :vector<Datascript_runtime.Data_value.t>))

(type-variant navigation-value
  NavigationMissing
  (NavigationEntity :datascript.impl.entity/Entity)
  (NavigationEntities
   :datascript.db/database-view
   :vector<Datascript_runtime.Data_value.t>)
  (NavigationDatafiedEntity :datascript.datafy/datafied-entity)
  (NavigationDatafiedEntities :datascript.datafy/datafied-entities)
  (NavigationScalar :Datascript_runtime.Data_value.t))

(signature datascript.datafy/entity-navigation
  :fn<datascript.impl.entity/Entity;datascript.datafy/navigation-value>)

(signature datascript.datafy/datafy
  :fn<datascript.datafy/navigation-value;datascript.datafy/navigation-value>)

(signature datascript.datafy/lookup
  :fn<datascript.datafy/navigation-value;datascript.datafy/navigation-key;datascript.datafy/navigation-value>)

(signature datascript.datafy/nav
  :fn<datascript.datafy/navigation-value;datascript.datafy/navigation-key;datascript.datafy/navigation-value;datascript.datafy/navigation-value>)

(signature datascript.datafy/pulled-entity-id
  :fn<Datascript_runtime.Data_value.t;option<int>>)

(signature datascript.datafy/navigation-entity-id
  :fn<datascript.datafy/navigation-value;option<int>>)

(signature datascript.datafy/empty-pulled-entities
  :fn<vector<Datascript_runtime.Data_value.t>>)

(signature datascript.datafy/empty-entity-ids
  :fn<vector<int>>)

(signature datascript.datafy/navigation-entity-ids
  :fn<datascript.datafy/navigation-value;vector<int>>)

(signature datascript.datafy/navigation-scalar
  :fn<datascript.datafy/navigation-value;option<Datascript_runtime.Data_value.t>>)
