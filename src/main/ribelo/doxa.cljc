(ns ribelo.doxa
  (:refer-clojure :exclude [-next -first filter])
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.extend]
   [ribelo.doxa.protocols :as p]
   [ribelo.doxa.pull-api :refer [-pull -mpull]]
   [ribelo.doxa.query :refer [-q -mq]]
   [ribelo.doxa.pick-api :refer [-pick]]
   [ribelo.doxa.util :as u]))

(comment
  (require '[taoensso.encore :as enc])
  (require '[clj-async-profiler.core :as prof]))

#?(:clj (set! *warn-on-reflection* true))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def dx? u/-dx?)
(def normalize u/-normalize)
(def denormalize u/-denormalize)
(def pull -pull)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def mpull -mpull)
(def q -q)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def mq -mq)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pick -pick)

;; An internal function that is used to commit changes to the database.
(defmulti -submit-commit
  "'-submit-commit', is used to commit changes to the in-memory database. It's
  an internal function, not intended for public use.

  # Arguments

  - `db`: the current state of the in-memory database.
  - `tx` (^clojure.lang.IPersistentVector): This is a persistent vector in
  Clojure that holds the transaction information. 

  The dispatch function provided to `defmulti` is a function that takes the
  same arguments as '-submit-commit' and returns a value. This returned value
  is used to decide which method implementation will be used for a given set of
  arguments.

  # Returns

  - This function return a updated db. Its purpose is to commit the
  changes given by the transaction 'tx' to the database `_db`.

  # Examples

  This function's usage will highly depend on how it has been designed to
  interact with your specific in-memory database keeping in mind the
  transactional context. Therefore, providing a generalizable example wouldn't
  be feasible."
  (fn [_db ^clojure.lang.IPersistentVector tx] (nth tx 0)))

(defmulti -submit-failure
  "This Clojure code consists of a multifunction for defining custom errors in
  response to specific types of transactions in a Doxa database.

  # Arguments
  - tx: This should be a vector. The first element of the vector is read as the
  transaction type, while the second element and onwards may hold relevant
  transaction data.
  - msg: This is an optional argument that specifies a custom error message,
  replacing any default message."
  (fn
    ([tx] (nth tx 0))
    ([tx _msg] (nth tx 0))))

;; This function serves as a default error handler for a `submit` operation.
(defmethod -submit-failure :default
  ([tx] (throw (ex-info "Using default submit error handler, consider using your own!" {:tx tx})))
  ([tx msg] (throw (ex-info msg {:tx tx}))))

;; The `defmethod -submit-failure :dx/put` is a method implementation of
;; '-submit-commit' multifn, specifically designated for :dx/put dispatch
;; value. This method implementation plays a crucial role when an error occurs
;; during a database commit operation. It's also an internal function, not
;; designed for direct public use.
(defmethod -submit-failure :dx/put
  ([tx]
   (throw (ex-info "Invalid put commit" {:tx tx})))
  ([tx msg]
   (throw (ex-info msg {:tx tx}))))

;; This method provides the implementation for the `:dx/put` method of
;; `-submit-commit`. It is used to effectively 'put' or commit changes to the
;; specified in-memory database. It's also an internal function, not designed
;; for direct public use.
(defmethod -submit-commit :dx/put
  [dx tx]
  (let [cnt (count tx)]
    (case cnt
      2 (let [x (nth tx 1)]
          (cond
            (u/-entity? x)
            (u/-put-entity dx x)

            (u/-entities? x)
            (u/-put-entities dx x)

            :else
            (-submit-failure tx)))

      (let [ref (nth tx 1)
            tid (nth ref 0)
            eid (nth ref 1)]

        (when-not (u/-ref-lookup? ref)
          (-submit-failure tx))

        (case cnt
          3 (let [m (nth tx 2)]
              (if-let [ref (u/-entity-ref m)]
                (if (ex/kw-identical? eid ref)
                  (u/-put-entity dx m)
                  (-submit-failure tx))
                (if (map? m)
                  (u/-put-entity dx (assoc m tid eid))
                  (-submit-failure tx))))

          4 (let [k (nth tx 2)
                  v (nth tx 3)]
              (if (u/-entity? v)
                (-> (u/-safe-put-kv dx ref k (u/-entity-ref v))
                    (u/-put-entity (u/-safe-merge v (u/-key->rvd k) ref)))

                (if-let [ids (u/-entities-refs v)]
                  (-> (u/-safe-put-kv dx ref k ids)
                      (u/-put-entities (mapv (fn [m] (assoc m (u/-key->rvd k) ref)) v)))

                  (if (u/-ref-lookup? v)
                    (if (dx v)
                      (u/-safe-put-kv dx ref k v)
                      (-submit-failure tx))
                    (u/-safe-put-kv dx ref k v)))))

          (if (pos? cnt)
            (apply u/-safe-put-kvs dx ref (into [] (drop 2) tx))
            (-submit-failure tx)))))))

;; "This method provides the implementation for the `:dx/merge` method of
;; `-submit-commit`. It is used to effectively 'merge' data with specified
;; in-memory database. It's also an internal function, not designed for direct
;; public use."
(defmethod -submit-commit :dx/merge
  [dx tx]
  (let [cnt (count tx)]
    (case cnt
      2 (let [x (nth tx 1)]
          (cond
            (u/-entity? x)
            (u/-merge-entity dx x)

            (u/-entities? x)
            (u/-merge-entities dx x)

            :else
            (-submit-failure tx)))

      (let [ref (nth tx 1)
            tid (nth ref 0)
            eid (nth ref 1)]

        (when-not (u/-ref-lookup? ref)
          (-submit-failure tx))

        (case cnt
          3 (let [m (nth tx 2)]
              (if-let [ref (u/-entity-ref m)]
                (if (ex/kw-identical? eid ref)
                  (u/-merge-entity dx m)
                  (-submit-failure tx))
                (if (map? m)
                  (u/-merge-entity dx (assoc m tid eid))
                  (-submit-failure tx))))

          4 (let [k (nth tx 2)
                  v (nth tx 3)]
              (if (u/-entity? v)
                (-> (u/-safe-put-kv dx ref k (u/-entity-ref v))
                    (u/-merge-entity v))

                (if-let [ids (u/-entities-refs v)]
                  (-> (u/-safe-put-kv dx ref k ids)
                      (u/-merge-entities v))

                  (if (u/-ref-lookup? v)
                    (if (dx v)
                      (u/-safe-put-kv dx ref k v)
                      (-submit-failure tx))
                    (u/-safe-merge-kv dx ref k v)))))

          (if (pos? cnt)
            (apply u/-safe-put-kvs dx ref (into [] (drop 2) tx))
            (-submit-failure tx)))))))

;; "The `defmethod -submit-failure :dx/put` is a method implementation of
;; '-submit-commit' multifn, specifically designated for :dx/delete dispatch
;; value. This method implementation plays a crucial role when an error occurs
;; during a database delete operation. It's also an internal function, not
;; designed for direct public use."
(defmethod -submit-failure :dx/delete
  ([tx]
   (throw (ex-info "Invalid delete commit" {:tx tx})))
  ([tx msg]
   (throw (ex-info msg {:tx tx}))))

;; "This method provides the implementation for the `:dx/delete` method of
;; `-submit-commit`. It is used to effectively 'delete' data in specified
;; in-memory database. It's also an internal function, not designed for direct
;; public use."
(defmethod -submit-commit :dx/delete
  [dx tx]
  (let [cnt (count tx)]
    (case cnt
      2 (let [x (nth tx 1)]
          (cond
            (map? x)
            (u/-delete-entity dx (u/-entity-ref x))

            (u/-ref-lookup? x)
            (u/-delete-entity dx x)

            (keyword? x)
            (u/-delete-table dx x)))

      3 (let [ref (nth tx 1)
              k (nth tx 2)]
          (if (vector? k)
            (ex/loop-it [j k :let [acc dx]]
                        #_{:clj-kondo/ignore [:unexpected-recur :invalid-arity]}
                        (recur (u/-delete-key acc ref j))
                        acc)

            (u/-delete-key dx ref k)))

      4 (let [ref (nth tx 1)
              k (nth tx 2)
              x (nth tx 3)]
          (u/-delete-val dx ref k x)))))

;; "This method provides the implementation for the `:dx/delete` method of
;; `-submit-commit`. It is used to effectively 'update' data in specified
;; in-memory database. It's also an internal function, not designed for direct
;; public use."
(defmethod -submit-commit :dx/update
  [dx tx]
  (let [cnt (count tx)
        ref (nth tx 1)]
    (if (>= cnt 3)
      (let [k-or-f (nth tx 2)]
        (cond
          (keyword? k-or-f)
          (let [k k-or-f
                f (nth tx 3)
                x (get-in dx [ref k])]
            (p/-put dx ref (assoc (dx ref {}) k (apply f x (drop 4 tx)))))

          (fn? k-or-f)
          (let [f k-or-f
                m (dx ref)]
            (p/-put dx ref (apply f m (drop 3 tx))))

          :else
          (-submit-failure tx)))
      (-submit-failure tx))))

;; "This method provides the implementation for the `:dx/delete` method of
;; `-submit-commit`. It is used match data with specified in-memory database.
;; It's also an internal function, not designed for direct public use."
(defmethod -submit-commit :dx/match
  [dx tx]
  (let [cnt (count tx)
        ref-or-map (nth tx 1)]
    (case cnt
      2 (if (map? ref-or-map)
          (if-let [ref (u/-entity-ref ref-or-map)]
            (let [e (dx ref)]
              (reduce-kv
               (fn [_ k v]
                 (if (= v (get e k)) true (reduced false)))
               false
               (dx ref)))
            (-submit-failure tx))
          (-submit-failure tx))

      (let [x (nth tx 2)
            e (dx ref-or-map)]
        (case cnt
          3 (cond
              (fn? x)
              (x e)

              (map? x)
              (reduce-kv
               (fn [_ k v]
                 (if (= v (get e k)) true (reduced false)))
               false
               x))

          4 (let [v (nth tx 3)]
              (if (fn? v)
                (v (get e x))
                (if-not (fn? x)
                  (let [v' (get e x)]
                    (or (= v v')
                        (when (coll? v') (ex/some #{v} v'))))
                  (-submit-failure tx "function should be the 3rd element")))))))))

(defn commit
  "This function, `commit`, is designed to handle the transactions to be
  processed within the Doxa database.

  # Arguments
  - `dx`: Doxa database where the transactions will be processed.
  - `txs`: Sequence of transactions to be committed. Each transaction is an
  array with a keyword at the first position representing the kind of
  transaction.
  - `meta'` - (Optional) Any metadata associated with the operation.

  This function goes through each transaction and depending upon its kind,
  commits it to the Doxa database. It has built-in error checking
  functionalities to prevent the application from crashing when encountering
  invalid transactions in the list. Once all transactions are processed, any
  listeners in the database are notified with the updated state.

  This function also handles caching and reindexing in the database, which are
  imperative for performance improvement and efficient data retrieval.

  # Examples

  ```clojure
  (def dx (empty-dx))
  (def txs [[:dx/put {:dx/id 1 :name \"Alice\"}] [:dx/put {:dx/id 2 :name \"Bob\"}]])
  (commit dx txs)
  ; Updated Doxa database with inserted data {\"Alice\", \"Bob\"}.
  ```"
  ([dx txs] (commit dx txs nil))
  ([dx txs meta']
   (let [txs (if (and (vector? txs) (vector? (first txs))) txs [txs])
         dx'
         (ex/loop-it [tx txs :let [acc (p/-clear-tx dx) match? true]]
                     (let [kind (nth tx 0)]
                       (if (ex/kw-identical? :dx/match kind)
                         #_{:clj-kondo/ignore [:invalid-arity]}
                         (recur acc (-submit-commit acc tx))
                         (if match?
                           (if-let [db' (-submit-commit acc tx)]
                             #_{:clj-kondo/ignore [:invalid-arity]}
                             (recur db' match?)
                             (throw (ex-info "db is nil!" {})))
                           #_{:clj-kondo/ignore [:invalid-arity]}
                           (recur acc match?))))
                     acc)
         dx' (-> (p/-set-cache! dx' (p/-refresh (some-> (p/-cache dx') deref) (p/-tx dx'))) (p/-reindex))]
     (when-let [listeners (some-> dx' meta ::listeners deref)]
       (ex/run! (fn [[_ f]] (f dx')) listeners))
     dx')))

(defn commit!
  "This function is intended to commit transactions to the given db in place.
  For more look at [commit]."
  [conn_ txs]
  (swap! conn_ commit txs))

(defn dx-with
  "This function is to perform a batch update or insertion in a Doxa (DX)
  database. It takes a Doxa instance and a vector of maps, which represent the
  data which will be merged into the Doxa instance. If the Doxa instance is not
  provided, it will create a new one.

  # Arguments

  - `dx`: (optional, map) the current Doxa instance (database state). If it's
  not passed, the function will use an empty map `{}`
  - `data`: (vector of maps) each map represents an entity to be added.

  # Returns
  - It returns a new Doxa instance (a new database state) with the data merged
  in.

  # Examples

  ```clojure
  (def data [{:id 1, :author \"Author 1\", :book \"Book 1\"} {:id 2, :author \"Author 2\", :book \"Book 2\"}])

  (dx-with data)
  ; => Returns a Doxa instance containing the provided data

  (def dx {:example \"Existing data\"})

  (dx-with dx data)
  ; => Returns a Doxa instance merging the existing Doxa instance and the new data
  ```
  "
  ([data] (dx-with {} data))
  ([dx data]
   (commit dx (mapv (fn [m] [:dx/merge m]) data))))

(defn create-dx
  "This function create a new instance of Doxa(a.k.a. dx), an in-memory
  immutable database.

  # Arguments

  - `empty-db`: (`map`) represents an optional map that will be populated by the
  `data` parameter. Has a default value of `{}` if left undefined.

  - `data`: (`seq`) contains an optional sequence of data (which can be queries, rules or facts)
  to populate the `empty-db`. Has a default value of `[]` if left undefined.

  - `meta`: (`map`) is used for any additional metadata that should be associated
  with the DX instance. By default, will provide an atom enclosing an empty map
  to capture any potential listeners. 

  # Returns

  - Returns a new database by merging the data and metadata into the passed empty-db map or a default empty Db. If no data is passed, it returns the updated meta passed to it or a default meta.

  # Examples

  ```clojure
  (create-dx)
  ; => returns a new db with default meta

  (create-dx {} [{:id 1, :name \"doxa\"}] {:meta-key \"meta-value\"})
  ; => {[:id 1] {:id 1, :name \"doxa\"}}
  ```"
  ([] (create-dx {}))
  ([empty-db]
   (create-dx empty-db []))
  ([empty-db data]
   (create-dx empty-db data {}))
  ([empty-db data meta]
   (let [default-meta {::listeners (atom {})}]
     (if (not-empty data)
       (vary-meta (dx-with empty-db data) merge default-meta meta)
       (vary-meta empty-db merge default-meta meta)))))
{[:id 1] {:id 1, :name "doxa"}}

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn connect!
  "This function is used to establish a connection in a data-oriented context.
  This function's usage would largely depend on specific use-case and
  implementation of `p/-connect`"
  ([dx]
   (p/-connect dx))
  ([dx x]
   (p/-connect dx x)))

(declare entity)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn listen!
  "This function registers a listener for a Doxa datastore and returns another
  function to remove the listener once it's no longer needed.

  # Arguments

  - `dx`: (atom) a Doxa instance where listeners are registered. It should be
  an atom containing a Doxa database
  - `k`: (keyword) unique identifier of the listener. Makes sure your listener
  can be later identified
  - `f`: (function) listener's function. It would be invoked with Doxa instance
  every time a transaction is committed in `dx`

  # Returns

  - a function with no arguments that would remove the listener `k` when called

  # Examples

  ```clojure
  (def dx (create-dx))
  (defn test-listener [dx] (println \"Something has changed\"))
  (def remove-listener (listen! dx :test-listener test-listener))
  ; After every committed transaction in `dx`, \"Something has changed\" will be printed

  ; when you want to stop observing
  (remove-listener)
  ```
  
  # Exception

  - this function will throw an exception if `dx` does not have a metadata
  entry for ::listeners. The exception's data will contain the metadata of
  `dx`."
  [dx k f]
  (if-let [atm_ (some-> dx meta ::listeners)]
    (do (swap! atm_ assoc k f)
        (fn [] (swap! atm_ dissoc k)))
    (throw (ex-info "cannot add the listener, the atom does not exist" {:meta (meta dx)}))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn unlisten!
  "This function is designed to remove a specific listener from the doxa. 

  # Arguments 

  - `dx`: (Doxa) - Doxa instance from which we want to remove a listener
  - `k`: any valid Clojure value 

  # Returns
  If successful, it returns `true` if listener exists or `false`, if not.

  Otherwise, it throws an exception with a message that indicates the listener
  cannot be removed because the atom (in which listeners are stored) does not
  exist. The metadata of the Doxa instance is attached to the exception data.

  # Example

  ```clojure
  (unlisten! dx :listener-key)
  ;; => true
  ```"
  [dx k]
  (if-let [atm_ (some-> dx meta ::listeners)]
    (if (get @atm_ k)
      (do (swap! atm_ dissoc k) true)
      false)
    (throw (ex-info "cannot remove the listener, the atom does not exist" {:meta (meta dx)}))))

(defn -entity-lookup
  "This function is used for looking up entities in the Doxa database, with an
  option to denormalize the references. It's an internal function, not intended
  for public use."
  [dx ref k denormalize?]
  (let [e (dx ref)]
    (if denormalize?
      (when-let [x (get e k)]
        (if (u/-ref-lookup? x)
          (entity dx ref {:denormalize? denormalize?})
          (if (u/-ref-lookups? x)
            (into (empty x) (fn [ref] (entity dx ref {:denormalize? denormalize?})) x)
            x)))
      (get e k))))

(defn entity
  "This function, `entity`, is a core part of the Doxa database. It provides a
  dynamic view on a specific entity in the database. The function is
  overloaded, meaning it can be called with different numbers of arguments.

    The function uses the `reify` macro to create an instance of an anonymous
  type that implements specific interfaces. This allows the function to return
  a dynamic view on an entity, which can be interacted with as if it were a
  map.

  The interfaces implemented are different for Clojure and ClojureScript due to
  differences in the language.

  # Clojure, interfaces

  - `clojure.lang.ILookup`: This allows the entity to be used like a map, where
  a key can be used to retrieve a value.

  - `clojure.lang.Associative`: This allows a key-value pair to be added to the
  entity.

  - `clojure.lang.IDeref`: This allows the entity to be dereferenced, returning
  the entity itself.

  # ClojureScript interfaces 
  - `IMap`: This allows the entity to be used like a map.

  - `IPrintWithWriter`: This allows the entity to be printed.

  - `ILookup`: This allows a key to be used to retrieve a value from the
  entity.

  - `IAssociative`: This allows a key-value pair to be added to the entity.

  - `IDeref`: This allows the entity to be dereferenced, returning the entity
  itself.


  # Arguments

  - `dx`: (Doxa) The Doxa database instance.
  - `ref`: ([k v]) The reference to the entity in the database.
  - An optional map with the key `denormalize?` which indicates whether the
  entity should be denormalized."
  ([dx ref] (entity dx ref {}))
  ([dx ref {:keys [denormalize?]}]
   {:pre [(u/-probably-dx? dx)]}
   (when (dx ref)
     #?(:clj
        (reify
          clojure.lang.ILookup
          (valAt [_ k]
            (-entity-lookup dx ref k denormalize?))

          clojure.lang.Associative
          (assoc [_ k v]
            (entity (commit! dx [:dx/put ref k v]) ref))

          clojure.lang.IDeref
          (deref [_]
            (when-let [e (dx ref)]
              (if denormalize?
                (denormalize dx e)
                e))))

        :cljs
        (reify
          IMap

          IPrintWithWriter
          (-pr-writer [this writer opts]
            (-pr-writer (-deref this) writer opts))

          ILookup
          (-lookup [_ k]
            (-entity-lookup dx ref k denormalize?))

          IAssociative
          (-assoc [_ k v]
            (entity (commit! dx [:dx/put ref k v]) ref))

          IDeref
          (-deref [_]
            (when-let [e (dx ref)]
              (if denormalize?
                (denormalize dx e)
                e))))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn table
  "This function retrieves a specific table from the Doxa database.

  # Arguments
  - `dx`: (Doxa instance) the Doxa database instance from which to retrieve the
  table.
  - `table`: (keyword) the keyword identifier of the table to retrieve. The
  format is `:table-namespace/id`.

  # Returns
  - If the table exists in the Doxa instance, it returns a new Doxa instance
  containing only the specified table and its associated data. 
  - If the table does not exist, it returns an empty Doxa instance.

  # Examples

  ```clojure
  (def dx (doxa/db {:table-namespace/id {:a 1, :b 2}}))
  (table dx :table-namespace/id)
  ;; => TODO:
  ```
  ```clojure
  (def dx (doxa/db {:table-namespace/id {:a 1, :b 2}}))
  (table dx :non-existent-table)
  ;; => TODO:
  ```"
  [dx table]
  (if-let [xs (some-> dx p/-index (get table))]
    (ex/select-keys dx xs)
    (empty dx)))

;; (defn- -filter [pred dx]
;;   (persistent!
;;     (reduce
;;       (fn [acc ref]
;;         (let [e (entity dx ref {:denormalize? true})]
;;           (if (pred e) (assoc! acc ref e) acc)))
;;       (transient {})
;;       (-keys dx))))

;; (defn- -view [dx f]
;;   (let [cache_ (volatile! nil)]
;;     (reify
;;       clojure.lang.IRef
;;       (deref [_]
;;         (if-let [snapshot @cache_]
;;           snapshot
;;           (vreset! cache_ (f dx)))))))

;; (defn filter [pred dx]
;;   (let [view (-view dx (partial -filter pred))
;;         err (ex-info "you shouldn't use put on view" {:dx dx})]
;;     (reify
;;       clojure.lang.IRef
;;       (deref [_]
;;         @view)

;;       clojure.lang.IKVReduce
;;       (kvreduce [_ f init]
;;         (reduce-kv f init (-filter pred dx)))

;;       i/IDoxa
;;       (i/-put [_ _ _]
;;         (throw err))
;;       (i/-put [_ _ _ _]
;;         (throw err))

;;       (i/-pick [this e]
;;         (i/-pick (-filter pred dx) e))
;;       (i/-pick [this e a]
;;         (i/-pick (-filter pred dx) e a))
;;       (i/-pick [this e a v]
;;         (i/-pick (-filter pred dx) e a v))

;;       (i/-del [_ _]
;;         (throw err))
;;       (i/-del [_ _ _]
;;         (throw err))
;;       (i/-del [_ _ _ _]
;;         (throw err))

;;       (-keys [this]
;;         (-keys (-filter pred dx)))

;;       (-entities [this]
;;         (vals (-filter pred dx))))))
