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

(def dx? u/-dx?)
(def normalize u/-normalize)
(def denormalize u/-denormalize)
(def pull -pull)
(def mpull -mpull)
(def q -q)
(def mq -mq)
(def pick -pick)

(defmulti -submit-commit (fn [_db ^clojure.lang.IPersistentVector tx] (nth tx 0)))

(defmulti -submit-failure
  (fn
    ([tx] (nth tx 0))
    ([tx _msg] (nth tx 0))))

(defmethod -submit-failure :default
  ([tx] (throw (ex-info "Using default submit error handler, consider using your own!" {:tx tx})))
  ([tx msg] (throw (ex-info msg {:tx tx}))))

(defmethod -submit-failure :dx/put
  ([tx]
   (throw (ex-info "Invalid put commit" {:tx tx})))
  ([tx msg]
   (throw (ex-info msg {:tx tx}))))

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
                (if (ex/-kw-identical? eid ref)
                  (u/-put-entity dx m)
                  (-submit-failure tx))
                (if (map? m)
                  (u/-put-entity dx (ex/-assoc* m tid eid))
                  (-submit-failure tx))))

          4 (let [k (nth tx 2)
                  v (nth tx 3)]
              (if (u/-entity? v)
                (-> (u/-safe-put-kv dx ref k (u/-entity-ref v))
                    (u/-put-entity (u/-safe-assoc v (u/-key->rev k) ref)))

                (if-let [ids (u/-entities-refs v)]
                  (-> (u/-safe-put-kv dx ref k ids)
                      (u/-put-entities (ex/-mapv (fn [m] (ex/-assoc* m (u/-key->rev k) ref)) v)))

                  (if (u/-ref-lookup? v)
                    (if (ex/-get* dx v)
                      (u/-safe-put-kv dx ref k v)
                      (-submit-failure tx))
                    (u/-safe-put-kv dx ref k v)))))

          (if (pos? cnt)
            (apply u/-safe-put-kvs dx ref (into [] (drop 2) tx))
            (-submit-failure tx)))))))

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
                (if (ex/-kw-identical? eid ref)
                  (u/-merge-entity dx m)
                  (-submit-failure tx))
                (if (map? m)
                  (u/-merge-entity dx (ex/-assoc* m tid eid))
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
                    (if (ex/-get* dx v)
                      (u/-safe-put-kv dx ref k v)
                      (-submit-failure tx))
                    (u/-safe-put-kv dx ref k v)))))

          (if (pos? cnt)
            (apply u/-safe-put-kvs dx ref (into [] (drop 2) tx))
            (-submit-failure tx)))))))

(defmethod -submit-failure :dx/delete
  ([tx]
   (throw (ex-info "Invalid delete commit" {:tx tx})))
  ([tx msg]
   (throw (ex-info msg {:tx tx}))))

(defmethod -submit-commit :dx/delete
  [dx tx]
  (let [cnt (count tx)]
    (case cnt
      2 (let [x (nth tx 1)
              ref (u/-entity-ref x)]
          (cond
            (some? ref)
            (u/-delete-entity dx ref)

            (u/-ref-lookup? x)
            (u/-delete-entity dx x)))

      3 (let [ref (nth tx 1)
              k (nth tx 2)]
          (u/-clearing-delete dx [ref k]))

      4 (let [ref (nth tx 1)
              k (nth tx 2)
              x (nth tx 3)]
          (u/-clearing-delete dx [ref k] x)))))

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
                x (ex/-get-in dx [ref k])]
            (p/-put dx ref (ex/-assoc* (ex/-get* dx ref) k (apply f x (drop 4 tx)))))

          (fn? k-or-f)
          (let [f k-or-f
                m (ex/-get* dx ref)]
            (p/-put dx ref (apply f m (drop 3 tx))))

          :else
          (-submit-failure tx)))
      (-submit-failure tx))))

(defmethod -submit-commit :dx/match
  [dx tx]
  (let [cnt (count tx)
        ref (nth tx 1)
        e (ex/-get* dx ref)
        x (nth tx 2)]
    (case cnt
      3 (cond
          (fn? x)
          (x e)

          (map? x)
          (reduce-kv
            (fn [_ k v]
              (if (= v (ex/-get e k)) true (reduced false)))
            false
            x))

      4 (let [v (nth tx 3)]
          (if (fn? v)
            (v (ex/-get e x))
            (if-not (fn? x)
              (let [v' (ex/-get e x)]
                (or (= v v')
                    (when (coll? v') (ex/-some #{v} v'))))
              (-submit-failure tx "function should be the 3rd element")))))))

(defn commit
  ([dx txs] (commit dx txs nil))
  ([dx txs meta']
   (let [txs (if (and (vector? txs) (vector? (ex/-first txs))) txs [txs])
         dx'
         (ex/-loop [tx txs :let [acc (p/-clear-tx dx) match? true]]
           (let [kind (nth tx 0)]
             (if (ex/-kw-identical? :dx/match kind)
               (recur acc (-submit-commit acc tx))
               (if match?
                 (if-let [db' (-submit-commit acc tx)]
                   (recur db' match?)
                   (throw (ex-info "db is nil!" {})))
                 (recur acc match?))))
           acc)]
     (-> (p/-set-cache! dx' (p/-refresh (some-> (p/-cache dx') deref) (p/-tx dx'))) (p/-reindex)))))

(defn commit! [conn_ txs]
  (swap! conn_ commit txs))

(defn dx-with
  ([data] (dx-with {} data))
  ([dx data]
   (commit dx (ex/-mapv (fn [m] [:dx/merge m]) data))))

(defn create-dx
  ([] (create-dx {}))
  ([empty-db]
   (create-dx empty-db []))
  ([empty-db data]
   (create-dx empty-db data {}))
  ([empty-db data meta]
   (if (not-empty data)
     (vary-meta (dx-with empty-db data) ex/-merge meta)
     (vary-meta empty-db ex/-merge meta))))

(defn connect!
  ([dx]
   (p/-connect dx))
  ([dx x]
   (p/-connect dx x)))

(declare entity)

(defn -entity-lookup [dx ref k denormalize?]
  (let [e (ex/-get* dx ref)]
    (if denormalize?
      (when-let [x (ex/-get e k)]
        (if (u/-ref-lookup? x)
          (entity dx ref {:denormalize? denormalize?})
          (if (u/-ref-lookups? x)
            (into (empty x) (fn [ref] (entity dx ref {:denormalize? denormalize?})) x)
            x)))
      (ex/-get e k))))

(defn entity
  ([dx ref] (entity dx ref {}))
  ([dx ref {:keys [denormalize?]}]
   {:pre [(u/-probably-dx? dx)]}
   (when (ex/-get* dx ref)
     #?(:clj
        (reify
          clojure.lang.ILookup
          (valAt [this k]
            (-entity-lookup dx ref k denormalize?))

          clojure.lang.Associative
          (assoc [this k v]
              (entity (commit! dx [:dx/put ref k v]) ref))

          clojure.lang.IDeref
          (deref [this]
            (when-let [e (ex/-get* dx ref)]
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
          (-lookup [this k]
            (-entity-lookup dx ref k denormalize?))

          IAssociative
          (-assoc [this k v]
            (entity (commit! dx [:dx/put ref k v]) ref))

          IDeref
          (-deref [this]
            (when-let [e (ex/-get* dx ref)]
              (if denormalize?
                (denormalize dx e)
                e))))))))

(defn table [dx table]
  (if-let [xs (some-> dx p/-index (ex/-get table))]
    (ex/-loop [ref xs :let [acc (transient {})]]
      (recur (ex/-assoc!* acc ref (entity dx ref)))
      (persistent! acc))
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
