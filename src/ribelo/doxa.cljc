(ns ribelo.doxa
  (:refer-clojure :exclude [-next -first filter])
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa.impl.protocols :as p]
   [ribelo.doxa.util :as u]))

(comment
  (require '[taoensso.encore :as enc])
  (require '[clj-async-profiler.core :as prof]))

#?(:clj (set! *warn-on-reflection* true))

(def dx? u/-dx?)
(def normalize u/-normalize)
(def denormalize u/-denormalize)

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
            (u/-put-entities dx x)))

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
                  (u/-put-entity dx (p/-put m tid eid))
                  (-submit-failure tx))))

          4 (let [k (nth tx 2)
                  v (nth tx 3)]
              (if (u/-entity? v)
                (-> (u/-safe-put-kv dx ref k (u/-entity-ref v))
                    (u/-put-entity v))

                (if-let [ids (u/-entities-refs v)]
                  (-> (u/-safe-put-kv dx ref k ids)
                      (u/-merge-entities v))

                  (if (u/-ref-lookup? v)
                    (if (p/-pick dx v)
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
                x (p/-pick dx ref k)]
            (p/-put dx ref k (apply f x (drop 4 tx))))

          (fn? k-or-f)
          (let [f k-or-f
                m (p/-pick dx ref)]
            (p/-put dx ref (apply f m (drop 3 tx))))

          :else
          (-submit-failure tx)))
      (-submit-failure tx))))

(defmethod -submit-commit :dx/match
  [dx tx]
  (let [cnt (count tx)
        ref (nth tx 1)
        e (p/-pick dx ref)
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

(defn- -commit-many
  ([dx txs] (-commit-many dx txs nil))
  ([dx txs meta']
   (let [it (ex/-iter txs)
         cache (p/-cache dx)]
     (loop [acc (p/-clear-tx dx) match? true]
       (if (.hasNext it)
         (if-let [tx (.next it)]
           (let [kind (nth tx 0)]
             (if (ex/-kw-identical? :dx/match kind)
               (recur acc (-submit-commit acc tx))
               (if match?
                 (let [db' (-submit-commit acc tx)]
                   (recur db' match?))
                 (recur acc match?))))
           (recur acc match?))
         (do
           (p/-refresh cache (p/-tx acc))
           acc))))))

(defn commit
  ([dx txs] (commit dx txs nil))
  ([dx txs meta']
   (if (vector? (nth txs 0))
     (-commit-many dx txs meta')
     (-commit-many dx [txs] meta'))))

(defn commit! [conn_ txs]
  (swap! conn_ commit txs))

(defn dx-with
  ([data] (dx-with {} data))
  ([dx data]
   (-commit-many dx (ex/-mapv (fn [m] [:dx/put m]) data))))

(defn create-dx
  ([] (create-dx {}))
  ([data]
   (create-dx {} data))
  ([empty-db data]
   (create-dx empty-db data {}))
  ([empty-db data opts]
   (if (not-empty data) (dx-with empty-db data) empty-db)))

(defn connect! [dx]
  (p/-connect dx))

(declare entity)

(defn -entity-lookup [dx ref k denormalize?]
  (let [e (p/-pick dx ref)]
    (if denormalize?
      (when-let [x (ex/-get e k)]
        (if (u/-ref-lookup? x)
          (entity dx ref {:denormalize? denormalize?})
          (if (u/-probably-ref-lookups? x)
            (into (empty x) (fn [ref] (entity dx ref {:denormalize? denormalize?})) x)
            x)))
      (ex/-get e k))))

(defn entity
  ([dx ref] (entity dx ref {}))
  ([dx ref {:keys [denormalize?]}]
   {:pre [(u/-probably-dx? @dx)]}
   (when (p/-pick dx ref)
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
            (when-let [e (p/-pick dx ref)]
              (if denormalize?
                (denormalize dx e)
                e))))

        :cljs
        (reify
          ILookup
          (-lookup [this k]
            (-entity-lookup dx ref k denormalize?))

          IAssociative
          (-assoc [this k v]
              (entity (commit! dx [:dx/put ref k v]) ref))

          IDeref
          (-deref [this]
            (when-let [e (p/-pick dx ref)]
              (if denormalize?
                (denormalize dx e)
                e))))))))

(defn table [dx table]
  (when-let [xs (ex/-get* (p/-index dx) table)]
    (ex/-loop [ref xs :let [acc (transient {})]]
      (recur (ex/-assoc! acc ref (p/-pick dx ref)))
      (persistent! acc))))

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

;; (filter (fn [e] (= "Oleg" (:name e))) dx)

