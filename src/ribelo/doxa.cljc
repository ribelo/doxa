(ns ribelo.doxa
  (:require
   [clojure.datafy :refer :all]
   [meander.epsilon :as m]
   [taoensso.encore :as e]
   [editscript.core :as es]))

(def ^:private eid? (some-fn keyword? e/pos-int? string?))

(defn empty-db [] {})

(defn db? ^Boolean [db] (map? db))

(defn add-meta
  [db]
  (with-meta db {:listeners (atom {})}))

(defn create-db
  []
  (add-meta {}))

(defn- apply-tx
  [db cache_ tx]
  (m/match tx
    ;; add ?id ?k ?v
    [:db/add (m/pred eid? ?id) (m/pred keyword? ?k) (m/pred some? ?v)]
    (assoc-in db [?id ?k] ?v)
    ;; add -?id ?k ?v
    [:db/add (m/pred e/neg-int? ?id) (m/pred keyword? ?k) (m/pred some? ?v)]
    (let [id (get @cache_ ?id (e/uuid-str))]
      (assoc-in db [id ?k] ?v))
    ;; add m
    [:db/add {:db/id (m/or (m/pred eid? ?id) (m/let [?id (e/uuid-str)])) :as ?m}]
    (assoc db ?id ?m)
    ;; retract ?id
    [:db/retract (m/pred eid? ?id)]
    (dissoc db ?id)
    ;; retract ?id ?k
    [:db/retract (m/pred eid? ?id) ?k]
    (e/dissoc-in db [?id ?k])
    ;; retract {}
    [:db/retract {:db/id (m/pred eid? ?id)}]
    (dissoc db ?id)))

(defn listen!
  ([db cb] (listen! db (e/uuid-str) cb))
  ([db k cb]
   (if (meta db)
     (swap! (:listeners (meta db)) assoc k cb)
     (with-meta db (atom {k cb})))
   k))

(defn unlisten!
  ([db k]
   (when (meta db)
     (swap! (:listeners (meta db)) dissoc k))))

(defn -transact
  ([db txs] (-transact db txs nil))
  ([db txs tx-meta]
   (let [db'
         (let [cache_ (atom {})]
           (reduce
            (fn [acc tx]
              (apply-tx acc cache_ tx))
            db
            txs))]
     {:db-before db
      :db-after db'
      :tx-data txs
      :tx-meta tx-meta})))

(defn transact
  ([db txs] (transact db txs nil))
  ([db txs tx-meta]
   (let [report (-transact db txs tx-meta)]
     (doseq [[_ cb] (some-> (meta db) :listeners deref)]
       (cb report))
     (report :db-after))))

(defn transact!
  ([db_ txs] (transact! db_ txs nil))
  ([db_ txs tx-meta]
   (swap! db_ (fn [db] (transact db txs tx-meta)))))

;; pull
(def data
  [[:db/add
    {:db/id     :ivan
     :name      "Ivan"
     :last-name "Ivanov"
     :friend    :petr}]
   [:db/add
    {:db/id     :petr
     :name      "Petr"
     :last-name "Petrov"
     :friend    [:smith :ivan]}]
   [:db/add
    {:db/id     :smith
     :name      "Smith"
     :last-name "Smith"
     :friend    [:ivan]}]])

(def conn_ (atom (empty-db)))

(transact! conn_ data)

(get-in [{:spectre [:film/name :film/year]}] [0 :spectre])

;; query

(defn rev-keyword? [k]
  (when (keyword? k)
    (->> (name k) first #{\_} some?)))

(defn rev->keyword [k]
  (m/match k
    (m/pred qualified-keyword? ?k)
    (let [[ns k] (e/explode-keyword ?k)]
      (e/merge-keywords [ns (e/substr k 1)]))
    (m/pred keyword? ?k)
    (keyword (e/substr (name k) 1))))

(defn reverse-search [db k id]
  (m/rewrite db
    {?id {:db/id ?id
          ~k (m/or !pid [!pid ...])}
     & ?more
     :as ?m}
    [(m/cata [?id !pid]) & (m/cata ?more)]

    [?id (m/or ~id (m/scan ~id))] ?id

    {} []))

(defn eql
  ([db selector id]
   (m/match id
     (m/pred eid? ?id)
     ((eql db [{?id selector}]) ?id)
     [(m/pred eid? !ids) ...]
     (mapv #(eql db selector %) !ids)))
  ([db selector]
   (m/rewrite {:db       db
               :selector selector}
     ;; get key from db
     {:db {?k ?v} :k (m/pred eid? ?k)}
     ?v
     ;; single join
     {:db       (m/or (m/pred eid? ?root) [(m/pred eid? ?root)])
      :selector ?k}
     (m/cata {:db       (m/cata {:db ~db :k ?root})
              :selector ?k})
     ;; multi join
     {:db [(m/pred eid? !roots) ...] :selector ?k}
     [(m/cata {:db       (m/cata {:db ~db :k !roots})
               :selector ?k}) ...]
     ;; reverse search
     {:db       {:db/id ?id :as ?m}
      :selector [(m/or (m/pred rev-keyword? !rks) (m/pred keyword? !ks)) ...]}
     ~(merge
       (m/rewrite !ks
         (m/scan :*) ?m
         _ ~(select-keys ?m !ks))
       (->> !rks (into {} (map (fn [k] [k (reverse-search db (rev->keyword k) ?id)])))))
     ;; reverse search
     {:db       {:db/id ?id :as ?db}
      :selector (m/pred rev-keyword? ?k)}
     {?k ~(reverse-search db (rev->keyword ?k) ?id)}
     ;;
     {:db       {?k ?m :as ?db}
      :selector (m/pred eid? ?k)}
     {?k ?m}

     {:db       [{:as !maps} ...]
      :selector [(m/pred keyword? !ks) ...]}
     ~(mapv (fn [m] (select-keys m !ks)) !maps)
     ;; reverse
     {:db       {:db/id ?id :as ?db}
      :selector {(m/pred rev-keyword? ?root) ?more}}
     {?root (m/cata {:db       ~db
                     :selector ~(let [ids (reverse-search db (rev->keyword ?root) ?id)]
                                  (mapv (fn [id] {id ?more}) ids))
                     :flat?    true})}
     ;; no key
     {:db       {?root (m/not (m/some ?m)) :as ?db}
      :selector {?root ?more}
      :flat?    (m/not true)}
     nil

     {:db       {?root (m/some ?m) :as ?db}
      :selector {?root ?more}
      :flat?    (m/not true)}
     {?root (m/cata {:db ?m :selector ?more})}

     {:db       {:as ?db}
      :selector (m/and {?root ?more}
                       (m/app #(doto % prn) ?x))
      :flat?    true}
     (m/cata {:db ~(get ?db ?root) :selector ?more})

     {:db       {:as ?db}
      :selector [!ks ...]
      :flat?    (m/and ?flat (m/not true))}
     {& [(m/cata {:db ?db :selector !ks :flat? ?flat}) ...]}

     {:db       {:as ?db}
      :selector [!ks ...]
      :flat?    ?flat}
     [(m/cata {:db ?db :selector !ks :flat? ?flat}) ...])))

(eql @conn_ [:*] :ivan)

(defn datalog->meander [where]
  (loop [[elem & where*] where m {} fns [] vars []]
    (if elem
      (let [[m fns vars]
            (m/match elem
              [(m/pred (complement list?) ?e) ?k]
              [(update m ?e merge {:db/id ?e ?k `(m/some)}) fns vars]
              [(m/pred (complement list?) ?e) ?k ?v]
              [(update m ?e merge {:db/id ?e ?k ?v}) fns vars]
              [(?fn . !args ...)]
              [m (conj fns `(m/guard (apply ~?fn ~!args))) vars]
              [(?fn . !args ...) ?x]
              [m fns (conj vars `(m/let [~?x (apply ~?fn ~!args)]))])]
        (recur where* m fns vars))
      `(m/and ~m ~@fns ~@vars))))

(defn eql-many [db selector ids]
  (mapv #(eql db selector %) ids))

(defn parse-query [q]
  (m/rewrite q
    [(m/pred keyword? ?k) . !args ... & ?more]
    {& [[?k [!args ...]] & (m/cata ?more)]}
    [] []))

(defn parse-find [x]
  (m/rewrite x
    [(eql/project ?x)]
    '~[(eql/project ?db ~?x)]
    ?x ?x))



#?(:clj
   (m/defsyntax query [args]
     (let [q (datalog->meander args)]
       `~q)))

#?(:clj
   (defmacro q [q' db]
     (let [{:keys [where find]} (parse-query q')]
       `(doall
         (m/rewrites ~db
           ~(query where) ~find)))))

(datalog->meander
 (parse-query
  '[:find ?e
    :where [?e :name "Ivan"]]))

(q [:find (eql @conn_ [?e])
    :where
    [?e :name "Ivan"]]
  @conn_)

{:root/persons {:ivan {:name "Ivan"}}}




;; => {:a [1 2 3] :b [4 5] :c [6 7 8 9]}
