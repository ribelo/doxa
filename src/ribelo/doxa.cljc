(ns ribelo.doxa
  (:refer-clojure  :exclude [ident? -next])
  #?(:cljs (:require-macros [ribelo.doxa :refer [q with-dx with-dx! -iter]]))
  (:require
   [meander.epsilon :as m]
   [taoensso.encore :as enc]
   [taoensso.timbre :as timbre]
   [editscript.core :as es]
   [editscript.edit :as ese]))

#?(:clj (set! *warn-on-reflection* true))

(declare reverse-search)

(def ^:dynamic *empty-map* (hash-map))
(def ^:dynamic *atom-fn* atom)
(def ^:dynamic *id-sufixes* #{"id" "by-id" "list"})

(defmacro ^:private -iter
  "returns an iterator for both clj and cljs.
  while there is an iter function in cljs, there isn't and there won't be one in
  clj, which is a pity because iterator is much faster than reduce."
  [xs]
  `(enc/if-clj (clojure.lang.RT/iter ~xs) (cljs.core/iter ~xs)))

(defmacro ^:private -first-key
  [xs]
  `(first (keys ~xs)))

(defmacro ^:private -first-val
  [xs]
  `(first (vals ~xs)))

(def
  ^{:doc "returns a vector even if the argument is nil"}
  conjv (fnil conj []))

(def
  ^{:doc "returns a map even if the argument is nil"}
  into-map (fnil into {}))

(def ^:private simple-eid?    (some-fn keyword? int? string?))
(def ^:private compound-eid? #(and (vector? %) (enc/revery? simple-eid? %)))
(def ^:private eid?           (some-fn simple-eid? compound-eid?))

(defn- key-id? [k]
  (m/rewrite k
    (m/keyword _ (m/pred #(contains? *id-sufixes* %))) true
    _ false))

(defn- ident? [x]
  (m/rewrite x
    [(m/pred key-id?) (m/pred eid?)] true
    _ false))

(defn- idents? [xs]
  (m/rewrite xs
    [(m/pred ident?) ...] true
    _ false))

(defn- entity? [^clojure.lang.IPersistentMap m]
  (m/rewrite m
    (m/and {} (m/scan [(m/pred key-id?) (m/pred eid?)])) true
    _ false))

(defn- entities? [^clojure.lang.IPersistentVector xs]
  (m/rewrite xs
    [(m/pred entity?) ...] true
    _ false))

(def ^:private not-entities? (complement (some-fn entity? entities?)))

(defn- entity-id [^clojure.lang.IPersistentMap m]
  ^::m/dangerous
  (m/rewrite m
    (m/scan [(m/pred key-id? ?k) ?v])
    [?k ?v]
    _ false))

(defn normalize
  "turns a nested map into a flat collection with references."
  [data]
  (let [it (-iter data)]
    (loop [m (transient {}) r [] id nil]
      (enc/cond
        (and (not (.hasNext it)) (nil? id))
        nil
        ;; 
        (and (not (.hasNext it)) (enc/some? id))
        (conj r [id (persistent! m)])
        ;;
        :let [^clojure.lang.MapEntry me (.next it)
              k  #?(:clj (.key me) :cljs (.-key me))
              v  #?(:clj (.val me) :cljs (.-val me))]
        (key-id? k)
        (recur (assoc! m k v) r [k v])
        ;;
        (entity? v)
        (recur (assoc! m k (entity-id v)) (into r (normalize v)) id)
        ;;
        (entities? v)
        (recur (assoc! m k (mapv entity-id v))
               (reduce (fn [acc m'] (into acc (normalize m'))) r v)
               id)
        ;;
        (ident? v)
        (recur (assoc! m k v) r id)
        ;;
        :else
        (recur (assoc! m k v) r id)))))

(defn- -denormalize
  ([db data max-level level]
   (let [it (-iter data)]
     (loop [m {}]
       (enc/cond
         (> level max-level)
         (timbre/warnf "maximum nesting level %s for %s has been exceeded" max-level (entity-id data))
         (and (not (.hasNext it)))
         m
         ;;
         :let [^clojure.lang.MapEntry me (.next it)
               k  #?(:clj (.key me) :cljs (.-key me))
               v  #?(:clj (.val me) :cljs (.-val me))]
         (map? v)
         (recur (assoc m k (-denormalize db v max-level (inc level))))
         ;;
         (ident? v)
         (recur (assoc m k (let [m (or (get-in m v)
                                       (-denormalize db (get-in db v) max-level (inc level)))]
                             m)))
         (idents? v)
         (recur (assoc m k (let [xs (mapv (fn [ident] (or (get-in m ident)
                                                         (-denormalize db (get-in db ident) max-level (inc level)))) v)]
                             xs)))
         ;;
         :else
         (recur (assoc m k v)))))))

(defn denormalize
  "turns a flat map into a nested one. to avoid stackoverflow and infinite loop,
  it takes a maximum nesting level as an additional argument"
  ([   data          ] (-denormalize data data 12        0))
  ([db data          ] (-denormalize db   data 12        0))
  ([db data max-level] (-denormalize db   data max-level 0)))

(comment
  (let [data (commit {} [:dx/put {:db/id :ivan :name "ivan" :friend [{:db/id 1 :name "petr" :friend [[:db/id :ivan]]}
                                                                     {:db/id 2 :name "petr"}
                                                                     {:db/id 3 :name "petr"}
                                                                     {:db/id 4 :name "petr"}
                                                                     {:db/id 5 :name "petr"}
                                                                     {:db/id 6 :name "petr"}
                                                                     {:db/id 7 :name "petr"}
                                                                     {:db/id 8 :name "petr"}
                                                                     {:db/id 9 :name "petr"}
                                                                     {:db/id 10 :name "petr"}]}])]
    (denormalize data))
  ;; => 1362.47
  )


(defn -submit-commit
  "apply transactions to db."
  [db tx]
  (m/find tx
    ;; put [?tid ?eid] ?k ?v
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)]
     (m/pred keyword? ?k) (m/pred (complement (some-fn entity? entities? ident?)) ?v)]
    (assoc-in db [?tid ?eid ?k] ?v)
    ;; put [?tid ?eid] ?k ?entity
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred entity? ?v)]
    (let [xs (normalize ?v)
          it (-iter xs)]
      (loop [db' (assoc-in db [?tid ?eid ?k] (entity-id ?v))]
        (enc/cond
          :if-not (.hasNext it) db'
          :let [[ks m] (.next it)]
          (recur (assoc-in db' ks m)))))
    ;; put [?tid ?eid] ?k [?entity ...]
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) [(m/pred entity? !vs) ...]]
    (let [itx (-iter !vs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext itx) acc
          :let    [?v  (.next itx)
                   xs  (normalize ?v)
                   ity (-iter xs)]
          (recur
           (loop [acc (update-in acc [?tid ?eid ?k] conjv (entity-id ?v))]
             (enc/cond
               (not (.hasNext ity)) acc
               :let                 [[ks m] (.next ity)]
               (recur (assoc-in acc ks m))))))))
    ;; put [?tid ?eid] ?k [?ref ?id]
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) [(m/pred key-id? ?kid) (m/pred eid? ?rid)]]
    (if (get-in db [?kid ?rid])
      (assoc-in db [?tid ?eid ?k] [?kid ?rid])
      (throw (ex-info "lookup ref does not exist" {:ref [?kid ?rid]})))
    ;; put ?entity
    [:dx/put (m/pred entity? ?m)]
    (let [xs (normalize ?m)
          it (-iter xs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext it) acc
          :let    [[ks m] (.next it)]
          (recur (assoc-in acc ks m)))))
    ;; put [m ...]
    [:dx/put (m/pred entities? ?vs)]
    (let [itx (-iter ?vs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext itx) acc
          :let    [?m  (.next itx)
                   xs  (normalize ?m)
                   ity (-iter xs)]
          :else
          (recur
           (loop [acc' acc]
             (enc/cond
               :if-not (.hasNext ity) acc'
               :let    [[ks m] (.next ity)]
               (recur  (assoc-in acc' ks m))))))))
    ;; put [?tid ?eid] m
    [:dx/put [(m/pred key-id? ?tid) (m/pred eid? ?eid)] ?m]
    (let [xs (normalize (assoc ?m ?tid ?eid))
          it (-iter xs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext it) acc
          :let    [[ks m] (.next it)]
          (recur  (update-in acc ks enc/merge m)))))
    ;; delete [?tid ?eid]
    [:dx/delete [(m/pred key-id? ?tid) (m/pred eid? ?eid) :as ?ident]]
    (enc/cond
      :let     [n (count (keys (db ?tid)))
                v (get-in db ?ident)]
      (nil? v) db
      :let     [it (-iter (reverse-search db ?ident))
                db' (enc/cond
                      (> n 1) (enc/dissoc-in db [?tid] ?eid)
                      (= n 1) (dissoc db ?tid))]
      (loop [acc db']
        (enc/cond
          :if-not (.hasNext it) acc
          :let    [[tid eid k] (.next it)]
          (recur  (-submit-commit acc [:dx/delete [tid eid] k ?ident])))))
    ;; delete [?tid ?eid] ?k
    [:dx/delete [(m/pred key-id? ?tid) (m/pred eid? ?eid)] ?k]
    (let [m  (get-in db [?tid ?eid])
          m' (dissoc m ?k)]
      (if (seq m') (assoc-in db [?tid ?eid] m') (-submit-commit db [:dx/delete [?tid ?eid]])))
    ;; delete {}
    [:dx/delete {(m/pred key-id? ?tid) (m/pred eid? ?eid)}]
    (-submit-commit db [:dx/delete [?tid ?eid]])
    ;; delete [?tid ?eid] ?k ?v
    [:dx/delete [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred some? ?v)]
    (enc/cond
      :if-not                                        [v (get-in db [?tid ?eid ?k])] db
      (and (seq v) (> (count v) 1) (not (ident? v))) (update-in db [?tid ?eid ?k] #(into [] (remove #{?v}) %))
      (or (and (seq v) (= (count v) 1)) (ident? v))  (enc/dissoc-in db [?tid ?eid] ?k)
      (and v (not (vector? v)))                      (throw (ex-info (enc/format "%s is not a vector" [?tid ?eid ?k]) {:v v}))
      (throw (ex-info "invalid commit" {:tx tx})))
    ;; conj [?tid ?eid] ?k ?v
    [:dx/conj [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred not-entities? ?v)]
    (enc/cond
      :let         [xs (get-in db [?tid ?eid ?k])]
      (ident?  xs) (assoc-in db [?tid ?eid ?k] [xs ?v])
      (vector? xs) (assoc-in db [?tid ?eid ?k] (conj xs ?v))
      (some?   xs) (assoc-in db [?tid ?eid ?k] [xs ?v])
      :else        (assoc-in db [?tid ?eid ?k] [?v]))
    ;; conj [?tid ?eid] ?k ?m
    [:dx/conj [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred entity? ?v)]
    (-> (update-in db [?tid ?eid ?k] conjv (entity-id ?v))
        (-submit-commit [:dx/put ?v]))
    ;; conj [?tid ?eid] ?k [?m ...]
    [:dx/conj [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) [(m/pred entity? !vs) ...]]
    (let [it (-iter !vs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext it) acc
          :let    [v    (.next it)
                   acc' (-> (update-in acc [?tid ?eid ?k] conjv (entity-id v))
                            (-submit-commit [:dx/put v]))]
          :else   (recur acc'))))
    ;; merge [?tid ?eid] ?m
    [:dx/merge [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/and (m/pred map? ?v) (m/not (m/pred entity? ?v)))]
    (update-in db [?tid ?eid] into-map (assoc ?v ?tid ?eid))
    ;; merge [?tid ?eid] ?k ?m
    [:dx/merge [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/and (m/pred map? ?v) (m/not (m/pred entity? ?v)))]
    (update-in db [?tid ?eid ?k] into-map ?v)
    ;; update [?tid ?eid] ?f & ?args
    [:dx/update [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred fn? ?f) & ?args]
    (update-in db [?tid ?eid] (partial apply ?f) ?args)
    ;; update [?tid ?eid] ?k ?f
    [:dx/update [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred fn? ?f) & ?args]
    (update-in db [?tid ?eid ?k] (partial apply ?f) ?args)
    ;; match [?tid ?eid] ?m
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred entity? ?m)]
    (= ?m (get-in db [?tid ?eid]))
    ;; match [?tid ?eid] ?f
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred fn? ?f)]
    (?f (get-in db [?tid ?eid]))
    ;; match [?tid ?eid] ?k ?v
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred (complement fn?) ?v)]
    (= ?v (get-in db [?tid ?eid ?k]))
    ;; match [?tid ?eid] ?k ?f
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred fn? ?f)]
    (?f (get-in db [?tid ?eid ?k]))
    ;; _
    _ (throw (ex-info "invalid commit" {:tx tx}))))

(comment
  (enc/qb 1e5
    (-submit-commit {:db/id {:ivan {:db/id :ivan :name "ivan"}}}
                    [:dx/put [:db/id :ivan] :friend {:db/id :petr :name "petr"}]))
  ;; => 177.95
  (enc/qb 1e5
    (-submit-commit {:db/id {:ivan {:db/id :ivan :name "ivan"}}}
                    [:dx/put [:db/id :ivan] :friend [{:db/id :petr :name "petr"}
                                                     {:db/id :smith :name "smith"}]]))
  ;; => 402.66
  )

(defn listen!
  "listens for changes in the db. each time changes are made via commit, the
  callback is called with the db. the transaction report is written to the db
  metadata, and may include, depending on the configuration, the transaction
  time, the difference between the old and new db, the db hash. calling listen
  twice with the same k overwrites the previous callback."
  ([db_ cb] (listen! db_ (enc/uuid-str) cb))
  ([db_ k cb]
   (if (meta db_)
     (swap! (get (meta db_) :listeners (atom {})) assoc k cb)
     (alter-meta! db_ assoc :listeners (atom {k cb})))
   k))

(comment
  (let [db_ (atom (create-dx))]
    (alter-meta! db_ assoc :a 1)
    ;; (listen! db_ #(println :sex))
    ;; (commit! db_ [[:dx/put [:db/id :ivan] :a 1]])
    )

  )

(defn unlisten!
  "remove registered listener"
  ([db_ k]
   (when (meta db_)
     (swap! ((meta db_) :listeners) dissoc k))))

(defn- -commit
  ([db txs] (-commit db txs nil))
  ([db txs tx-meta]
   (let [db'   (enc/cond
                 (vector? (first txs))
                 (let [it (-iter txs)]
                   (loop [acc db match? true]
                     (enc/cond
                       :if-not (.hasNext it)
                       acc
                       :let [tx   (.next it)
                             kind (first tx)]
                       (enc/kw-identical? kind :dx/match)
                       (recur acc (-submit-commit acc tx))
                       ;;
                       match?
                       (recur (-submit-commit acc tx) match?)
                       ;;
                       (not match?)
                       (recur acc match?))))
                 ;;
                 (keyword? (first txs))
                 (-submit-commit db txs))
         meta' (meta db')]
     (cond-> (vary-meta db' assoc ::last-transaction-timestamp (enc/now-udt))
       (::with-diff? meta')
       (vary-meta assoc
                  ::tx (ese/get-edits (es/diff db db' {:algo :quick}))
                  ::h  (hash db'))
       ;;
       tx-meta
       (vary-meta assoc :tx-meta tx-meta)))))

(defn commit
  "apply transactions to db. txs can be either a single transaction or a vector of
  transactions. returns a modified db. transaction report is stored in the
  db metadata.

  usage:
  [:dx/put|delete|conj|update|match [table eid] ?m | (?k ?v)]

  put:
      entire map|s with entity id
      [:dx/put {:person/id :ivan :name \"ivan\" :age 30}]
      [:dx/put [{:person/id :ivan :name \"ivan\" :age 30} ...]]

      entire map, without entity id
      [:dx/put [:person/id :ivan] {:name \"ivan\" :age 30}]

      ident|s
      [:dx/put [:person/id :ivan] :friend [:person/id :petr]]
      [:dx/put [:person/id :ivan] :friend [[:person/id :petr] ...]]

      reference map|s
      [:dx/put [:person/id :ivan] :friend {:person/id :petr :name \"petr\"}]
      [:dx/put [:person/id :ivan] :friend [{:person/id :petr :name \"petr\"} ...]]

      key value
      [:dx/put [:person/id :ivan] :age 12]

  delete:
      ident
      [:dx/delete [:person/id :ivan]]

      entity|s
      [:dx/delete {:person/id :ivan ...}]
      [:dx/delete [{:person/id :ivan ...} ...]]

      key
      [:dx/delete [:person/id :ivan] :age]

      value - like disj
      [:dx/delete [:person/id :ivan] :aka \"tupen\"]

  conj:
      value
      [:dx/conj [:person/id :ivan] :aka \"tupen\"]

      ident|s
      [:dx/conj [:person/id :ivan] :friend [:person/id :petr]]
      [:dx/conj [:person/id :ivan] :friend [[:person/id :petr] ...]]

      entity|s
      [:dx/conj [:person/id :ivan] :friend {:person/id :petr ...}]
      [:dx/conj [:person/id :ivan] :friend [{:person/id :petr ...} ...]]

  update:
      entity
      [:dx/update [:person/id :ivan] (fn [entity] (f entity)]

      key
      [:dx/update [:person/id :ivan] :age inc]

  match:
      entity
      [:dx/match [:person/id :ivan] {:person/id :ivan ...}]

      key value
      [:dx/match [:person/id :ivan] :age 30]

      key fn
      [:dx/match [:person/id :ivan] :salary #(> % 10000)]
  "
  ([db txs]         (commit  db txs nil))
  ([db txs tx-meta] (-commit db txs tx-meta)))

(defn commit!
  "accepts an atom with db, see `commit`"
  ([db_ txs] (commit! db_ txs nil))
  ([db_ txs tx-meta]
   (swap! db_ (fn [db] (commit db txs tx-meta)))
   (when-let [it (some-> (meta db_) :listeners deref -iter)]
       (while #?(:clj (.hasNext it) :cljs ^cljs (.hasNext it))
         (let [[_k cb] (.next it)]
           (cb @db_))))))

(defn patch
  "patch db using ediscript edits"
  ([db edits]
   (patch db edits (enc/now-udt)))
  ([db edits time]
   (let [db' (es/patch db (es/edits->script edits))]
     (vary-meta db' assoc
                ::last-transaction-timestamp  time
                ::tx edits))))

(defn patch!
  "patch db inside atom, see `patch`"
  ([db_ edits]
   (patch! db_ edits (enc/now-udt)))
  ([db_ edits time]
   (swap! db_ (fn [db] (patch db edits time)))
   (when-let [it (some-> (meta @db_) :listeners deref -iter)]
       (while #?(:clj (.hasNext it) :cljs ^cljs (.hasNext it))
         (let [[_k cb] (.next it)]
           (cb @db_))))))

(defn db-with
  ([data] (db-with *empty-map* data))
  ([db data]
   (-commit db (mapv (fn [m] [:dx/put m]) data))))

(defn create-dx
  "creates a db, can take a map, which is written to the metadata"
  ([]
   (create-dx [] {::with-diff? false}))
  ([data]
   (create-dx data {::with-diff? false}))
  ([data {:keys [with-diff?] :as opts}]
   (with-meta
     (if (not-empty data) (db-with data) *empty-map*)
     (into opts {::last-transaction-timestamp nil ::tx nil ::cache_ (atom {})}))))

(defn -last-tx [db] (some-> db meta ::tx last))

;; pull

(comment
  (def txs
    [{:db/id     :ivan
      :name      "Ivan"
      :last-name "Ivanov"
      :friend    [[:db/id :petr]]
      :age       30}
     {:db/id     :petr
      :name      "Petr"
      :last-name "Petrov"
      :friend    [[:db/id :smith] [:db/id :ivan]]
      :age       15}
     {:db/id     :smith
      :name      "Smith"
      :last-name "Smith"
      :friend    [[:db/id :petr]]
      :age       55}])

  (def conn_ (atom (db-with txs)))
  )

(comment
  (enc/qb 1e5
    (commit! conn_ [[:dx/put [:db/id :ivan] :age (rand-int 100)]])
    (commit! conn2_ [[:dx/put [:db/id :ivan] :name (rand-int 100)]]))
  (def diff (:tx (meta @conn2_)))
  (let [tx (repeat 1000 [:dx/put [:db/id :ivan] :age 8])
        diff (es/edits->script (vec (repeat 1000 (first diff))))]
    (enc/qb 1e3
      (commit @conn_ tx)
      (es/patch @conn2_ diff))))

(defn- -rev-keyword? [k]
  (let [name' (name k)]
    (and (enc/str-starts-with? name' "_")
         (not (enc/str-starts-with? name' "__")))))

(def ^:private -forward-keyword?
  (complement -rev-keyword?))

(defn- -rev->keyword [k]
  (cond
    (qualified-keyword? k)
    (let [[ns k] (enc/explode-keyword k)]
      (enc/merge-keywords [ns (enc/substr k 1)]))
    ;;
    (keyword? k)
    (keyword (enc/substr (name k) 1))))

(defn reverse-search
  ([db id]
   ^::m/dangerous
   (vec (m/search db {?pid {?eid {?k (m/or ~id (m/scan ~id))}}} [?pid ?eid ?k])))
  ([db k id]
   ^::m/dangerous
   (vec (m/search db {?pid {?eid {~k (m/or ~id (m/scan ~id))}}} [?pid ?eid]))))

(defn pull*
  ([db query]
   (pull* db query nil))
  ([db query parent]
   (enc/cond
     (= [:*] query)
     (get-in db parent)
     :else
     (let [it (-iter query)]
       (loop [r {} id parent]
         (enc/cond
           (not (.hasNext it))
           r
           ;;
           :let  [elem (.next it)]
           (and (map? elem) (ident? (ffirst elem)))
           (recur (pull* db (second (first elem)) (ffirst elem)) id)
           ;; prop
           (and (some? id) (#{:*} elem))
           (recur
            (let [m  (get-in db id)
                  it (-iter m)]
              (loop [acc (transient {})]
                (enc/cond
                  (not (.hasNext it))
                  (persistent! acc)
                  ;;
                  :let [elem (.next it)
                        k    (nth elem 0)
                        v    (nth elem 1)]
                  ;;
                  (and (idents? v) (= 1 (count v)))
                  (recur (assoc! acc k (nth v 0)))
                  ;;
                  :else
                  (recur (assoc! acc k v)))))
            id)
           ;;
           (and (some? id) (keyword? elem) (not (-rev-keyword? elem)))
           (recur (enc/assoc-some r elem (get-in db (conj id elem))) id)
           (and (some? id) (keyword? elem) (-rev-keyword? elem))
           (let [k (-rev->keyword elem)]
             (recur
              (enc/assoc-some r elem (enc/cond
                                       :let  [v (reverse-search db k id)]
                                       (and (idents? v) (= (count v) 1))
                                       (nth v 0)
                                       :else v))
              id))
           ;; join
           :when (and (some? id) (map? elem)) ;; {:friend [:name]}
           :let  [k    (ffirst elem)
                  rev? (-rev-keyword? k)
                  ref' (if-not rev?
                         (get-in db (conj parent k))
                         (reverse-search db (-rev->keyword k) id))]
           ;;
           (and (some? id) (ident? ref'))
           (recur (enc/assoc-some r k (pull* db (second (first elem)) ref')) id)
           ;;
           (and (some? id) (idents? ref') (not rev?))
           (recur (enc/assoc-some r (ffirst elem)
                                  (into [] (comp (map (partial pull* db (second (first elem)))) (remove empty?)) ref'))
                  id)
           (and (some? id) (idents? ref') rev?)
           (recur (enc/assoc-some r (ffirst elem)
                                  (enc/cond
                                    :let [xs (mapv (partial pull* db (second (first elem))) ref')
                                          n  (count xs)]
                                    ;;
                                    (> n 1)
                                    (into [] (comp (map not-empty) (remove nil?)) xs)
                                    ;;
                                    (= n 1)
                                    (not-empty (first xs))))
                  id)
           ;;
           (some? id)
           (recur r id)))))))

(defn pull
  ([db query]
   (pull db (second (first query)) (ffirst query)))
  ([db query id]
   (enc/cond
     (ident?  id)                (pull* db query id )
     (idents? id) (mapv (fn [id'] (pull* db query id')) id))))

(defn pull-one
  ([db query]
   (-> (pull db (second (first query)) (ffirst query)) vals first))
  ([db query id]
   (enc/cond
     (ident?  id)                (-> (pull* db query id)  vals first)
     (idents? id) (mapv (fn [id'] (-> (pull* db query id') vals first)) id))))

(comment

  (enc/qb 1e5
    (pull @conn_ [:name {:friend [:name {:friend [:name :age]}]}] [:db/id :ivan]))
  ;; => 627.52

  (enc/qb 1e5
    (m/search @conn_
      {_ {:ivan {:name ?name   :friend (m/scan [_ ?f])}
          ?f    {:name ?fname  :friend (m/scan [_ ?ff])}
          ?ff   {:name ?ffname :age ?ffage}}}
      {:name ?name :friend {:name ?fname :friend {:name ?ffname :age ?ffage}}}))
  ;; => 620.35

  (enc/qb 1e5
    (pull @conn_ [:name {:friend [:name]}] [:db/id :ivan]))
  ;; => 229.91

  (enc/qb 1e5
    (pull @conn_ [{:friend [:name]}] [:db/id :ivan]))
  ;; => 187.2

  (enc/qb 1e5
    (pull @conn_ [:name :age :sex] [:db/id :ivan]))
  ;; => 150.18
  )

(defn haul
  ([db]   (denormalize db   12))
  ([db x] (haul        db x 12))
  ([db x max-level]
   (enc/cond
     (keyword? x)
     (denormalize db (db x) max-level)
     ;;
     (vector? x)
     (denormalize db (get-in db x) max-level)
     ;;
     (map? x)
     (denormalize db x max-level)
     )))

;; * datalog

(defn parse-find [args]                 ;TODO
  (m/rewrite args
    [!xs ... '.]
    {& (m/cata [!xs ...]) :mapcat? true :first? true}
    [!xs ... '...]
    {& (m/cata [!xs ...]) :mapcat? true}
    ;; ?x ?y ...
    [(m/symbol _ _ :as !symbols) ...]
    {:find [!symbols ...]}
    ;; ?x ?y ?z
    [(m/symbol _ _ :as !symbols) ...]
    {:find [!symbols ...]}
    ;; pull
    [(pull ?q [?table ?e])]
    {:find [?table ?e] :pull {:q ?q :ident [?table ?e]}}
    ;; [?x ?y]
    [[!xs ...]]
    {:find [[!xs ...]]}
    [[!xs ... '.]]
    {:find [[!xs ...]] :mapcat? true :first? true}
    [[!xs ... '...]]
    {:find [[!xs ...]] :mapcat? true}))

(defn parse-query [q & args]
  (m/rewrite q
    [:find & ?args & (m/cata ?out)]
    {& [~(parse-find ?args) ?out]}
    [:keys [!ks ...] & (m/cata ?out)]
    {& [[:keys [!ks ...]] ?out]}
    [(m/keyword _ _ :as ?keyword) & ?args & (m/cata ?out)]
    {?keyword ?args & ?out}

    [] {:args ~(or (vec args) [])}))

(defn build-args-map [{:keys [in args] :as q}]
  (m/rewrite q
    {:in nil}
    {}
    ;;
    {:in   [(m/pred symbol? !xs) ..?n]
     :args (m/or [!ys ..?n] [[!ys ..?n]])}
    {& [[!xs !ys] ...]}
    ;;
    {:in   [(m/pred symbol? !xs) (m/pred vector? !zs) ..?n]
     :args [!ys !js ..?n]}
    [~(enc/into-all
      {} (map vector !xs !ys) (build-args-map {:in !zs :args !js}))]
    ;;
    {:in   [[(m/pred symbol? !xs)] ...]
     :args [!ys ...]}
    {& [[!xs [:or !ys]] ...]}
    {:in [[[!xs ...]]]
     :args [[!ys ...]]}
    ~(into [] (map #(build-args-map {:in !xs :args [%]})) !ys)))

(defn qsymbol? [x]
  (and (symbol? x) (enc/str-starts-with? (name x) "?")))

(defn- some-value
  ([] `(m/some))
  ([v]
   (m/rewrite v
     (m/and ?v (m/not (m/symbol _)))
     ?v
     ;;
     (m/pred qsymbol? ?v)
     ~`(m/some ~?v)
     ;;
     (m/symbol _ _ :as ?v)
     ~`(m/some (unquote ~?v))))
  ([s v]
   (m/rewrite [s v]
     [(m/symbol _ _ :as ?s) (m/and ?v (m/not (m/symbol _)))]
     ~`(m/and ~?s ~?v)
     ;;
     [(m/symbol _ _ :as ?s) (m/symbol _ _ :as ?v)]
     ~`(m/some (unquote ~?v))
     _ ~v)))

(defn parse-query-elem [elem args-map]
  (m/rewrite elem
    (m/pred qsymbol? ?x)
    ~(get args-map ?x ?x)
    ;;
    (m/map-of (m/cata !ks) (m/cata !vs))
    {& [[!ks !vs] ...]}
    ;;
    (?f . (m/cata !xs) ...)
    ~(apply list ?f !xs)
    ;; else
    ?x ?x))


(defn build-meander-query [m]
  (m/rewrite m
    {:maps (m/pred seq [!maps ...]) & ?more}
    (m/cata [:done [~(reduce enc/nested-merge !maps) & (m/cata ?more)]])
    {:let (m/pred seq [!let ...]) & ?more}
    [(`m/let [!let ...]) & (m/cata ?more)]
    {:guards (m/pred seq [!guards ...]) & ?more}
    [(`m/guard !guards) ... & (m/cata ?more)]
    (m/and [:done [!elems ..?n :as ?m]] (m/guard (= ?n 1)))
    {& [!elems ...]}
    (m/and [:done [!elems ..?n]] (m/guard (> ?n 1)))
    (`m/and . !elems ...)
    {} nil))

(defn datalog->meander [{:keys [where in args] :as q}]
  (let [args-maps (build-args-map q)]
    (m/rewrite {:where where :args-maps args-maps}
      {:where ?where :args-maps [!maps ..?n]}
      (`m/or . (m/cata {:where ?where :args-map !maps}) ...)
      ;;
      {:where ?where :args-maps {:as ?map}}
      (m/cata {:where ?where :args-map ?map})
      ;;
      {:where [!elems ...] :args-map (m/some ?args-map)}
      (m/cata [:done . (m/cata {:elem !elems :args-map ?args-map}) ...])
      ;;
      (m/with [%map   [:map !maps]
               %guard [:guard !guards]
               %let   [:let [!lets ...]]
               %seq   [:done . (m/or %map %guard %let) ...]]
        %seq)
      ~(build-meander-query {:maps !maps :guards !guards :let !lets})
      ;;
      (m/and {:elem [!xs ..?n] :as ?m} (m/guard (= ?n 2)))
      (m/cata {& ?m :elem ['_ . !xs ... nil]})
      (m/and {:elem [!xs ..?n] :as ?m} (m/guard (= ?n 3)))
      (m/cata {& ?m :elem ['_ . !xs ...]})
      ;;
      {:args-map ?args-map
       :elem   [(m/app #(parse-query-elem % ?args-map) ?table)
                (m/app #(parse-query-elem % ?args-map) ?e)
                (m/app #(parse-query-elem % ?args-map) ?attr)
                (m/app #(parse-query-elem % ?args-map) ?val)]
       :parsed (m/not (m/some))
       :as     ?m}
      (m/cata {& ?m :parsed [?table ?e ?attr ?val]})
      ;; [?e ?k nil?]
      {:parsed [?table (m/and ?e (m/not (m/pred list?))) ?attr (m/pred nil?)]}
      [:map {?table {?e {?attr ~(some-value)}}}]
      ;; [?e ?k ?v]
      {:parsed [?table (m/and ?e (m/not (m/pred list?))) ?attr (m/and ?v (m/not (m/pred vector?)) (m/not (m/pred qsymbol?)))]
       :elem   [_ _ _ ?uv]}
      [:map {?table {?e {?attr ~(some-value ?uv ?v)}}}]
      ;; [?e ?k ?v]
      {:parsed [?table (m/and ?e (m/not (m/pred list?))) ?attr (m/and ?v (m/not (m/pred vector?)) (m/pred qsymbol?))]}
      [:map {?table {?e {?attr ~(some-value ?v)}}}]
      ;; [?e ?k [:or [!xs ...]]
      {:parsed [?table (m/and ?e (m/not (m/pred list?))) ?attr [:or [!vs ...]]]}
      [:map {?table {?e {?attr (`m/or . !vs ...)}}}]
      ;; [?e ?k [?ref ?id]]
      {:parsed [?table (m/and ?e (m/not (m/pred list?))) ?attr (m/and ?v [_ _])]}
      [:map {?table {?e {?attr ~`(m/or ~?v (m/scan ~?v))}}}]
      ;; [?e ?k [!vs ...]]
      {:parsed [?table (m/and ?e (m/not (m/pred list?))) ?attr (m/and ?v [!vs ...])]}
      [:map {?table {?e {?attr (`m/or ?v . !vs ...)}}}]
      ;; [(f)]
      {:args-map (m/some ?args-map)
       :elem [(?f . (m/app #(parse-query-elem % ?args-map) !args) ...)]}
      [:guard (?f . !args ...)]
      ;; [(f) ?x]
      {:args-map (m/some ?args-map)
       :elem [(?f . (m/app #(parse-query-elem % ?args-map) !args) ...) ?x]}
      [:let [?x (?f . !args ...)]])))

#?(:clj
   (m/defsyntax query [args]
     (let [q (datalog->meander args)]
       `~q)))

(defn comp-some [& fns]
  (apply comp (filter identity fns)))

(defmacro q [q' db & args]
  (let [{:keys [find first? mapcat? pull keys] :as pq} (apply parse-query q' args)]
    `(let [data# ~(with-meta `(m/rewrites ~db
                                ~(query pq) ~find)
                    (merge {::m/dangerous true} &env (meta &form)))]
       (into ~(if-not (and (seq pull) first?) [] {})
             (comp-some
              ~(when (seq pull)
                 `(map (fn [elem#]
                         (let [q#            '~(:q pull)
                               [?table# ?e#] '~(:ident pull)
                               args-map#     (zipmap '~find elem#)
                               table#        (args-map# ?table#)
                               e#            (args-map# ?e#)]
                           (pull ~db q# [table# e#])))))
              ~(when first? `(take 1))
              ~(when mapcat? `(mapcat identity))
              ~(when keys `(map (fn [m#] (zipmap '~keys m#)))))
             data#))))

(defn tx->datom [tx]
  (m/rewrite tx
    [[?table] :+ {?eid {?a ?v}}]
    [?table ?eid ?a ?v]
    [[?table] :r {?eid {?a ?v}}]
    [?table ?eid ?a nil]
    [[?table ?eid] :+ {?a ?v}]
    [?table ?eid ?a ?v]
    [[?table ?eid] :r {?a ?v}]
    [?table ?eid ?a nil]
    [[?table ?eid ?a] :+ ?v]
    [?table ?eid ?a ?v]
    [[?table ?eid ?a] :r ?v]
    [?table ?eid ?a nil]
    [[?table] :-]
    [?table nil nil nil]
    [[?table ?eid] :-]
    [?table ?eid nil nil]
    [[?table ?eid ?a] :-]
    [?table ?eid ?a nil]))

(defn -tx-match-datom? [tx datom]
  (m/rewrite [(tx->datom tx) datom]
    [[?table ?eid ?a ?v]
     (m/or
      [(m/or ?table (m/pred symbol?))
       (m/or ?eid (m/pred symbol?) (m/guard (nil? ?eid)))
       (m/or ?a (m/pred symbol?) (m/guard (nil? ?a)))
       (m/or ?v (m/pred symbol?) (m/guard (nil? ?v)))]
      [(m/or ?eid (m/pred symbol?) (m/guard (nil? ?eid)))
       (m/or ?a (m/pred symbol?) (m/guard (nil? ?a)))
       (m/or ?v (m/pred symbol?) (m/guard (nil? ?v)))])]
    true
    _
    false))

(defn -last-tx-match-datom? [db datom]
  (-tx-match-datom? (-last-tx db) datom))

(defn -tx-match-where? [db datoms]
  (m/rewrite datoms
    (m/scan (m/app (partial -last-tx-match-datom? db) (m/pred true?))) true
    _ false))

(defn -tx-match-query? [db query]
  (-tx-match-where? db (-> query parse-query :where)))

(defmacro q* [q' db & args]
  `(let [m#     (meta ~db)
         t#     (:t m#)
         subs#  (:subscribers m#)]
     (enc/cond
       (enc/rsome #(enc/kw-identical? :mem/del %) [~@args])
       (enc/do-true
        (swap! subs# assoc-in [(quote ~q') :r] nil)
        (swap! subs# assoc-in [(quote ~q') :t] nil))

       (and (get-in @subs# [(quote ~q') :r])
            (not (enc/rsome #(enc/kw-identical? :mem/fresh %) [~@args]))
            (or (> (get-in @subs# [(quote ~q') :t]) t#)
                (not (-tx-match-query? ~db (quote ~q')))))
       (get-in @subs# [(quote ~q') :r])

       :else
       (let [r# (q ~q' ~db ~@args)]
         (swap! subs# assoc-in [(quote ~q') :r] r#)
         (swap! subs# assoc-in [(quote ~q') :t] (enc/now-udt))
         r#))))

(comment
  )


;; * re-frame

(def dxs_ (atom {}))

(defn valid-id? [id]
  (or (keyword? id) (and (vector? id) (enc/revery? keyword? id))))

(defn reg-dx! [id store]
  (let [id    (enc/have! valid-id? id)
        store (enc/have! enc/derefable? store)]
    (#?(:clj swap! :cljs vswap!) dxs_ assoc id store)))

(defn get-dx [k]
  (enc/have! valid-id? k)
  (@dxs_ k))

(defn get-dx! [k]
  (enc/have! valid-id? k)
  (if-let [dx (@dxs_ k)]
    dx
    (let [dx (*atom-fn* *empty-map*)]
      (reg-dx! k dx)
      dx)))

(defmacro with-dx
  {:style/indent 1}
  [bindings & body]
  {:pre [(even? (count bindings))]}
  (let [s (seq bindings)]
    (if s
      (let [[v k & ?more] bindings]
        `(if-let [~v (@dxs_ ~k)]
           ~(if ?more `(with-dx ~(vec ?more) ~@body) `(do ~@body))
           (throw (ex-info "doxa: dx doesn't exists!" {:dx ~k})))))))

(defmacro with-dx!
  {:style/indent 1}
  [bindings & body]
  {:pre [(even? (count bindings))]}
  (let [s (seq bindings)]
    (if s
      (let [[v k & ?more] bindings]
        `(if-let [~v (@dxs_ ~k)]
           ~(if ?more `(with-dx! ~(vec ?more) ~@body) `(do ~@body))
           (let [~v (*atom-fn* *empty-map*)] ; for testing
             (timbre/warnf "[doxa]: dx %s doesn't exists! creating it!" ~k)
             (reg-dx! ~k ~v)
             ~(if ?more `(with-dx! ~(vec ?more) ~@body) `(do ~@body))))))))
