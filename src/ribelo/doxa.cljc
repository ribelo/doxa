(ns ribelo.doxa
  (:refer-clojure  :exclude [ident? -next])
  #?(:cljs (:require-macros [ribelo.doxa :refer [q with-dx -iter]]))
  (:require
   [meander.epsilon :as m]
   [taoensso.encore :as enc]
   [taoensso.timbre :as timbre]
   [editscript.core :as es]
   [editscript.edit :as ese]))

#?(:clj (set! *warn-on-reflection* true))

(declare reverse-search)

(defmacro ^:private -iter [xs]
  `(enc/if-clj (clojure.lang.RT/iter ~xs) (cljs.core/iter ~xs)))

(def conjv (fnil conj []))

(def ^:private simple-eid?    (some-fn keyword? nat-int? string?))
(def ^:private compound-eid? #(and (vector? %) (enc/revery? simple-eid? %)))
(def ^:private eid?           (some-fn simple-eid? compound-eid?))

(defn key-id? [k]
  (enc/cond
    :when (keyword? k)
    :if-not [ns'   (namespace k)
             name' (name k)]
    false
    (or (#?(:clj = :cljs identical?) name' "id")
        (#?(:clj = :cljs identical?) name' "by-id")
        (#?(:clj = :cljs identical?) name' "list"))))

(comment
  (enc/qb 1e6 (key-id? :db/id))
  ;; => 73.67
  )

(defn #?(:clj ident? :cljs ^boolean ident?) [x]
  (and (vector? x) (key-id? (nth x 0)) (eid? (nth x 1))))

(defn #?(:clj idents? :cljs ^boolean idents?)
  [xs]
  (when (vector? xs)
    (let [it (-iter xs)]
      (loop []
        (enc/cond
          :if-not (.hasNext it) true
          (ident? (.next it)) (recur))))))

(defn db? [db] (map? db))

(defn conn? [conn] (enc/derefable? conn))

(defn #?(:clj entity? :cljs ^boolean entity?)
  [m]
  (when (map? m)
    (let [it (-iter m)]
      (loop []
        (enc/cond
          :if-not (.hasNext it) false
          :let [[k v] (.next it)]
          (and (key-id? k) (eid? v)) true
          :else (recur))))))

(comment
  (let [m (into {} (map (fn [i] [i i])) (range 1e3))]
    (enc/qb 1e4
      (entity? m)))
  ;; => 58.38
  )

(defn #?(:clj entities? :cljs ^boolean entities?)
  [xs]
  (when (vector? xs)
    (let [it (-iter xs)]
      (loop []
        (enc/cond
          :if-not
          (.hasNext it)
          true
          (entity? (.next it))
          (recur))))))

(comment
  (enc/qb 1e5 (entities? [{:db/id 1}]))
  ;; => 18.79
  )

(def not-entities? (complement (some-fn entity? entities?)))

(defn entity-id [m]
  (let [it (-iter m)]
    (loop []
      (enc/cond
        :if-not     (.hasNext it) nil
        :let        [[k v] (.next it)]
        (key-id? k) [k v]
        :else       (recur)))))

(comment
  (entity-id data)
  (enc/qb 1e6 (entity-id {:db/id 1}))
  ;; => 108.46
  )

#?(:clj
   (m/defsyntax dbg [pattern]
     `(m/app #(doto % prn) ~pattern)))

(defn normalize
  [data]
  (let [it (-iter data)]
    (loop [m (transient {}) r [] id nil]
      (enc/cond
        (and (not (.hasNext it)) (some? id))
        (conj r [id (persistent! m)])
        ;;
        :let [[k v] (.next it)]
        (key-id? k)
        (recur (assoc! m k v) r [k v])
        ;;
        (entity? v)
        (recur (assoc! m k [(entity-id v)]) (enc/into! r (normalize v)) id)
        ;;
        (entities? v)
        (recur (assoc! m k (mapv entity-id v))
               (reduce (fn [acc m'] (into acc (normalize m'))) r v)
               id)
        ;;
        (ident? v)
        (recur (assoc! m k [v]) r id)
        ;;
        :else
        (recur (assoc! m k v) r id)))))

(comment
  (enc/qb 1e5 (normalize {:db/id :ivan :name "ivan" :friend [:db/id :petr]}))
  ;; => 79.35
  )

;; {:table {:key/id {k v}}}

(defn denormalize
  ([data]           (denormalize data data 10        0))
  ([data max-level] (denormalize data data max-level 0))
  ([db data max-level level]
   (let [it (-iter data)]
     (loop [m {}]
       (enc/cond
         (> level max-level)
         (timbre/warnf "maximum nesting level %s for %s has been exceeded" max-level (entity-id data))
         (and (not (.hasNext it)))
         m
         ;;
         :let [[k v] (.next it)]
         (map? v)
         (recur (assoc m k (denormalize db v max-level (inc level))))
         ;;
         (idents? v)
         (recur (assoc m k (enc/cond
                             :let [xs (mapv (fn [ident] (or (get-in m ident)
                                                           (denormalize db (get-in db ident) max-level (inc level)))) v)
                                   n  (count xs)]
                             (> n 1) xs
                             (= n 1) (nth xs 0))))
         ;;
         :else
         (recur (assoc m k v)))))))

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
    (denormalize data))[k v] (.next it)
  ;; => 1362.47
  )


(defn submit-commit [db tx]
  (m/find tx
    ;; put [?tid ?eid] ?k ?v
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)]
     (m/pred keyword? ?k) (m/pred (complement (some-fn entity? entities? ident?)) ?v)]
    (assoc-in db [?tid ?eid ?k] ?v)
    ;; put [?tid ?eid] ?k ?ident
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)]
     (m/pred keyword? ?k) (m/pred ident? ?v)]
    (update-in db [?tid ?eid ?k] conjv ?v)
    ;; put [?tid ?eid] ?k ?m
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred entity? ?v)]
    (let [xs (normalize ?v)
          it (-iter xs)]
      (loop [db' (assoc-in db [?tid ?eid ?k] (entity-id ?v))]
        (enc/cond
          :if-not (.hasNext it) db'
          (recur (assoc-in db [?tid ?eid ?k] (entity-id (.next it)))))))
    ;; put [?tid ?eid] ?k [?m ...]
    [:dx/put [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) [(m/pred entity? !vs) ...]]
    (let [itx (-iter !vs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext itx) acc
          :let    [?v (.next itx)
                   xs (normalize ?v)
                   ity (-iter xs)]
          (recur
           (loop [acc (update-in acc [?tid ?eid ?k] conjv (entity-id ?v))]
             (enc/cond
               (not (.hasNext ity)) acc
               :let                 [[ks m] (.next ity)]
               (recur (assoc-in acc ks m))))))))
    ;; put m
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
    (let [it1 (-iter ?vs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext it1) acc
          :let    [?m  (.next it1)
                   xs  (normalize ?m)
                   it2 (-iter xs)]
          :else
          (recur
           (loop [acc' acc]
             (enc/cond
               :if-not (.hasNext it2) acc'
               :let    [[ks m] (.next it2)]
               (recur (assoc-in acc' ks m))))))))
    ;; put [?tid ?eid] m
    [:dx/put [(m/pred key-id? ?tid) (m/pred eid? ?eid)] ?m]
    (let [xs (normalize (assoc ?m ?tid ?eid))
          it (-iter xs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext it) acc
          :let    [[ks m] (.next it)]
          (recur (assoc-in acc ks m)))))
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
          (recur (submit-commit acc [:dx/delete [tid eid] k ?ident])))))
    ;; delete [?tid ?eid] ?k
    [:dx/delete [(m/pred key-id? ?tid) (m/pred eid? ?eid)] ?k]
    (enc/dissoc-in db [?tid ?eid] ?k)
    ;; delete {}
    [:dx/delete {(m/pred key-id? ?tid) (m/pred eid? ?eid)}]
    (submit-commit db [:dx/delete [?tid ?eid]])
    ;; delete [?tid ?eid] ?k ?v
    [:dx/delete [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred some? ?v)]
    (enc/cond
      :if-not                       [v (get-in db [?tid ?eid ?k])] db
      (and (seq v) (> (count v) 1)) (update-in db [?tid ?eid ?k] #(into [] (remove #{?v}) %))
      (and (seq v) (= (count v) 1)) (enc/dissoc-in db [?tid ?eid] ?k)
      (and v (not (vector? v)))     (throw (ex-info (enc/format "%s is not a vector" [?tid ?eid ?k]) {:v v}))
      db)
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
        (submit-commit     [:dx/put ?v]))
    ;; conj [?tid ?eid] ?k [?m ...]
    [:dx/conj [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) [(m/pred entity? !vs) ...]]
    (let [it (-iter !vs)]
      (loop [acc db]
        (enc/cond
          :if-not (.hasNext it) acc
          :let    [v    (.next it)
                   acc' (-> (update-in acc [?tid ?eid ?k] conjv (entity-id v))
                            (submit-commit      [:dx/put v]))]
          :else   (recur acc'))))
    ;; update m
    [:dx/update [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred fn? ?f) & ?args]
    (update-in db [?tid ?eid] (partial apply ?f) ?args)
    ;; update k
    [:dx/update [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred fn? ?f) & ?args]
    (update-in db [?tid ?eid ?k] (partial apply ?f) ?args)
    ;; match [?tid ?eid] ?m
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred entity? ?m)]
    (= ?m (get-in db [?tid ?eid]))
    ;; match [?tid ?eid] ?f
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred ifn? ?f)]
    (?f (get-in db [?tid ?eid]))
    ;; match [?tid ?eid] ?k ?v
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred (complement fn?) ?v)]
    (= ?v (get-in db [?tid ?eid ?k]))
    ;; match [?tid ?eid] ?k ?f
    [:dx/match [(m/pred keyword? ?tid) (m/pred eid? ?eid)] (m/pred keyword? ?k) (m/pred ifn? ?f)]
    (?f (get-in db [?tid ?eid ?k]))
    ;; _
    _ (timbre/errorf "invalid commit %s" tx)))

(comment
  (enc/qb 1e5
    (submit-commit {:db/id {:ivan {:db/id :ivan :name "ivan"}}}
                   [:dx/put [:db/id :ivan] :friend {:db/id :petr :name "petr"}]))
  ;; => 177.95
  (enc/qb 1e5
    (submit-commit {:db/id {:ivan {:db/id :ivan :name "ivan"}}}
                   [:dx/put [:db/id :ivan] :friend [{:db/id :petr :name "petr"}
                                                    {:db/id :smith :name "smith"}]]))
  ;; => 402.66
  )

(defn listen!
  ([db cb] (listen! db (enc/uuid-str) cb))
  ([db k cb]
   (if (meta db)
     (swap! (get (meta db) :listeners (atom {})) assoc k cb)
     (with-meta db (atom {k cb})))
   k))

(comment
  (def tmp (create-dx))
  (listen! tmp #(println :sex))
  (commit tmp [:dx/put [:db/id :ivan] :age 1])
  (es/patch tmp (es/edits->script [[[:db/id] :+ {:ivan {:age 2}}]]))
  )

(defn unlisten!
  ([db k]
   (when (meta db)
     (swap! ((meta db) :listeners) dissoc k))))

(defn commit*
  ([db txs] (commit* db txs nil))
  ([db txs tx-meta]
   (let [db'   (enc/cond
                 (vector? (first txs))
                 (let [it (-iter txs)]
                   (loop [acc db match? true]
                     (enc/cond
                       :if-not (.hasNext it)
                       acc
                       :let [tx (.next it)
                             kind (first tx)]
                       (enc/kw-identical? kind :dx/match)
                       (recur acc (submit-commit acc tx))
                       ;;
                       match?
                       (recur (submit-commit acc tx) match?)
                       ;;
                       (not match?)
                       (recur acc match?))))
                 ;;
                 (keyword? (first txs))
                 (submit-commit db txs))
         meta' (meta db')]
     (cond-> (vary-meta db' assoc :t (enc/now-udt))
       (:with-diff? meta')
       (vary-meta assoc
                  :tx (ese/get-edits (es/diff db db' {:algo :quick}))
                  :h  (hash db'))
       ;;
       tx-meta
       (vary-meta assoc :tx-meta tx-meta)))))

(defn commit
  ([db txs] (commit db txs nil))
  ([db txs tx-meta]
   (let [db' (commit* db txs tx-meta)]
     (when-let [it (some-> (meta db) :listeners deref -iter)]
       (while #?(:clj (.hasNext it) :cljs ^cljs (.hasNext it))
         (let [[_k cb] (.next it)]
           (cb db'))))
     db')))

(defn commit!
  ([db_ txs] (commit! db_ txs nil))
  ([db_ txs tx-meta]
   (swap! db_ (fn [db] (commit db txs tx-meta)))))

(defn patch
  ([db edits]
   (patch db edits (enc/now-udt)))
  ([db edits time]
   (let [db' (es/patch db (es/edits->script edits))]
     (when-let [it (some-> (meta db') :listeners deref -iter)]
       (while #?(:clj (.hasNext it) :cljs ^cljs (.hasNext it))
         (let [[_k cb] (.next it)]
           (cb db'))))
     (vary-meta db' assoc
                :t  time
                :tx edits))))

(defn patch!
  ([db_ edits]
   (patch! db_ edits (enc/now-udt)))
  ([db_ edits time]
   (swap! db_ (fn [db] (patch db edits time)))))

(defn db-with
  ([data] (db-with {} data))
  ([db data]
   (commit* db (mapv (fn [m] [:dx/put m]) data))))

(defn create-dx
  ([]
   (create-dx [] {:with-diff? false}))
  ([data]
   (create-dx data {:with-diff? false}))
  ([data {:keys [with-diff?] :as opts}]
   (with-meta
     (if (not-empty data) (db-with data) {})
     (merge opts {:listeners (atom {}) :t nil :tx nil}))))

(comment
  (commit* {} [:dx/put {}])
  (commit {} txs)

  )



;; pull

(comment
  (def txs
    [{:db/id     :ivan
      :name      "Ivan"
      :last-name "Ivanov"
      :friend    [:db/id :petr]
      :age       30}
     {:db/id     :petr
      :name      "Petr"
      :last-name "Petrov"
      :friend    [[:db/id :smith] [:db/id :ivan]]
      :age       15}
     {:db/id     :smith
      :name      "Smith"
      :last-name "Smith"
      :friend    [:db/id :petr]
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
  (enc/str-starts-with? (name k) "_"))

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
   (vec (m/search db
          {?pid {?eid {?k (m/or ~id (m/scan ~id))}}} [?pid ?eid ?k])))
  ([db k id]
   (vec (m/search db
          {?pid {?eid {~k (m/or ~id (m/scan ~id))}}} [?pid ?eid]))))

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
           (recur
            (enc/assoc-some r elem (enc/cond
                                     :let  [v (get-in db (conj id elem))]
                                     (and (idents? v) (= (count v) 1))
                                     (nth v 0)
                                     :else v))
            id)
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
           (and (some? id) (idents? ref'))
           (recur (enc/assoc-some r (ffirst elem) (enc/cond
                                                    :let [xs (mapv (partial pull* db (second (first elem))) ref')
                                                          n  (count xs)]
                                                    ;;
                                                    (> n 1)
                                                    (into [] (comp (map not-empty) (remove nil?)) xs)
                                                    ;;
                                                    (= n 1)
                                                    (not-empty (first xs)))) id)
           ;;
           (some? id)
           (recur r id)))))))

(defn pull
  ([db query]
   (pull db (second (first query)) (ffirst query)))
  ([db query id]
   (enc/cond
     (ident? id)  (pull* db query id)
     (idents? id) (mapv (fn [id'] (pull* db query id')) id))))

(defn pull-value
  ([db query]
   (-> (pull db (second (first query)) (ffirst query)) vals first))
  ([db query id]
   (-> (pull* db query id) vals first)))

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
     (denormalize db (get-in db x) max-level))))

(comment
  [?name] ["Ivan" "Petr"]                  -> {?name ["Ivan" "Petr"]}
  [[?name ?age]] [["Ivan" 30] ["Petr" 18]] -> {?name ["Ivan" "Petr"]})

;; * datalog

(defn parse-query [q & args]
  (loop [[elem & more] q k nil r {:args (vec args)}]
    (enc/cond
      (not elem)
      r
      ;;
      (keyword? elem)
      (recur more elem r)
      ;;
      (and (= :find k) (list? elem) (= 'pull (first elem)))
      (let [[_ ?q [?table ?e]] elem]
        (recur more k (-> (update   r k conjv ?table)
                          (update     k conjv ?e)
                          (update :pull conjv [?q [?table ?e]]))))
      (and (= :find k) (vector? elem) (list? (first elem)) (= 'pull (ffirst elem)) (= '... (last elem)))
      (let [[_ ?q [?table ?e]] (first elem)]
        (recur more k (-> (update   r k conjv ?table)
                          (update     k conjv ?e)
                          (assoc  :unpack? true)
                          (update :pull conjv [?q [?table ?e]]))))
      ;;
      (and (= :find k) (= '. elem))
      (recur more k (assoc r :first? true))
      ;;
      (and (= :find k) (vector? elem) (= '... (last elem)))
      (recur more k (-> (assoc r :unpack? true)
                        (update k (partial enc/into-all []) (butlast elem))))
      ;;
      ;; (and (= :find k) (list? elem))
      ;; (let [[?fn & ?args] elem]
      ;;   (recur more k (-> (update r k (partial enc/into-all []) ?args)
      ;;                     (update :fns conjv [?fn ?args]))))
      ;;
      :else
      (recur more k (update r k conjv elem)))))

(comment

  (parse-query '[:find [[?e ?name] ...]])
  (parse-query '[:find [?e ...]])
  (q [:find [(pull [:*] [:db/id ?e]) ...]
      :where
      [?e :name]]
    @conn_)

  (q [:find ?e .
      :where
      [?e :document/id 1]]
    {:document/id {1 {:document/id 1}}})
  )

(defn build-args-map [{:keys [in args] :as q}]
  (m/match q
    {:in nil}
    [{}]
    ;;
    {:in   [(m/pred symbol? !xs) ..?n]
     :args (m/or [!ys ..?n] [[!ys ..?n]])}
    [(into {} (map vector !xs !ys))]
    ;;
    {:in   [(m/pred symbol? !xs) (m/pred vector? !zs) ..?n]
     :args [!ys !js ..?n]}
    [(enc/into-all
      {} (map vector !xs !ys) (build-args-map {:in !zs :args !js}))]
    ;;
    {:in   [[(m/pred symbol? !xs)] ...]
     :args [!ys ...]}
    [(into {} (map vector !xs !ys))]
    {:in [[[!xs ...]]]
     :args [[!ys ...]]}
    (mapcat #(build-args-map {:in !xs :args [%]}) !ys)
    ))

(comment
  (-> (parse-query '[:where [?e :name ?name]
                     :in ?name]
                   "Ivan")
      (build-args-map))
  (-> (parse-query '[:in ?name ?age] "ivan" 35)
      (build-args-map))
  (-> (parse-query '[:in [?name] [?age]] ["ivan" "petr"] [30 20])
      (build-args-map))
  (-> (parse-query '[:in [[?name ?age]]] [["ivan" 20] ["petr" 30]])
      (build-args-map))
  (-> (parse-query '[:find ?e
                     :in ?attr [?value]
                     :where [?e ?attr ?value]]
                   :name ["Ivan" "Petr"])
      (build-args-map))
  (-> (parse-query '[:find (count ?e)
                     :in ?attr [?value]
                     :where [?e ?attr ?value]]
                   :name ["Ivan" "Petr"])
      (build-args-map)))

(defn qsymbol? [x]
  (and (symbol? x) (enc/str-starts-with? (name x) "?")))

(defn- some-value
  ([] `(m/some))
  ([?v]
   (enc/cond
     (not (symbol? ?v))
     ?v
     ;;
     (and (symbol? ?v) (qsymbol? ?v))
     `(m/some ~?v)
     ;;
     (symbol? ?v)
     `(m/some (unquote ~?v))))
  ([?s ?v]
   (enc/cond
     ;;
     (and (symbol? ?s) (not (symbol? ?v)))
     `(m/and ~?s ~?v)
     :else ?v)))

(defn datalog->meander [{:keys [where in args] :as q}]
  (let [args-map (build-args-map q)]
    (loop [[arg-map & more] args-map r []]
      (if arg-map
        (let [x (loop [[elem & more] where m {} fns [] vars [] q 0]
                  (enc/cond
                    (and (not elem) (or (pos? (count fns)) (pos? (count vars)) (pos? q)))
                    `(m/and ~@(map (fn [[_ m']] m') m) ~@fns ~@vars)
                    ;;
                    (and (not elem))
                    `~(m 0)
                    ;;
                    :let [[table e k v] (case (count elem) (1 2 3) (into ['_] elem) 4 elem)
                          [?table ?e ?k ?v]    [(get arg-map table table)
                                                (get arg-map e e)
                                                (get arg-map k k)
                                                (get arg-map v v)]]
                    ;; [?e ?k nil]
                    (and (not (list? ?e)) ?k (nil? ?v))
                    (recur more (update-in m [q ?table ?e] merge {?k (some-value)}) fns vars q)
                    ;; [?e ?k ?v]
                    (and (not (list? ?e)) ?k (not (vector? ?v)) (not (qsymbol? ?v)))
                    (recur more (update-in m [q ?table ?e] merge {?k (some-value v ?v)}) fns vars q)
                    ;;
                    (and (not (list? ?e)) ?k (not (vector? ?v)) (qsymbol? ?v))
                    (recur more (update-in m [q ?table ?e] merge {?k (some-value ?v)}) fns vars q)
                    ;; [?e ?k [?t ?ref]]
                    (and (not (list? ?e)) ?k (vector? ?v) (= 2 (count ?v)) (enc/rsome qsymbol? ?v))
                    (recur more (update-in m [q ?table ?e] merge {?k `(m/scan ~?v)}) fns vars (inc q))
                    ;; [?e ?k [!vs ...]]
                    (and (not (list? ?e)) ?k (vector? ?v) (not (enc/rsome qsymbol? ?v)))
                    (recur more (update-in m [q ?table ?e] merge {?k `(m/or ~@?v)}) fns vars q)
                    ;; [(?f)]
                    :let [?fn   (first ?e)
                          !args (rest ?e)]
                    ;;
                    (and (list? ?e) (nil? ?k))
                    (recur more m (conj fns `(m/guard (~?fn ~@!args))) vars q)
                    ;; [(?f) ?x]
                    (and (list? ?e) (symbol? ?k))
                    (recur more m fns (conj vars `(m/let [~?k (~?fn ~@!args)])) q)))]
          (recur more (conj r x)))
        (enc/cond
          (> (count r) 1)
          `(m/or ~@r)
          :else (first r))))))

(comment
  (enc/qb 1e5
        (m/search @conn_
          {_ {?e {:name "Ivan" :friend (m/scan [?t ?f])}
              ?f {:name ?name}}}
          ?name)
        (m/search @conn_
          (m/and {_ {?e {:name "Ivan" :friend (m/scan [?t ?f])}}}
                 {_ {?f {:name ?name}}})
          ?name))


  (-> (parse-query '[:where
                     [:person/id ?e1 :name "Ivan"]
                     [?e2 :name "Ivan"]
                     [(+ ?e1 ?e2) ?x]])
      (datalog->meander))
  (-> (parse-query '[:in [?name]
                     :where
                     [?e :name ?name]]
                   "Ivan")
      (datalog->meander))
  (-> (parse-query '[:in [?name]
                     :where
                     [?e :name ?name]]
                   ["Ivan" "Petr"])
      (datalog->meander))
  (-> (parse-query '[:in [[?name ?age]]
                     :where
                     [?e :name ?name]
                     [?e :age ?age]]
                   [["Ivan" 20] ["Petr" 30]])
      (datalog->meander))
  (-> (parse-query '[:find (pull [:*] [?table ?e])
                     :where
                     [?e :name "Ivan"]])
      (datalog->meander)))

#?(:clj
   (m/defsyntax query [args]
     (let [q (datalog->meander args)]
       `~q)))

(defmacro q [q' db & args]
  (let [{:keys [find first? unpack? pull] :as pq} (apply parse-query q' args)]
    `(let [data# (m/rewrites ~db
                   ~(query pq) ~find)]
       (cond->> data#
         (seq '~pull)
         (map (fn [elem#]
                   (mapv (fn [[?q# [?table# ?e#]]]
                           (let [args-map# (zipmap '~find elem#)
                                 table#    (args-map# ?table#)
                                 e#        (args-map# ?e#)]
                             (pull ~db ?q# [table# e#])))
                         '~pull)))
         ;;
         '~first?
         first
         ;;
         '~unpack?
         (mapcat identity)
         :else vec))))

(comment

  (q [:find ?id
      :where
      [?table ?e :db/id [?table ?id]]]
    @conn_)

  (q [:find [(pull [:*] [?table ?e]) ...]
      :where
      [?table ?e :name ?name]]
    @conn_)
  (q [:find ?e
      :where
      [?e :name "Ivan"]]
    @conn_)

  (parse-query '[:find ?e
                 :in ?name
                 :where
                 [?e :name ?name]]
               "Ivan"))

(comment
  (enc/qb 1e5
    (doall
     (m/rewrite @conn_
       (m/map-of _ (m/map-of _ {:name !name}))
       [!name ...]))
    (doall
     (m/search @conn_
       {_ {_ {:name ?name}}}
       ?name))))


;; * re-frame

(def dxs_ (atom {}))

(defn reg-dx! [id store]
  (let [id    (enc/have keyword? id)
        store (enc/have enc/derefable? store)]
    (swap! dxs_ assoc id store)))

#?(:clj
   (defmacro with-dx
     {:style/indent 1}
     [bindings & body]
     {:pre [(even? (count bindings))]}
     `(let [~@(mapcat (fn [[v k]] [v `(@dxs_ ~k)]) (partition 2 bindings))]
        ~@body)))

;; (defmacro with-dx2
;;      {:style/indent 1}
;;      [[db store] & body]
;;      `(let [~db (@dxs_ ~store)]
;;         ~@body))
