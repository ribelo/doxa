(ns ribelo.doxa
  (:refer-clojure :exclude [ident? -next])
  (:require
   [clojure.set :as set]
   [ribelo.doxa.impl :refer [-put -get -get-in -put -put-in -del -del-in -keys -k -v -deque -add-first -add-last]]))

(comment
  (require '[taoensso.encore :as enc]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private sentinel #?(:clj (Object.) :cljs (js/Object.)))

(defn- -iter
  "returns an iterator for both clj and cljs.
  while there is an iter function in cljs, there isn't and there won't be one in
  clj, which is a pity because iterator is much faster than reduce."
  ^java.util.Iterator [xs]
  #?(:clj
     (.iterator ^java.lang.Iterable xs)
     :cljs
     (iter xs)))

(defn- -kw-identical? [x y]
  #?(:clj
     (identical? x y)
     :cljs
     (keyword-identical? x y)))

(defn- -first-key
  [xs]
  (first (keys xs)))

(defn- -first-val
  [xs]
  (first (vals xs)))

(defn- -merge [& maps]
  (reduce (fn [acc m] (persistent! (reduce-kv (fn [acc k v] (assoc! acc k v)) (transient acc) m))) {} maps))

(defn- -revery? [pred xs]
  (reduce (fn [_ x] (if (pred x) true (reduced false))) true xs))

(defn- -rsome? [pred xs]
  (reduce (fn [_ x] (if (pred x) (reduced true) false)) false xs))

(defn- -reduce-kvs
  "Like `reduce-kv` but takes a flat sequence of kv pairs."
  [rf init kvs]
  (transduce (partition-all 2) (completing (fn [acc [k v]] (rf acc k v))) init kvs))

(defn- -simple-eid? [x]
  (or (keyword? x) (int? x) (string? x)))

(defn- -compound-eid? [xs]
  (and (vector? xs) (-revery? -simple-eid? xs)))

(defn- -eid? [x]
  (or (-simple-eid? x) (-compound-eid? x)))

(defn- -key-id? [k]
  (when (or (keyword? k) (string? k)) (.endsWith (name k) "id")))

(defn- -ref-lookup? [xs]
  (and (vector? xs) (= 2 (count xs))
       (-key-id? (nth xs 0))
       (-eid? (nth xs 1))))

(defn- -ref-lookups? [xs]
  (and (vector? xs) (-revery? -ref-lookup? xs)))

(defn- -entity? [m]
  (and (map? m) (reduce-kv (fn [_ k v] (if (and (-key-id? k) (-eid? v)) (reduced true) false)) false m)))

(defn- -entities? [xs]
  (and (vector? xs) (-revery? -entity? xs)))

(defn- -entity-ref [m]
  (when (map? m)
    (reduce-kv
      (fn [_ k v] (if (and (-key-id? k) (-eid? v)) (reduced [k v]) nil))
      nil
      m)))

(defn- -entities-refs [xs]
  (when (vector? xs) (not-empty (into #{} (keep -entity-ref) xs))))

(defn normalize
  "turns a nested map into a flat collection with references."
  [data]
  (let [it (-iter data)]
    (persistent!
      (loop [m (transient {}) r (transient []) id nil]
        (cond
          (and (not (.hasNext it)) (nil? id))
          nil

          (and (not (.hasNext it)) id)
          (conj! r [id (persistent! m)])

          :else
          (let [me (.next it)
                k (-k me)
                v (-v me)]
            (if (-key-id? k)
              (recur (assoc! m k v) r [k v])

              (if-let [eid (-entity-ref v)]
                (recur (assoc! m k [eid]) (reduce conj! r (normalize v)) id)

                (if-let [eids (-entities-refs v)]
                  (recur (assoc! m k eids) (reduce (fn [acc m'] (reduce conj! acc (normalize m'))) r v) id)

                  (cond
                    (-ref-lookup? v)
                    (recur (assoc! m k [v]) r id)

                    :else
                    (recur (assoc! m k v) r id)))))))))))

(defn- -denormalize
  ([db data max-level level]
   (let [it (-iter data)]
     (loop [m (transient {})]
       (if (or (not (.hasNext it)) (> level max-level))
         (persistent! m)
         (let [me (.next it)
               k (-k me)
               v (-v me)]
           (cond
             (map? v)
             (recur (assoc! m k (-denormalize db v max-level (inc level))))

             (-ref-lookup? v)
             (recur (assoc! m k (or (get-in m v) (-denormalize db (-get-in db v) max-level (inc level)))))

             (-ref-lookups? v)
             (recur (assoc! m k (mapv (fn [ident] (or (get-in m ident) (-denormalize db (-get-in db ident) max-level (inc level)))) v)))

             :else
             (recur (assoc! m k v)))))))))

(defn denormalize
  "turns a flat map into a nested one. to avoid stackoverflow and infinite loop,
  it takes a maximum nesting level as an additional argument"
  ([   data          ] (-denormalize data data 12        0))
  ([db data          ] (-denormalize db   data 12        0))
  ([db data max-level] (-denormalize db   data max-level 0)))

(defn tx->lookup-ref [tx]
  (let [ref-or-entity (nth tx 1)]
    (case (count tx)
      2 (or (-entity-ref ref-or-entity) (-entities-refs ref-or-entity))
      3 (when (-ref-lookup? ref-or-entity) ref-or-entity)
      4 (when (-ref-lookup? ref-or-entity) ref-or-entity)
      nil)))

(defn- -search-in-map
  ([m x]
   (into [] (keep (fn [k] (-search-in-map m k x))) (keys m)))
  ([m k x]
   (let [v (-get m k)]
     (when (or (= v x)
               (cond
                 (set? v) (v x)
                 (seqable? v) (-rsome? #{x} v)))
       k))))

(defn- -eid-search [db eid]
  (persistent!
    (reduce-kv
      (fn [acc tid table]
        (reduce-kv
          (fn [acc eid' _entity]
            (if (-kw-identical? eid' eid)
              (conj! acc [tid eid])
              acc))
          acc
          table))
      (transient [])
      db)))

(defn- -reverse-search
  ([db v]
   (-reverse-search db sentinel v))
  ([db k v]
   (persistent!
     (reduce
       (fn [acc tid]
         (reduce
           (fn [acc eid]
             (if-let [xs (if (= sentinel k)
                           (-search-in-map (-get-in db [tid eid]) v)
                           (-search-in-map (-get-in db [tid eid]) k v))]
               (reduce conj! acc (mapv (fn [x] [tid eid x]) xs))
               acc))
           acc
           (-keys (-get db tid))))
       (transient [])
       (-keys db)))))

(defn diff-entity
  ([[tid eid :as _ref] e1 e2]
   (mapv (fn [diff] (-> (-add-first diff eid) (-add-first tid) vec)) (-diff-entity e1 e2)))
  ([e1 e2]
   (let [it1 (-iter (or e2 {}))
         it2 (-iter (or e1 {}))
         diff
         (loop [acc (-deque)]
           (if (.hasNext it1)
             (let [me (.next it1)
                   k (-k me)
                   v2 (-v me)]
               (if-let [v1 (-get e1 k)]
                 (if (= v1 v2)
                   (recur acc)
                   (recur (-> (-add-last acc (-deque [:- k v1])) (-add-last (-deque [:+ k v2])))))
                 (recur (-add-last acc (-deque [:+ k v2])))))
             acc))]
     (loop [acc diff]
       (if (.hasNext it2)
         (let [me (.next it2)
               k (-k me)
               v1 (-get e1 k)]
           (if (-get e2 k)
             (recur acc)
             (recur (-add-first acc (-deque [:- k v1])))))
         acc)))))

(defn diff-dbs [odb ndb]
  (let [itn1 (-iter (-keys ndb))
        ito1 (-iter (-keys odb))
        diff (loop [acc (-deque)]
               (if (.hasNext itn1)
                 (let [tid (.next itn1)
                       table (-get ndb tid)
                       itn2 (-iter (-keys table))]
                   (recur
                     (loop [acc acc]
                       (if (.hasNext itn2)
                         (let [eid (.next itn2)
                               e   (-get-in ndb [tid eid] {})]
                           (recur (reduce -add-last acc (-diff-entity odb [tid eid] e))))
                         acc))))
                 acc))]
    (vec
      (loop [acc diff]
        (if (.hasNext ito1)
          (let [tid (.next ito1)
                table (-get odb tid)
                ito2 (-iter (-keys table))]
            (recur
              (loop [acc acc]
                (if (.hasNext ito2)
                  (let [eid (.next ito2)
                        e (-get-in ndb [tid eid] {})]
                    (recur (reduce -add-first acc (-diff-entity odb [tid eid] e))))
                  acc))))
          acc)))))

(defn -diff-entities
  [db xs]
  (persistent!
    (reduce
      (fn [acc e]
        (let [ref (-entity-ref e)]
          (reduce conj! acc (-diff-entity db ref e))))
      (transient [])
      xs)))

(defn- -merge-entity [db m]
  (reduce (fn [acc [ks m]] (-put-in acc ks (-merge (-get-in acc ks) m))) db (normalize m)))

(defn- -merge-entities [db xs]
  (reduce -merge-entity db xs))

(defn- -put-entity [db m]
  (reduce (fn [acc [ks m]] (-put-in acc ks m)) db (normalize m)))

(defn- -put-entities [db xs]
  (reduce -put-entity db xs))

(defn- -put-kv [db [tid eid] k v]
  (if (-get-in db [tid eid])
    (-put-in db [tid eid k] v)
    (-put-in db [tid eid] {tid eid k v})))

(defn- -put-kvs [db ref & kvs]
  (-reduce-kvs (fn [acc k v] (-put-kv acc ref k v)) db kvs))

(defmulti -submit-commit (fn [_db ^clojure.lang.IPersistentVector tx] (nth tx 0)))

(defmulti -submit-failure
  (fn
    ([tx] (nth tx 0))
    ([tx _msg] (nth tx 0))))

(defmethod -submit-failure :default
  ([tx] (throw (ex-info "Using default submit error handler, consider using your own!" {:tx tx})))
  ([tx msg] (throw (ex-info msg {:tx tx}))))

(defmulti -diff-commit (fn [_old-db _new-db ^clojure.lang.IPersistentVector tx] (nth tx 0)))

(defmethod -diff-commit :default [old-db new-db tx]
  (println "Using default brute force diff handler, consider using your own!" {:tx tx})
  (diff-dbs old-db new-db))

(defn default-commit-diff [old-db new-db tx]
  (if-let [ref (tx->lookup-ref tx)]
    (cond
      (-ref-lookup? ref)
      (let [oe (-get-in old-db ref)
            ne (-get-in new-db ref)]
        (diff-entity ref oe ne))

      (-ref-lookups? ref)
      (persistent!
        (reduce
          (fn [acc ref]
            (let [oe (-get-in old-db ref)
                  ne (-get-in new-db ref)]
              (conj! acc (diff-entity ref oe ne))))
          (transient [])
          ref))
      :else
      [])
    []))

(defmethod -diff-commit :dx/put
  [old-db new-db tx]
  (default-commit-diff old-db new-db tx))

(defmethod -diff-commit :dx/delete
  [old-db new-db tx]
  (default-commit-diff old-db new-db tx))

(defmethod -diff-commit :dx/update
  [old-db new-db tx]
  (default-commit-diff old-db new-db tx))

(defmethod -submit-failure :dx/put
  ([tx]
   (throw (ex-info "Invalid put commit" {:tx tx})))
  ([tx msg]
   (throw (ex-info msg {:tx tx}))))

(defmethod -submit-commit :dx/put
  [db tx]
  (let [cnt (count tx)]
    (case cnt
      2 (let [x (nth tx 1)]
          (cond
            (-entity? x)
            (-put-entity db x)

            (-entities? x)
            (-put-entities db x)))

      (let [ref (nth tx 1)
            tid (nth ref 0)
            eid (nth ref 1)]

        (when-not (-ref-lookup? ref)
          (-submit-failure tx))

        (case cnt
          3 (let [m (nth tx 2)]
              (if-let [ref (-entity-ref m)]
                (if (-kw-identical? eid ref)
                  (-put-entity db m)
                  (-submit-failure tx))
                (if (map? m)
                  (-put-entity db (-put m tid eid))
                  (-submit-failure tx))))

          4 (let [k (nth tx 2)
                  v (nth tx 3)]
              (if (-entity? v)
                (-> (-put-kv db ref k (-entity-ref v))
                    (-put-entity v))

                (if-let [ids (-entities-refs v)]
                  (-> (-put-kv db ref k ids)
                      (-merge-entities v))

                  (if (-ref-lookup? v)
                    (if (-get-in db v)
                      (-put-kv db ref k v)
                      (-submit-failure tx))
                    (-put-kv db ref k v)))))

          (if (pos? cnt)
            (apply -put-kvs db ref (into [] (drop 2) tx))
            (-submit-failure tx)))))))

(defn- -safe-delete
  ([db ks] (-safe-delete db ks ::dissoc))
  ([db [tid eid k] v]
   (let [dissoc? (-kw-identical? ::dissoc v)
         db' (if dissoc? (-del-in db [tid eid k]) (-del-in db [tid eid k] v))]
     (if (not-empty (if dissoc? (-get-in db' [tid eid]) (-get-in db' [tid eid k])))
       db'
       (let [db' (if dissoc? (-del-in db' [tid eid]) (-del-in db' [tid eid k]))]
         (if (not-empty (-get db' tid))
           db'
           (-del db' tid)))))))

(defn- -delete-entity [db [tid _ :as ref]]
   (reduce
     (fn [acc ks]
       (-safe-delete acc ks ref))
     (let [db' (-del-in db ref)]
       (if (empty? (-get db' tid))
         (-del db tid)
         db'))
     (-reverse-search db ref)))

(defmethod -submit-failure :dx/delete
  ([tx]
   (throw (ex-info "Invalid delete commit" {:tx tx})))
  ([tx msg]
   (throw (ex-info msg {:tx tx}))))

(defmethod -submit-commit :dx/delete
  [db tx]
  (let [cnt (count tx)]
    (case cnt
      2 (let [x (nth tx 1)
              ref (-entity-ref x)]
          (cond
            (some? ref)
            (-delete-entity db ref)

            (-ref-lookup? x)
            (-delete-entity db x)))

      3 (let [ref (nth tx 1)
              tid (nth ref 0)
              eid (nth ref 1)
              k (nth tx 2)]
          (-safe-delete db [tid eid k]))

      4 (let [ref (nth tx 1)
              tid (nth ref 0)
              eid (nth ref 1)
              k (nth tx 2)
              x (nth tx 3)]
          (-safe-delete db [tid eid k] x)))))

(defmethod -submit-commit :dx/update
  [db tx]
  (let [cnt (count tx)
        ref (nth tx 1)
        tid (nth ref 0)
        eid (nth ref 1)]
    (if (>= cnt 3)
      (let [k-or-f (nth tx 2)]
        (cond
          (keyword? k-or-f)
          (let [k k-or-f
                f (nth tx 3)
                x (-get-in db [tid eid k])]
            (-put-in db [tid eid k] (apply f x (drop 4 tx))))

          (fn? k-or-f)
          (let [f k-or-f
                m (-get-in db [tid eid])]
            (-put-in db [tid eid] (apply f m (drop 3 tx))))

          :else
          (-submit-failure tx)))
      (-submit-failure tx))))

(defmethod -submit-commit :dx/match
  [db tx]
  (let [cnt (count tx)
        ref (nth tx 1)
        e (-get-in db ref)
        x (nth tx 2)]
    (case cnt
      3 (cond
          (fn? x)
          (x e)

          (map? x)
          (reduce-kv
            (fn [_ k v]
              (if (= v (-get e k)) true (reduced false)))
            false
            x))

      4 (let [v (nth tx 3)]
          (if (fn? v)
            (v (-get e x))
            (if-not (fn? x)
              (= v (-get e x))
              (-submit-failure tx "function should be the 3rd element")))))))

(defn- -commit-many
  ([db txs] (-commit-many db txs nil))
  ([db txs meta']
   (let [it (-iter txs)
         incremental? (-get meta' ::incremental?)]
     (loop [acc db changes (transient []) match? true]
       (if (.hasNext it)
         (if-let [tx (.next it)]
           (let [kind (nth tx 0)]
             (if (-kw-identical? :dx/match kind)
               (recur acc changes (-submit-commit acc tx))
               (if match?
                 (let [db' (-submit-commit acc tx)
                       diff (when incremental? (-diff-commit db db' tx))]
                   (recur db' (reduce conj! changes diff) match?))
                 (recur acc changes match?))))
           (recur acc changes match?))
         [acc (persistent! changes)])))))

(defn -commit
  ([db txs] (-commit db txs nil))
  ([db txs meta']
   (let [[db' changes]
         (if (vector? (nth txs 0))
           (-commit-many db txs meta')
           (-commit-many db [txs] meta'))]
     {:db-before db
      :db-after db'
      :tx-data txs
      :changes changes})))

(defn commit [db txs]
  (-get (-commit db txs) :db-after))

(defn commit! [db_ txs]
  (let [report (-commit @db_ txs (meta db_))]
    (reset! db_ (-get report :db-after))
    (alter-meta! db_ update :changes (fn [atm_] (let [changes (-get report :changes)] (if atm_ (reset! atm_ changes) (atom changes)))))
    (alter-meta! db_ assoc :tx-meta (-get report :tx-meta))
    db_))

(defn db-with
  ([data] (db-with {} data))
  ([db data]
   (-commit-many db (mapv (fn [m] [:dx/put m]) data))))

(defn create-dx
  "creates a db, can take a map, which is written to the metadata"
  ([]
   (create-dx [] {::incremental? true}))
  ([data]
   (create-dx data {::incremental? true}))
  ([data opts]
   (create-dx (atom nil :meta opts) data opts))
  ([atm_ data opts]
   (let [empty-db {}
         db (if (not-empty data) (db-with empty-db data) empty-db)]
     (reset! atm_ db)
     (when
       (::incremental? opts)
       (alter-meta! atm_ assoc :changes (atom [])))
     atm_)))

(defn pull
  ([db query]
   (pull db query nil))
  ([db query parent]
   (cond
     (-ref-lookups? parent)
     (mapv #(pull db query %) parent)

     (= [:*] query)
     (get-in db parent)

     (not (-ref-lookup? parent))
     (mapv #(pull db query %) (-eid-search db parent))

     :else
     (let [qit (-iter query)]
       (loop [r {} id parent]
         (enc/cond
           (not (.hasNext qit))
           r
           ;;
           :let  [elem (.next qit)]
           (and (map? elem) (-ident? (first-key elem)))
           (recur (pull db (first-val elem) (first-key elem)) id)
           ;; prop
           (and (some? id) (#{:*} elem))
           (recur
             (let [m  (get-in db id)
                   mit (-iter m)]
               (loop [acc (transient {})]
                 (enc/cond
                   (not (.hasNext mit))
                   (persistent! acc)
                   ;;
                   :let [elem (.next mit)
                         k    (nth elem 0)
                         v    (nth elem 1)]
                   ;;
                   (and (-idents? v) (= 1 (count v)))
                   (recur (assoc! acc k (nth v 0)))
                   ;;
                   :else
                   (recur (assoc! acc k v)))))
             id)
           ;;
           (and (some? id) (keyword? elem) (not (-rev-keyword? elem)))
           (let [v (get-in db (conj id elem))
                 one? (some-> (meta v) ::one?)
                 v' (if one? (nth v 0) v)]
             (recur (enc/assoc-some r elem v') id))
           ;;
           (and (some? id) (keyword? elem) (-rev-keyword? elem))
           (let [k (-rev->keyword elem)]
             (recur
               (enc/assoc-some r elem (enc/cond
                                        :let  [v (reverse-search db k id)]
                                        (and (-idents? v) (= (count v) 1))
                                        (nth v 0)
                                        :else v))
               id))
           ;;
           (and (some? id) (and (seq elem) (second elem) (enc/revery? keyword? (second elem))))
           (let [k   (nth elem 0)
                 eit (-iter (nth elem 1))]
             (loop [acc (transient {})]
               (enc/cond
                 (not (.hasNext eit))
                 (persistent! acc)
                 ;;
                 :let [kk (.next eit)]
                 (-rev-keyword? k)
                 (let [rk (-rev->keyword elem)]
                   (recur
                     (enc/assoc-some acc k (enc/cond
                                             :let  [v (reverse-search db rk id)]
                                             (and (-idents? v) (= (count v) 1))
                                             (nth v 0)
                                             :else v))))
                 :let [v' (get-in db (conj id kk))]
                 (some? v')
                 (recur (assoc! acc kk v'))
                 :else
                 (recur acc))))
           ;; join
           :when (and (some? id) (map? elem)) ;; {:friend [:name]}
           :let  [k     (first-key elem)
                  rev?  (-rev-keyword? k)
                  ref'  (if-not rev?
                          (get-in db (conj parent k))
                          (reverse-search db (-rev->keyword k) id))
                  one?  (some-> (meta ref') ::one?)
                  ref' (if one? (nth ref' 0) ref')]
           ;;
           (and (some? id) (or one? (-ident? ref')))
           (recur (enc/assoc-some r k (pull db (first-val elem) ref)) id)
           ;;
           (and (some? id) (-idents? ref') (not rev?))
           (let [rit (-iter ref')]
             (loop [acc (transient [])]
               (enc/cond
                 (not (.hasNext rit))
                 (enc/assoc-some r (first-key elem) (not-empty (persistent! acc)))
                 ;;
                 :let [ref' (.next rit)
                       k (nth ref' 0)
                       v (first-val elem)]
                 ;;
                 (map? v)
                 (let [q (get v k)]
                   (recur
                     (if q
                       (conj! acc (pull db q ref'))
                       acc)))
                 (vector? v)
                 (recur
                   (let [r (pull db v ref')]
                     (if (not-empty r) (conj! acc r) acc))))))
           ;;
           (and (some? id) (-idents? ref') rev?)
           (recur (enc/assoc-some r (first-key elem)
                                  (enc/cond
                                    :let [xs (mapv #(pull db (first-val elem) % env) ref')
                                          n  (count xs)]
                                    ;;
                                    (> n 1)
                                    (into [] (filter not-empty) xs)
                                    ;;
                                    (= n 1)
                                    (not-empty (first xs))))
                  id)
           ;;
           (some? id)
           (recur r id)))))))
