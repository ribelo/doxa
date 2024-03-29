(ns ribelo.doxa.util
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.ordered-set :as dxos :refer [ordered-set ordered-set?]]
   [ribelo.doxa.protocols :as p]
   [clojure.set :as set])
  #?(:clj
     (:import
      (ribelo.doxa.ordered_set OrderedSet))))

(comment (require '[taoensso.encore :as enc])
         (require '[criterium.core :as cc]))

(defn -key-id? [k]
  (or (ex/kw-identical? :id k) (when (keyword? k) (and (namespace k) (= (name k) "id")))))

(defn -ref-lookup? [xs]
  (and (vector? xs) (= 2 (count xs))
       (-key-id? (nth xs 0))))

(defn -ref-lookups? [xs]
  (and (ordered-set? xs) (-ref-lookup? (first xs))))

(defn -entity? [m]
  (or (get (meta m) :ribelo.doxa/entity-key)
      (and (map? m) (reduce-kv (fn [_ k _] (if (-key-id? k) (reduced true) false)) false m))))

(defn -entities? [xs]
  (and (coll? xs) (ex/every? -entity? xs)))

(defn -dx? [dx]
  (satisfies? p/IDoxa dx))

(defn -probably-dx? [dx]
  (satisfies? p/IDoxa dx))

(defn -entity-ref [m]
  (if-let [tid (get (meta m) :ribelo.doxa/entity-key)]
    ;; TODO
    [tid (get m tid)]
    (when (map? m)
      (reduce-kv
        (fn [_ k v]
          (when (-key-id? k)
            (reduced [k v])))
        nil
        m))))

(defn -entities-refs [xs]
  (when (coll? xs)
    (not-empty (ordered-set (ex/keep -entity-ref xs)))))

(defn -key->rvd [k]
  (keyword (namespace k) (str "_" (name k))))

(defn -rvd->key [k]
  (when (keyword? k)
    (let [s (name k)]
      (when (and (not (ex/str-starts-with? s "__")) (ex/str-starts-with? s "_"))
        (keyword (namespace k) (.substring s 1))))))

(defn -flatten-map [m]
  (persistent!
    (reduce-kv
      (fn [acc k v]
        (if (and (-ref-lookups? v) (= 1 (count v)))
          (assoc! acc k (nth v 0))
          (assoc! acc k v)))
      (transient {})
      m)))

(defrecord DoxaDBChange [e kind a v udt])

#?(:cljs
   (def tranit-write-handlers
     {DoxaDBChange
      (reify Object
        (tag [_ _] "dx/dbc")
        (rep [_ ^js x] [(.-e x) (.-kind x) (.-a x) (.-v x) (.-udt x)])
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))}))

#?(:cljs
   (def transit-read-handlers
     {"dx/dbc" (fn [[e kind a v udt]] (DoxaDBChange. e kind a v udt))}))

(defn -diff-entity
  ([e1 e2] (-diff-entity e1 e2 (ex/now-udt)))
  ([e1 e2 udt]
   (let [ref1 (-entity-ref e1)
         ref2 (-entity-ref e2)]
     (if (or (= ref1 ref2) (nil? ref1) (nil? ref2))
       (let [ref (or ref1 ref2)
             acc (reduce-kv
                   (fn [acc k v1]
                     (if (get e2 k)
                       acc
                       (conj! acc (DoxaDBChange. ref1 :- k v1 udt))))
                   (transient [])
                   e1)]
         (persistent!
           (reduce-kv
             (fn [acc k v2]
               (if-some [v1 (get e1 k)]
                 (if (= v1 v2)
                   acc
                   (-> (conj! acc (DoxaDBChange. ref :- k v1 udt)) (conj! (DoxaDBChange. ref :+ k v2 udt))))
                 (conj! acc (DoxaDBChange. ref :+ k v2 udt))))
             acc
             e2)))
       (throw (ex-info "can't diff entities with difrent key-id" {:eids [ref1 ref2]}))))))

(defn -variable? [x]
  (and (symbol? x) (.startsWith (name x) "?")))

(defn -underscore? [x]
  (= '_ x))

(defn -src-variable? [x]
  (and (symbol? x) (.startsWith (name x) "$")))

(defn -plain-symbol? [x]
  (and (symbol? x) (not (-variable? x)) (not (-underscore? x)) (not (-src-variable? x))))

(defn -dot? [x]
  (= '. x))

(defn -dots? [x]
  (= '... x))

(defn -constant? [x]
  (not (symbol? x)))

(defn -fn? [x]
  (not (symbol? x)))

(defn -pattern [x]
  (cond
    (-variable? x) :?
    (-underscore? x) :_
    (-src-variable? x) :$
    (-plain-symbol? x) :x
    (-constant? x) :c))

(defn -parse-double [[x y]]
  (if (list? x)
    :bind
    [(-pattern x) (-pattern y) nil]))

(defn -parse-datom [datom]
  (case (count datom)
    1 :filter
    2 (-parse-double datom)
    3 (cond
        (vector? datom)
        (mapv -pattern datom)
        (list? datom)
        (condp = (first datom)
          'and :and
          'or  :or))
    (when (= 'and (first datom)) :and)))

(defn -datom-match-change? [datom ^DoxaDBChange change]
  (case (-parse-datom datom)
    :filter nil
    :bind nil
    (let [[e a v] datom]
      (and
       (or (-variable? e) (-underscore? e) (= e (.-e change)))
       (or (-variable? a) (-underscore? a) (= a (.-a change)))
       (or (-variable? v) (-underscore? v) (= v (.-v change)) (nil? v))))))

(defn -datoms-match-changes? [datoms changes]
  (ex/loop-it [datom datoms :let [acc false]]
    (if (true? (ex/loop-it [change changes :let [acc acc]]
                 (if (true? (-datom-match-change? datom change))
                   true
                   (recur acc))
                 acc))
      true
      (recur acc))
    acc))

(defn -search-attr-in-map
  ([m x]
   (ex/loop-it [[k v] m :let [acc (transient #{})]]
     (if (= x v)
       (recur (conj! acc k))
       (recur acc))
     (persistent! acc))))

(defn -eid-search [dx eid]
  (persistent!
    (reduce-kv
      (fn [acc [tid eid'] _]
        (if (ex/kw-identical? eid' eid)
          (conj! acc [tid eid])
          acc))
      (transient [])
      dx)))

(declare -safe-merge-entity)

(defn -merge-normalized-data [data]
  (ex/loop-it [[k v] (ex/group-by first data) :let [acc (transient [])]]
    (recur (conj! acc [k (reduce (fn [acc [_ m]] (-safe-merge-entity acc m)) {} v)]))
    (persistent! acc)))

(defn -normalize
  "turns a nested map into a flat collection with references."
  ([data] (-normalize data {}))
  ([data m]
   (let [it (ex/iter data)]
     (-merge-normalized-data
      (persistent!
       (loop [m (transient m) r (transient []) id nil]
         (cond
           (and (not (.hasNext it)) (nil? id))
           nil

           (and (not (.hasNext it)) id)
           (conj! r [id (persistent! m)])

           :else
           (let [[k v] (.next it)]

             (if (-key-id? k)
               (recur (assoc! m k v) r [k v])

               (if-let [eid (-entity-ref v)]
                 (recur
                  (assoc! m k eid)
                  (reduce conj! r (-normalize v {(-key->rvd k) (-entity-ref data)})) id)

                 (if-let [eids (-entities-refs v)]
                   (recur
                    (assoc! m k eids)
                    (reduce (fn [acc m'] (reduce conj! acc (-normalize m' {(-key->rvd k) (-entity-ref data)}))) r v) id)

                   (cond
                     (and (not (-rvd->key k)) (-ref-lookup? v))
                     (recur
                      (assoc! m k v)
                      (conj! r [v {(first v) (second v) (-key->rvd k) id}])
                      id)

                     (and (not (-rvd->key k)) (and (coll? v) (seq v) (ex/every? -ref-lookup? v)))
                     (recur
                      (assoc! m k (ordered-set v))
                      (reduce
                       (fn [acc [tid eid :as ref']]
                         (conj! acc [ref' {tid eid (-key->rvd k) id}]))
                       r
                       v)
                      id)

                     (or (and (coll? v) (seq v)) (and (not (coll? v)) (some? v)))
                     (recur (assoc! m k v) r id)

                     :else
                     (recur m r id)))))))))))))

(defn -denormalize
  ([data] (-denormalize data data 12 0))
  ([dx data] (-denormalize dx data 12 0))
  ([dx data max-level] (-denormalize dx data max-level 0))
  ([dx data max-level level]
   (let [it (ex/iter data)]
     (loop [m (transient {})]
       (if (or (not (.hasNext it)) (> level max-level))
         (persistent! m)
         (let [[k v] (.next it)]
           (cond
             (map? v)
             (recur (assoc! m k (-denormalize dx v max-level (inc level))))

             (-ref-lookup? v)
             (recur (assoc! m k (or (get-in m v) (-denormalize dx (dx v) max-level (inc level)))))

             (-ref-lookups? v)
             (recur (assoc! m k (mapv (fn [ident] (or (get-in m ident) (-denormalize dx (dx ident) max-level (inc level)))) v)))

             :else
             (recur (assoc! m k v)))))))))

(declare -safe-merge)

(defn -safe-merge-entities [e1 e2]
  (reduce-kv
    (fn [acc k v] (-safe-merge acc k v))
    e1
    e2))

(defn -merge-entity [dx m]
  (reduce
   (fn [acc [ref m]]
     (let [merged (-safe-merge-entities (dx ref {}) m)]
       (p/-put acc ref merged)))
   dx (-normalize m)))

(defn -merge-entities [dx xs]
  (reduce -merge-entity dx xs))

(defn -put-entity [dx m]
  (reduce (fn [acc [ref m]] (p/-put acc ref m)) dx (-normalize m)))

(defn -put-entities [dx xs]
  (reduce -put-entity dx xs))

(defn -safe-merge
  ([m k v]
   (let [x (get m k)]
     (if x
       (cond
         (and (-ref-lookups? x) (-ref-lookups? v))
         (assoc m k (into x v))

         (and (-ref-lookups? x) (-ref-lookup? v))
         (assoc m k (conj x v))

         (and (-ref-lookup? x) (-ref-lookup? v) (not= x v))
         (assoc m k (ordered-set [x v]))

         (and (-ref-lookup? x) (-ref-lookup? v) (not= x v))
         m

         (and (-ref-lookup? x) (-ref-lookups? v))
         (assoc m k (into [x] v))

         (and (not (-key-id? k)) (not (-ref-lookup? x)) (vector? x) (not (vector? v)))
         (assoc m k (conj x v))

         (and (not (-key-id? k)) (not (-ref-lookup? x)) (vector? x) (vector? v))
         (assoc m k (into x v))

         (and (not (-key-id? k)) (not (-ref-lookup? x)) (not (vector? x)) (vector? v))
         (assoc m k (into v [x]))

         :else
         (assoc m k v))
       (assoc m k v))))
  ([dx ref k v]
   (p/-put dx ref (-safe-merge (dx ref) k v))))

(defn -safe-put-kv [dx [tid eid :as ref] k v]
  (if (dx ref)
    (p/-put dx ref (assoc (dx ref) k v))
    (p/-put dx ref {tid eid k v})))

(defn -safe-merge-kv [dx [tid eid :as ref] k v]
  (if (dx ref)
    (p/-put dx ref (-safe-merge (dx ref) k v))
    (p/-put dx ref {tid eid k v})))

(defn -safe-put-kvs [dx ref & kvs]
  (ex/reduce-kvs (fn [acc k v] (-safe-put-kv acc ref k v)) dx kvs))

(defn -safe-merge-entity [e1 e2]
  (reduce-kv
   (fn [acc k v] (-safe-merge acc k v))
   e1
   e2))

(defn -safe-dissoc
  ([dx ref k]
   (let [m (dx ref {})]
     (if-let [m' (not-empty (dissoc m k))]
       (p/-put dx ref m')
       (p/-del dx ref))))
  ([dx ref k v]
   (let [m (dx ref)
         x (get m k)]
     (if x
       (cond
         (-ref-lookups? x)
         (if-let [xs (not-empty (disj x v))]
           (p/-put dx ref (assoc m k xs))
           (if-let [m' (not-empty (dissoc m k))]
             (p/-put dx ref m')
             (p/-del dx ref)))

         (and (not (-ref-lookup? x)) (vector? x))
         (if-let [v (not-empty (ex/remove (partial = v) x))]
           (p/-put dx ref (assoc m k v))
           (if-let [m' (not-empty (dissoc m k))]
             (p/-put dx ref m')
             (p/-del dx ref)))

         (= x v)
         (if-let [m' (not-empty (dissoc m k))]
           (p/-put dx ref m')
           (p/-del dx ref))

         :else
         (throw (ex-info "unexpected!" {})))
       dx))))

;; (ribelo.doxa/commit {} [:dx/merge {:db/id 1 :name "Ivan" :friend [{:db/id 2 :name "Petr"}]}])
;; => {[:db/id 2] {:_friend [:db/id 1], :db/id 2, :name "Petr"}, [:db/id 1] {:db/id 1, :name "Ivan", :friend #ordered/set #{[:db/id 2]}}}

;; -delete-backref dx [:db/id 1] :_friend
(defn -delete-backref
  ([dx ref k]
   (let [xs (get-in dx [ref k])
         fvd (-rvd->key k)]
     (cond
       (-ref-lookup? xs)
       (-> (-safe-dissoc dx ref k) (-safe-dissoc xs fvd ref))

       (-ref-lookups? xs)
       (ex/loop-it [x xs :let [acc (-safe-dissoc dx ref k)]]
         (recur (-safe-dissoc acc x fvd ref))
         acc))))
  ([dx ref k v]
   (let [fvd (-rvd->key k)]
     (-> (-safe-dissoc dx ref k v) (-safe-dissoc v fvd ref)))))

(as-> {} $ ((or $ {}) :a) ((or $ {}) :b) ((or $ {}) :c) ((or $ {}) :d) ((or $ {}) :f))

(defn -delete-ref
  ([dx ref k]
   (let [xs (some-> dx [ref k])
         rvd (-key->rvd k)]
     (cond
       (-ref-lookup? xs)
       (-> (-safe-dissoc dx ref k) (-safe-dissoc xs rvd ref))

       (-ref-lookups? xs)
       (ex/loop-it [x xs :let [acc (-safe-dissoc dx ref k)]]
         (recur (-safe-dissoc acc x rvd ref))
         acc))))
  ([dx ref k v]
   (let [fvd (-key->rvd k)]
     (-> (-safe-dissoc dx ref k v) (-safe-dissoc v fvd ref)))))

(defn -delete-key
  [dx ref k]
  (if-let [m (dx ref)]
    (let [x (get m k)
          dx'
          (cond
            (and (not (-rvd->key k)) (or (-ref-lookup? x) (-ref-lookups? x)))
            (-delete-ref dx ref k)

            (and (-rvd->key k) (or (-ref-lookup? x) (-ref-lookups? x)))
            (-delete-backref dx ref k)

            :else
            (-safe-dissoc dx ref k))]
      (if (not-empty (dx' ref)) dx' (p/-del dx' ref)))
    dx))

(defn -delete-val
  [dx ref k v]
  (if-let [m (dx ref)]
    (let [x (get m k)
          dx'
          (cond
            (and (not (-rvd->key k)) (or (-ref-lookup? x) (-ref-lookups? x)))
            (-delete-ref dx ref k v)

            (and (-rvd->key k) (or (-ref-lookup? x) (-ref-lookups? x)))
            (-delete-backref dx ref k v)

            (coll? x)
            (if-let [xs (not-empty (ex/remove (partial = v) x))]
              (p/-put dx ref (assoc m k xs))
              (p/-put dx ref (dissoc m k)))

            (= x v)
            (p/-put dx ref (dissoc m k))

            :else
            dx)]
      (if (not-empty (dx' ref)) dx' (p/-del dx' ref)))
    dx))

(defn -delete-entity
  ([dx ref]
   (-> (reduce-kv
        (fn [acc k v]
          (if-let [rvd (-rvd->key k)]
            (cond
              (-ref-lookup? v)
              (-delete-val acc v rvd ref)
              (-ref-lookups? v)
              (reduce (fn [acc x] (-delete-val acc x rvd ref)) acc v))
            (cond
              (-ref-lookup? v)
              (-delete-val acc v (-key->rvd k) ref)
              (-ref-lookups? v)
              (reduce (fn [acc x] (-delete-val acc x (-key->rvd k) ref)) acc v)
              :else
              acc)))
        dx
        (dx ref))
       (p/-del ref))))

(defn -delete-table
  [dx table]
  (if-let [xs (some-> dx p/-index (get table))]
    (ex/loop-it [x xs :let [acc dx]]
      (recur (p/-del acc x))
      acc)
    dx))

(deftype CachedResult [delay datoms])

(defn -make-index [db]
  (ex/loop-it [[k _] db :let [acc {}]]
    (let [[tid _ :as ref] k]
      (recur (update acc tid ex/conjs ref)))
    acc))

(defn -update-index [index changes]
  (ex/loop-it [^DoxaDBChange change changes :let [acc index]]
    (let [[tid _ :as ref] (.-e change)]
      (if (ex/kw-identical? tid (.-a change))
        (case (.-kind change)
          :+ (recur (update acc tid ex/conjs ref))
          :- (recur (update acc tid disj ref)))
        (recur acc)))
    acc))
