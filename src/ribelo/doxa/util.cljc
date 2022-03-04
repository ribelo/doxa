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
  (or (ex/-kw-identical? :id k) (when (keyword? k) (and (namespace k) (= (name k) "id")))))

(defn -ref-lookup? [xs]
  (and (vector? xs) (= 2 (count xs))
       (-key-id? (nth xs 0))))

(defn -ref-lookups? [xs]
  (ordered-set? xs))

(defn -entity? [m]
  (or
    (ex/-get (meta m) :ribelo.doxa/entity-key)
    (and (map? m) (ex/-reduce-kv (fn [_ k _] (if (-key-id? k) (reduced true) false)) false m))))

(defn -entities? [xs]
  (and (seqable? xs) (ex/-every? -entity? xs)))

(defn -dx? [dx]
  (satisfies? p/IDoxa dx))

(defn -probably-dx? [dx]
  (satisfies? p/IDoxa dx))

(defn -entity-ref [m]
  (if-let [tid (ex/-get (meta m) :ribelo.doxa/entity-key)]
    ;; TODO
    [tid (ex/-get m tid)]
    (when (map? m)
      (ex/-reduce-kv
        (fn [_ k v]
          (when (-key-id? k)
            (reduced [k v])))
        nil
        m))))

(defn -entities-refs [xs]
  (when (vector? xs)
    (ex/-not-empty (ordered-set (ex/-keep -entity-ref xs)))))

(defn -key->rev [k]
  (keyword (namespace k) (str "_" (name k))))

(defn -rev->key [k]
  (when (keyword? k)
    (let [s (name k)]
      (when (and (not (ex/-str-starts-with? s "__")) (ex/-str-starts-with? s "_"))
        (keyword (namespace k) (.substring s 1))))))

(defn -flatten-map [m]
  (persistent!
    (ex/-reduce-kv
      (fn [acc k v]
        (if (and (-ref-lookups? v) (= 1 (ex/-count* v)))
          (assoc! acc k (nth v 0))
          (assoc! acc k v)))
      (transient {})
      m)))

(deftype DoxaDBChange [e kind a v udt])

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
  ([e1 e2] (-diff-entity e1 e2 (ex/-now-udt)))
  ([e1 e2 udt]
   (let [ref1 (-entity-ref e1)
         ref2 (-entity-ref e2)]
     (if (or (= ref1 ref2) (nil? ref1) (nil? ref2))
       (let [ref (or ref1 ref2)
             acc (ex/-reduce-kv
                   (fn [acc k v1]
                     (if (ex/-get* e2 k)
                       acc
                       (conj! acc (DoxaDBChange. ref1 :- k v1 udt))))
                   (transient [])
                   e1)]
         (persistent!
           (ex/-reduce-kv
             (fn [acc k v2]
               (if-some [v1 (ex/-get* e1 k)]
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

(defn -patern [x]
  (cond
    (-variable? x) :?
    (-underscore? x) :_
    (-src-variable? x) :$
    (-plain-symbol? x) :x
    (-constant? x) :c))

(defn -parse-double [[x y]]
  (if (list? x)
    :bind
    [(-patern x) (-patern y) nil]))

(defn -parse-datom [datom]
  (case (count datom)
    1 :filter
    2 (-parse-double datom)
    3 (cond
        (vector? datom)
        (ex/-mapv -patern datom)
        (list? datom)
        (condp = (ex/-first datom)
          'and :and
          'or  :or))
    (when (= 'and (ex/-first datom)) :and)))

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
  (ex/-loop [datom datoms :let [acc false]]
    (if (true? (ex/-loop [change changes :let [acc acc]]
                 (if (true? (-datom-match-change? datom change))
                   true
                   (recur acc))
                 acc))
      true
      (recur acc))
    acc))

(defn -search-attr-in-map
  ([m x]
   (ex/-loop [me m :let [acc (transient #{})]]
     (if (= x (ex/-v me))
       (recur (conj! acc (ex/-k me)))
       (recur acc))
     (persistent! acc))))

(defn -search-in-map
  ([m x]
   (ex/-keep (fn [me] (-search-in-map m (ex/-k me) x)) m))
  ([m k x]
   (let [v (ex/-get m k)]
     (when (or (= v x)
               (cond
                 (set? v) (v x)
                 (ordered-set? v) (contains? v x)
                 (seqable? v) (ex/-some= x v)))
       k))))

(defn -eid-search [dx eid]
  (persistent!
    (ex/-reduce-kv
      (fn [acc [tid eid'] _]
        (if (ex/-kw-identical? eid' eid)
          (conj! acc [tid eid])
          acc))
      (transient [])
      dx)))

(declare -safe-merge-entity)

(defn -merge-normalized-data [data]
  (ex/-loop [me (ex/-group-by ex/-first data) :let [acc (transient [])]]
    (recur (conj! acc [(ex/-k* me) (ex/-reduce (fn [acc [_ m]] (-safe-merge-entity acc m)) {} (ex/-v* me))]))
    (persistent! acc)))

(defn -normalize
  "turns a nested map into a flat collection with references."
  ([data] (-normalize data {}))
  ([data m]
   (let [it (ex/-iter data)]
     (-merge-normalized-data
       (persistent!
         (loop [m (transient m) r (transient []) id nil]
           (cond
             (and (not (.hasNext it)) (nil? id))
             nil

             (and (not (.hasNext it)) id)
             (conj! r [id (persistent! m)])

             :else
             (let [me (.next it)
                   k (ex/-k* me)
                   v (ex/-v* me)]

               (if (-key-id? k)
                 (recur (ex/-assoc!* m k v) r [k v])

                 (if-let [eid (-entity-ref v)]
                   (recur
                     (ex/-assoc!* m k eid)
                     (ex/-reduce conj! r (-normalize v {(-key->rev k) (-entity-ref data)})) id)

                   (if-let [eids (-entities-refs v)]
                     (recur
                       (ex/-assoc!* m k eids)
                       (ex/-reduce (fn [acc m'] (ex/-reduce conj! acc (-normalize m' {(-key->rev k) (-entity-ref data)}))) r v) id)

                     (cond
                       (and (not (-rev->key k)) (-ref-lookup? v))
                       (recur
                         (ex/-assoc!* m k v)
                         (conj! r [v {(ex/-first v) (ex/-second v) (-key->rev k) id}])
                         id)

                       (and (not (-rev->key k)) (and (coll? v) (ex/-every? -ref-lookup? v)))
                       (recur
                         (ex/-assoc!* m k (ordered-set v))
                         (ex/-reduce
                           (fn [acc [tid eid :as ref']]
                             (conj! acc [ref' {tid eid (-key->rev k) id}]))
                           r
                           v)
                         id)

                       :else
                       (recur (ex/-assoc!* m k v) r id)))))))))))))

(defn -denormalize
  ([data] (-denormalize data data 12 0))
  ([dx data] (-denormalize dx data 12 0))
  ([dx data max-level] (-denormalize dx data max-level 0))
  ([dx data max-level level]
   (let [it (ex/-iter data)]
     (loop [m (transient {})]
       (if (or (not (.hasNext it)) (> level max-level))
         (persistent! m)
         (let [me (.next it)
               k (ex/-k me)
               v (ex/-v me)]
           (cond
             (map? v)
             (recur (ex/-assoc!* m k (-denormalize dx v max-level (inc level))))

             (-ref-lookup? v)
             (recur (ex/-assoc!* m k (or (get-in m v) (-denormalize dx (ex/-get* dx v) max-level (inc level)))))

             (-ref-lookups? v)
             (recur (ex/-assoc!* m k (mapv (fn [ident] (or (get-in m ident) (-denormalize dx (ex/-get* dx ident) max-level (inc level)))) v)))

             :else
             (recur (ex/-assoc!* m k v)))))))))

(defn -safe-put-v [m k v]
  (if-some [ov (and m (ex/-get* m k))]
    (cond
      (-ref-lookup? ov)
      (ex/-assoc* m k (ordered-set [ov v]))

      (and (-ref-lookups? ov) (-ref-lookup? v))
      (ex/-assoc* m k (conj ov v))

      (and (-ref-lookups? ov) (-ref-lookups? v))
      (ex/-assoc* m k (set/union ov v))

      (and (not (-key-id? k))
           (or (vector? ov) (set? ov))
           (not (or (vector? v) (set? v))))
      (ex/-assoc* m k (conj ov v))
        
      (and (not (-key-id? k)) (not (-key-id? k))
           (or (vector? ov) (set? ov))
           (or (vector? v) (set? v)))
      (ex/-assoc* m k (into ov v))

      (and (not (-key-id? k))
           (not (or (vector? ov) (set? ov)))
           (or (vector? v) (set? v)))
      (ex/-assoc* m k (conj v ov))

      :else
      (ex/-assoc* m k v))
    (ex/-assoc* m k v)))

(defn -safe-merge-entity [e1 e2]
  (ex/-reduce-kv
    (fn [acc k v] (-safe-put-v acc k v))
    e1
    e2))

(defn -merge-entity [dx m]
  (ex/-reduce
   (fn [acc [ref m]]
     (let [merged (-safe-merge-entity (ex/-get* dx ref {}) m)]
       (p/-put acc ref merged)))
   dx (-normalize m)))

(defn -merge-entities [dx xs]
  (ex/-reduce -merge-entity dx xs))

(defn -put-entity [dx m]
  (ex/-reduce (fn [acc [ref m]] (p/-put acc ref m)) dx (-normalize m)))

(defn -put-entities [dx xs]
  (ex/-reduce -put-entity dx xs))

(defn -safe-put-kv [dx [tid eid :as ref] k v]
  (if (ex/-get* dx ref)
    (p/-put dx ref (ex/-assoc* (ex/-get* dx ref) k v))
    (p/-put dx ref {tid eid k v})))

(defn -safe-put-kvs [dx ref & kvs]
  (ex/-reduce-kvs (fn [acc k v] (-safe-put-kv acc ref k v)) dx kvs))

(defn -safe-assoc
  ([m k v]
   (let [x (ex/-get* m k)]
     (if x
       (cond
         (-ref-lookups? x)
         (ex/-assoc* m k (conj x v))

         (and (not (-ref-lookup? x)) (vector? x))
         (ex/-assoc* m k (conj x v))

         :else
         (ex/-assoc* m k (into x v)))
       (ex/-assoc* m k v))))
  ([dx ref k v]
   (ex/-assoc* dx ref (-safe-assoc (ex/-get* dx ref) k v))))

(defn -safe-dissoc [dx ref k v]
  (let [m (ex/-get* dx ref)
        x (ex/-get* m k)]
    (if x
      (cond
        (-ref-lookups? x)
        (ex/-assoc* dx ref (ex/-assoc* m k (disj x v)))

        (and (not (-ref-lookup? x)) (vector? x))
        (ex/-assoc* dx ref (ex/-assoc* m k (ex/-remove (partial = v) x)))

        :else
        (ex/-assoc* dx ref (ex/-dissoc* m k)))
      dx)))

(defn -clearing-delete
  ([dx path] (-clearing-delete dx path ::dissoc))
  ([dx [ref k] v]
   (let [dissoc? (ex/-kw-identical? ::dissoc v)
         dx' (if dissoc? (ex/-dissoc-in dx [ref k]) (-safe-dissoc dx ref k v))]
     (if-some [x (if dissoc? (ex/-get* dx' ref) (ex/-get-in dx' [ref k]))]
       (if (map? x)
         ;; more than :db/id
         (if (> (count x) 1) dx' (if dissoc? (p/-del dx' ref) (ex/-dissoc-in dx' [ref k])))
         (if (> (count x) 0) dx' (if dissoc? (p/-del dx' ref) (ex/-dissoc-in dx' [ref k]))))
       dx'))))

(defn -delete-entity
  ([dx ref]
   (-> (ex/-reduce-kv
        (fn [acc k v]
          (if-let [rev (-rev->key k)]
            (cond
              (-ref-lookup? v)
              (-clearing-delete acc [v rev] ref)
              (-ref-lookups? v)
              (reduce (fn [acc x] (-clearing-delete acc [x rev] ref)) acc v))
            (cond
              (-ref-lookup? v)
              (-clearing-delete acc [v (-key->rev k)] ref)
              (-ref-lookups? v)
              (reduce (fn [acc x] (-clearing-delete acc [x (-key->rev k)] ref)) acc v)
              :else
              acc)))
        dx
        (ex/-get* dx ref))
       (p/-del ref))))

(deftype CachedResult [delay datoms])

(defn -update-index [index changes]
  (ex/-loop [^DoxaDBChange change changes :let [acc index]]
    (let [[tid _ :as ref] (.-e change)]
      (if (ex/-kw-identical? tid (.-a change))
        (case (.-kind change)
          :+ (recur (ex/-update acc tid ex/-conjs ref))
          :- (recur (ex/-update acc tid disj ref)))
        (recur acc)))
    acc))
