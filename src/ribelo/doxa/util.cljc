(ns ribelo.doxa.util
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa.impl.protocols :as p]))

(comment (require '[taoensso.encore :as enc])
         (require '[criterium.core :as cc]))

(defn -key-id? [k]
  (when (or (keyword? k) (string? k)) (.endsWith (name k) "id")))

(defn -ref-lookup? [xs]
  (and (vector? xs) (= 2 (count xs))
       (-key-id? (nth xs 0))))

(defn -probably-ref-lookups? [xs]
  (and (vector? xs) (-ref-lookup? (ex/-first xs))))

(defn -entity? [m]
  (and (map? m) (ex/-reduce-kv (fn [_ k _] (if (-key-id? k) (reduced true) false)) false m)))

(defn -entities? [xs]
  (and (vector? xs) (ex/-every? -entity? xs)))

(defn -dx? [dx]
  (satisfies? p/IDoxa dx))

(defn -probably-dx? [dx]
  (or (-dx? dx) (and (map? dx) (-ref-lookup? (ex/-first-key dx)))))

(defn -entity-ref [m]
  (when (map? m)
    (ex/-reduce-kv
      (fn [_ k v]
        (when (-key-id? k)
          (reduced [k v])))
      nil
      m)))

(defn -entities-refs [xs]
  (when (vector? xs)
    (ex/-not-empty (ex/-into #{} (keep -entity-ref) xs))))

(defn -key->rev [k]
  (keyword (namespace k) (str "_" (name k))))

(defn -rev->key [k]
  (when (keyword? k)
    (let [s (name k)]
      (when (zero? (.indexOf s "_"))
        (keyword (namespace k) (.substring s 1))))))

(defn -flatten-map [m]
  (persistent!
    (ex/-reduce-kv
      (fn [acc k v]
        (if (and (-probably-ref-lookups? v) (= 1 (count v)))
          (assoc! acc k (nth v 0))
          (assoc! acc k v)))
      (transient {})
      m)))

(deftype DoxaDBChange [e kind a v udt])

(defn -diff-entity
  ([e1 e2] (-diff-entity e1 e2 (ex/-udt)))
  ([e1 e2 udt]
   (let [ref1 (-entity-ref e1)
         ref2 (-entity-ref e2)]
     (if (or (= ref1 ref2) (nil? ref1) (nil? ref2))
       (let [ref (or ref1 ref2)
             acc (ex/-reduce-kv
                  (fn [acc k v1]
                    (if (ex/-get e2 k)
                      acc
                      (conj! acc (DoxaDBChange. ref1 :- k v1 udt))))
                  (transient [])
                  e1)]
         (persistent!
          (ex/-reduce-kv
           (fn [acc k v2]
             (if-let [v1 (ex/-get e1 k)]
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
    3 (ex/-mapv -patern datom)))

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
                 (seqable? v) (ex/-some #{x} v)))
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

(defn -normalize
  "turns a nested map into a flat collection with references."
  ([data] (-normalize data {}))
  ([data m]
   (let [it (ex/-iter data)]
     (persistent!
       (loop [m (transient m) r (transient []) id nil]
         (cond
           (and (not (.hasNext it)) (nil? id))
           nil

           (and (not (.hasNext it)) id)
           (conj! r [id (persistent! m)])

           :else
           (let [me (.next it)
                 k (ex/-k me)
                 v (ex/-v me)]

             (if (-key-id? k)
               (recur (assoc! m k v) r [k v])

               (if-let [eid (-entity-ref v)]
                 (recur (assoc! m k eid) (reduce conj! r (-normalize v {(-key->rev k) id})) id)

                 (if-let [eids (-entities-refs v)]
                   (recur (assoc! m k eids) (reduce (fn [acc m'] (reduce conj! acc (-normalize m' {(-key->rev k) id}))) r v) id)

                   (cond
                     (-ref-lookup? v)
                     (recur (assoc! m k v) r id)

                     :else
                     (recur (assoc! m k v) r id))))))))))))

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
             (recur (assoc! m k (-denormalize dx v max-level (inc level))))

             (-ref-lookup? v)
             (recur (assoc! m k (or (get-in m v) (-denormalize dx (p/-pick dx v) max-level (inc level)))))

             (-probably-ref-lookups? v)
             (recur (assoc! m k (mapv (fn [ident] (or (get-in m ident) (-denormalize dx (p/-pick dx ident) max-level (inc level)))) v)))

             :else
             (recur (assoc! m k v)))))))))

(defn -merge-entity [dx m]
  (ex/-reduce (fn [acc [ref m]] (p/-put acc ref (ex/-merge (ex/-get acc ref) m))) dx (-normalize m)))

(defn -merge-entities [dx xs]
  (ex/-reduce -merge-entity dx xs))

(defn -put-entity [dx m]
  (ex/-reduce (fn [acc [ref m]] (p/-put acc ref m)) dx (-normalize m)))

(defn -put-entities [dx xs]
  (ex/-reduce -put-entity dx xs))

(defn -safe-put-kv [dx [tid eid :as ref] k v]
  (if (p/-pick dx ref)
    (p/-put dx ref k v)
    (p/-put dx ref {tid eid k v})))

(defn -safe-put-kvs [dx ref & kvs]
  (ex/-reduce-kvs (fn [acc k v] (-safe-put-kv acc ref k v)) dx kvs))

(defn -clearing-delete
  ([dx path] (-clearing-delete dx path ::dissoc))
  ([dx [ref k] v]
   (let [dissoc? (ex/-kw-identical? ::dissoc v)
         dx' (if dissoc? (p/-del dx ref k) (p/-del dx ref k v))]
     (if-let [x (if dissoc? (p/-pick dx' ref) (p/-pick dx' ref k))]
       (if (map? x)
         (if (> (count x) 1) dx' (if dissoc? (p/-del dx' ref) (p/-del dx' ref k)))
         (if (> (count x) 0) dx' (if dissoc? (p/-del dx' ref) (p/-del dx' ref k))))
       dx'))))

(defn -delete-entity
  ([dx ref]
   (-> (ex/-reduce-kv
         (fn [acc k v]
           (if-let [k (-rev->key k)]
             (cond
               (-ref-lookup? v)
               (-clearing-delete acc [v k] ref)
               (-probably-ref-lookups? v)
               (reduce (fn [acc x] (-clearing-delete acc [x k] ref)) acc v))
             acc))
         dx
         (p/-pick dx ref))
       (p/-del ref))))

(defn -lookup-attr [dx k]
  (ex/-get-in (p/-index dx) [:a k] #{}))

(defn -lookup-val [dx k]
  (ex/-get-in (p/-index dx) [:v k] #{}))

(defn -lookup-for
  ([dx k v] (-lookup-for dx k v :ref))
  ([dx k v kind]
   (let [attr-refs (ex/-get-in (p/-index dx) [:a k] #{})
         val-refs (ex/-get-in (p/-index dx) [:v v] #{})
         intersection (ex/-intersection attr-refs val-refs)]
     (ex/-loop [x intersection :let [acc (transient #{})]]
       (let [m (p/-pick dx x)]
         (if (= v (ex/-get* m k))
           (recur (conj! acc (case kind :ref [x m] :entity m)))
           (recur acc)))
       (persistent! acc)))))

(defn -lookup-pred
  ([dx k v] (-lookup-pred dx k v :ref))
  ([dx k pred kind]
   (let [attr-refs (ex/-get-in (p/-index dx) [:a k] #{})]
     (ex/-loop [x attr-refs :let [acc (transient #{})]]
       (let [m (p/-pick dx x)]
         (if (pred (ex/-get* m k))
           (recur (conj! acc (case kind :ref x :entity m)))
           (recur acc)))
       (persistent! acc)))))

(defn -lookup [dx k v]
  (if (or (fn? v) (set? v))
    (-lookup-pred dx k v)
    (-lookup-for dx k v)))

(defn -vals [xs k]
  (ex/-loop [m xs :let [acc (transient #{})]]
    (if-let [v (ex/-get* m k)]
      (recur (conj! acc v))
      (recur acc))
    (persistent! acc)))
