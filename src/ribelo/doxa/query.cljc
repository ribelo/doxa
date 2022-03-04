(ns ribelo.doxa.query
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.protocols :as p]
   [ribelo.doxa.util :as u]
   [ribelo.doxa.pull-api :as dxp])
  #?(:clj
     (:import
      (ribelo.doxa.util CachedResult))))

#?(:clj (set! *warn-on-reflection* true))

(def -resolve-fn
  {'= =, '== ==, 'not= not=, '!= not=, '< <, '> >, '<= <=, '>= >=, '+ +, '- -,
   '* *, '/ /, 'quot quot, 'rem rem, 'mod mod, 'inc inc, 'dec dec, 'max max, 'min min,
   'zero? zero?, 'pos? pos?, 'neg? neg?, 'even? even?, 'odd? odd?, 'compare compare,
   'rand rand, 'rand-int rand-int,
   'true? true?, 'false? false?, 'nil? nil?, 'some? some?, 'some ex/-some 'not not, ;; 'and and-fn, 'or or-fn,
   'complement complement, 'identical? identical?,
   'identity identity, 'keyword keyword, 'meta meta, 'name name, 'namespace namespace, 'type type,
   'vector vector, 'list list, 'set set, 'hash-map hash-map, 'array-map array-map,
   'count count, 'range range, 'not-empty not-empty, 'empty? empty?, 'contains? contains?,
   'str str, 'pr-str pr-str, 'print-str print-str, 'println-str println-str, 'prn-str prn-str, 'subs subs,
   're-find re-find, 're-matches re-matches, 're-seq re-seq, 're-pattern re-pattern,
   'ground identity,
   'tuple vector, 'untuple identity
   'intersect? ex/-intersect?})

(defmulti -query-fn (fn [x] x))

(defn- -find-patern [xs]
  (cond
    (ex/-every? u/-variable? xs)
    :rel

    (and (u/-variable? (ex/-first xs)) (u/-dot? (ex/-second xs)))
    :rel-first

    (and (vector? (ex/-first xs)) (u/-variable? (ex/-ffirst xs)) (u/-dots? (-> xs ex/-first ex/-second)))
    :rel-coll

    (and (vector? (ex/-first xs)) (ex/-every? u/-variable? (ex/-first xs)))
    :rel-tuple

    (and (list? (ex/-first xs)) (= 'pull (ex/-ffirst xs)))
    :pull

    :else (throw (ex-info "unrecognized input" {:xs xs}))))

(defmulti -reducer (fn [xs _opts] (-find-patern xs)))

(defmethod -reducer :rel
  [xs {:keys [limit]}]
  (fn
    ([stack acc _db]
     (let [acc' (conj! acc (ex/-mapv (fn [s] (ex/-get* stack s)) xs))]
       (if (and limit (= limit (count acc'))) (reduced acc') acc')))
    ([acc] (persistent! acc))))

(defmethod -reducer :rel-first
  [xs _opts]
  (let [k (ex/-first xs)]
    (fn
      ([stack _acc _db]
       (when-let [v (ex/-get* stack k)] (reduced v)))
      ([acc] acc))))

(defmethod -reducer :rel-coll
  [xs {:keys [limit]}]
  (let [k (ex/-ffirst xs)]
    (fn
    ([stack acc _db]
     (let [acc' (conj! acc (ex/-get* stack k))]
       (if (and limit (= limit (count acc'))) (reduced acc') acc')))
    ([acc] (persistent! acc)))))

(defmethod -reducer :rel-tuple
  [xs {:keys [limit]}]
  (fn
    ([stack acc _db]
     (let [acc' (conj! acc (ex/-mapv (fn [s] (ex/-get* stack s)) (ex/-first xs)))]
       (if (and limit (= limit (count acc'))) (reduced acc') acc')))
    ([acc] (persistent! acc))))

(defmethod -reducer :pull
  [xs {:keys [limit]}]
  (let [ref (-> xs ex/-first ex/-second)
        q   (-> xs ex/-first (nth 2))]
    (fn
      ([stack acc dx]
       (let [acc' (conj! acc (dxp/-pull dx q (ex/-get* stack ref)))]
         (if (and limit (= limit (count acc'))) (reduced acc') acc')))
      ([acc] (persistent! acc)))))

(defmethod -reducer :default
  [xs _opts]
  (ex-info "unrecognized patern" {:xs xs}))

(defn- -query->map [body]
  (ex/-loop [elem body :let [acc {} flag nil]]
    (if (keyword? elem)
      (recur acc elem)
      (recur (ex/-update acc flag ex/-conjv elem) flag))
    acc))

(defmulti -filterer (fn [x] (u/-parse-datom x)))

(defn -datoms-matcher
  ([datoms]
   (apply ex/-comp (reverse (ex/-mapv -filterer datoms)))))

;; [?e :name]
(defmethod -filterer [:? :c nil]
  [[e a]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (when (ex/-get* m a)
        [me (ex/-assoc stack e ref)]))))

;; [?e :name "Ivan"]
(defmethod -filterer [:? :c :c]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (when (= v (ex/-get* m a))
        [me (ex/-assoc stack e ref)]))))

;; [?e :name ?name]
(defmethod -filterer [:? :c :?]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [ov (ex/-get* stack v)]
        (when (= ov (ex/-get* m a))
          [me (ex/-assoc stack e ref)])
        (when-let [nv (ex/-get* m a)]
          [me (ex/-assoc stack e ref v nv)])))))

;; [?e ?a ?v]
(defmethod -filterer [:? :? :?]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (when-let [oa (ex/-get* stack a)]
        (if-let [ov (ex/-get* stack v)]
          (when (= ov (ex/-get* m oa))
            [me (ex/-assoc stack e ref)])
          [me (ex/-assoc stack v (ex/-get* m oa))])))))


;; [?e ?name "Ivan"]
(defmethod -filterer [:? :? :c]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [oa (ex/-get* stack a)]
        (when ((u/-search-attr-in-map m v) oa)
          [me (ex/-assoc stack e ref)])
        (when-let [na (not-empty (u/-search-attr-in-map m v))]
          [me (ex/-assoc stack e ref a (ex/-first na))])))))

(defmethod -filterer [:c :c :?]
  [[[tid eid] a v]]
  (fn [[[_ref m :as me] stack]]
    (when stack
      (when (= eid (ex/-get* m tid))
        (if-let [ov (ex/-get* stack v)]
          (when (= ov (ex/-get* m a))
            [me stack])
          [me (ex/-assoc stack v (ex/-get* m a))])))))

;; [(> ?age 15)]
(defmethod -filterer :filter
  [[[f & args]]]
  (if-let [f (or (-resolve-fn f) (-query-fn f))]
    (fn [[[_ref _m :as me] stack]]
      (when stack
        (when (apply f (mapv (fn [x] (if (seqable? x)
                                          (into (empty x) (map (fn [y] (ex/-get* stack y y))) x)
                                          (ex/-get* stack x x))) args))
          [me stack])))
    (throw (ex-info "can't resolve function" {:f f}))))

;; [(+ ?a ?b) ?c]]
(defmethod -filterer :bind
  [[[f & args] var]]
  (if-let [f (or (-resolve-fn f) (-query-fn f))]
    (fn [[[_ref _m :as me] stack]]
      (when stack
        (let [v (apply f (mapv (fn [x] (ex/-get* stack x x)) args))]
          [me (assoc stack var v)])))
    (throw (ex-info "can't resolve function" {:f f}))))

(defmethod -filterer :and
  [[_ & datoms]]
  (let [f (-datoms-matcher datoms)]
    (fn [[me stack]]
      (when stack
        (f [me stack])))))

(defmethod -filterer :or
  [[_ x y]]
  (let [fx (-filterer x)
        fy (-filterer y)]
    (fn [[me stack]]
      (when stack
        (or (fx [me stack]) (fy [me stack]))))))

(defn -create-acc [find]
  (case (-find-patern find)
    :rel (transient #{})
    :rel-first (transient [])
    :rel-coll (transient [])
    :rel-tuple (transient [])
    :pull (transient #{})))

(defn -group-datoms [datoms]
  (ex/-loop [d datoms :let [r (transient []) acc (transient []) e nil]]
    (let [pd (u/-parse-datom d)]
      (cond
        (and (vector? pd) (or (ex/-kw-identical? :? (ex/-first pd)) (ex/-kw-identical? :c (ex/-first pd))))
        (let [de (ex/-first d)]
          (if (and e (not= e (ex/-first d)))
            (recur (conj! r (persistent! acc)) (transient [d]) de)
            (recur r (conj! acc d) de)))

        (ex/-kw-identical? :or pd)
        (let [xs (-group-datoms (next d))]
          (case (count xs)
            1 (recur r (conj! acc d) e)
            (recur (ex/-conj-some! r (ex/-not-empty (persistent! acc))) (transient [d]) e)))

        (ex/-kw-identical? :and pd)
        (let [xs (-group-datoms (next d))]
          (case (count xs)
            1 (recur r (conj! acc d) e)
            (recur (ex/-conj-some! r (ex/-not-empty (persistent! acc))) (transient [d]) e)))

        :else
        (recur r (conj! acc d) e)))
    (persistent! (ex/-conj-some! r (ex/-not-empty (persistent! acc))))))

(-group-datoms '[(and [?e :age ?a]
                      [?e :name "Ivan"]
                      [[:db/id 1] :age ?a])])

;; TODO cartesian product
(defn -parse-input
  ([input args]
   (assert (= (count input) (count args)))
   (-parse-input input args {}))
  ([[in & more :as input] [x & args] acc]
   (if in
     (cond
       (and (u/-variable? in) (or (u/-ref-lookup? x) (not (vector? x))))
       (recur more args (assoc acc in x))
       (and (u/-variable? in) (vector? x))
       (ex/-mapv (fn [x] (-parse-input input (conj args x) acc)) x)
       (and (vector? in) (not (vector? (ex/-first in)))
            (vector? x) (not (vector? (ex/-first x)))
            (= (count in) (count x)))
       (zipmap in x)
       (and (vector? in) (vector? x) )
       (ex/-mapv (fn [x] (-parse-input in [x] acc)) x)
       )
     acc)))

(defn -process-loop [dx [filterer & more] reducer acc stack prev]
  (ex/-loop [me dx :let [acc acc stack (or prev stack)]]
    (if-let [[_ stack] (filterer [me stack])]
      (if (seq more)
        (recur (-process-loop dx more reducer acc {} stack) (or prev stack))
        (let [acc' (reducer stack acc dx)]
          (if (reduced? acc')
            @acc'
            (recur acc' (or prev {})))))
      (recur acc (or prev {})))
    acc))

(defn -process
  ([dx filterers reducer input acc]
   (reducer (-process dx filterers reducer input acc {})))
  ([dx filterers reducer inputs acc stack]
   (if (map? inputs)
     (-process-loop dx filterers reducer acc stack inputs)
     (ex/-loop [in inputs :let [acc acc]]
       (recur (-process-loop dx filterers reducer acc stack in))
       acc))))

(def -querer
  (ex/-memoize
    (fn [query]
      (let [pq (-query->map query)
            grouped-datoms (-group-datoms (ex/-get* pq :where))
            filterers (ex/-mapv -datoms-matcher grouped-datoms)
            reducer (-reducer (ex/-get* pq :find)
                              {:limit (some-> (ex/-get* pq :limit) ex/-first)})]
        (fn [db args]
          (let [inputs (-parse-input (ex/-get* pq :in) args)
                acc (-create-acc (ex/-get* pq :find))]
            (-process db filterers reducer inputs acc)))))))

(defn -q [query db & args]
  (let [querer (-querer query)]
    (some-> db (querer args))))

(defn -mq [query db & args]
  (let [k (conj args query)
        cache_ (p/-cache db)
        where (ex/-get* (-query->map query) :where)]
    (if (p/-has? @cache_ k)
      (let [cache' (p/-hit @cache_ k)
            item (ex/-get* cache' k)]
        (p/-set-cache! db cache')
        @(.-delay ^CachedResult item))
      (let [d (delay (apply -q (conj args db query)))
            item #?(:clj  (CachedResult. d where) :cljs (u/CachedResult. d where))
            cache' (p/-miss @cache_ k item)]
        (p/-set-cache! db cache')
        @d))))
