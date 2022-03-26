(ns ribelo.doxa.query
  (:require
   [clojure.set :as set]
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
   'true? true?, 'false? false?, 'nil? nil?, 'some? some?, 'some ex/some 'not not, ;; 'and and-fn, 'or or-fn,
   'complement complement, 'identical? identical?,
   'identity identity, 'keyword keyword, 'meta meta, 'name name, 'namespace namespace, 'type type,
   'vector vector, 'list list, 'set set, 'hash-map hash-map, 'array-map array-map,
   'count count, 'range range, 'not-empty not-empty, 'empty? empty?, 'contains? contains?,
   'str str, 'pr-str pr-str, 'print-str print-str, 'println-str println-str, 'prn-str prn-str, 'subs subs,
   're-find re-find, 're-matches re-matches, 're-seq re-seq, 're-pattern re-pattern,
   'ground identity,
   'tuple vector, 'untuple identity
   'intersect? ex/intersect?})

(defmulti -query-fn (fn [x] x))

(defn- -find-pattern [xs]
  (cond
    (ex/every? u/-variable? xs)
    :rel

    (and (u/-variable? (first xs)) (u/-dot? (second xs)))
    :rel-first

    (and (vector? (first xs)) (u/-variable? (ffirst xs)) (u/-dots? (-> xs first second)))
    :rel-coll

    (and (vector? (first xs)) (ex/every? u/-variable? (first xs)))
    :rel-tuple

    (and (list? (first xs)) (= 'pull (ffirst xs)))
    :pull

    :else (throw (ex-info "unrecognized input" {:xs xs}))))

(defmulti -reducer (fn [xs _opts] (-find-pattern xs)))

(defmethod -reducer :rel
  [xs {:keys [limit]}]
  (fn
    ([stack acc _db]
     (let [acc' (conj! acc (mapv (fn [s] (stack s)) xs))]
       (if (and limit (= limit (count acc'))) (reduced acc') acc')))
    ([acc] (persistent! acc))))

(defmethod -reducer :rel-first
  [xs _opts]
  (let [k (first xs)]
    (fn
      ([stack _acc _db]
       (when-let [v (stack k)] (reduced v)))
      ([acc] acc))))

(defmethod -reducer :rel-coll
  [xs {:keys [limit]}]
  (let [k (ffirst xs)]
    (fn
    ([stack acc _db]
     (let [acc' (conj! acc (stack k))]
       (if (and limit (= limit (count acc'))) (reduced acc') acc')))
    ([acc] (persistent! acc)))))

(defmethod -reducer :rel-tuple
  [xs {:keys [limit]}]
  (fn
    ([stack acc _db]
     (let [acc' (conj! acc (mapv (fn [s] (stack s)) (first xs)))]
       (if (and limit (= limit (count acc'))) (reduced acc') acc')))
    ([acc] (persistent! acc))))

(defmethod -reducer :pull
  [xs {:keys [limit]}]
  (let [ref (-> xs first second)
        q   (-> xs first (nth 2))]
    (fn
      ([stack acc dx]
       (let [acc' (conj! acc (dxp/-pull dx q (stack ref)))]
         (if (and limit (= limit (count acc'))) (reduced acc') acc')))
      ([acc] (persistent! acc)))))

(defmethod -reducer :default
  [xs _opts]
  (ex-info "unrecognized pattern" {:xs xs}))

(defn- -query->map [body]
  (ex/loop-it [elem body :let [acc {} flag nil]]
    (if (keyword? elem)
      (recur acc elem)
      (recur (update acc flag ex/conjv elem) flag))
    acc))

(defmulti -filterer (fn [x] (u/-parse-datom x)))

(defn -datoms-matcher
  ([datoms]
   (apply ex/comp (reverse (mapv -filterer datoms)))))

;; [?e :name]
(defmethod -filterer [:? :c nil]
  [[e a]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (when (get m a)
        [me (assoc stack e ref)]))))

;; [?e :name "Ivan"]
(defmethod -filterer [:? :c :c]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (when (= v (get m a))
        [me (assoc stack e ref)]))))

;; [?e :name ?name]
(defmethod -filterer [:? :c :?]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [ov (stack v)]
        (when (= ov (get m a))
          [me (assoc stack e ref)])
        (when-let [nv (get m a)]
          [me (assoc stack e ref v nv)])))))

;; [?e ?a ?v]
(defmethod -filterer [:? :? :?]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (when-let [oa (stack a)]
        (if-let [ov (stack v)]
          (when (= ov (get m oa))
            [me (assoc stack e ref)])
          [me (assoc stack v (get m oa))])))))


;; [?e ?name "Ivan"]
(defmethod -filterer [:? :? :c]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [oa (stack a)]
        (when (get (u/-search-attr-in-map m v) oa)
          [me (assoc stack e ref)])
        (when-let [na (not-empty (u/-search-attr-in-map m v))]
          [me (assoc stack e ref a (first na))])))))

(defmethod -filterer [:c :c :?]
  [[[tid eid] a v]]
  (fn [[[_ref m :as me] stack]]
    (when stack
      (when (= eid (m tid))
        (if-let [ov (stack v)]
          (when (= ov (m a))
            [me stack])
          [me (assoc stack v (m a))])))))

(defn -apply [f xs]
  (case (count xs)
    0
    (f)
    1
    (f (nth xs 1))
    2
    (f (nth xs 0) (nth xs 1))
    3
    (f (nth xs 0) (nth xs 1) (nth xs 2))
    4
    (f (nth xs 0) (nth xs 1) (nth xs 2) (nth xs 3))
    (apply f xs)))

;; [(> ?age 15)]
(defmethod -filterer :filter
  [[[f & args]]]
  (if-let [f (or (-resolve-fn f) (-query-fn f))]
    (fn [[me stack]]
      (when stack
        (when (-apply f (mapv (fn [k] (if (u/-variable? k) (stack k) k)) args))
          [me stack])))
    (throw (ex-info "can't resolve function" {:f f}))))

;; [(+ ?a ?b) ?c]]
(defmethod -filterer :bind
  [[[f & args] var]]
  (if-let [f (or (-resolve-fn f) (-query-fn f))]
    (fn [[[_ref _m :as me] stack]]
      (when stack
        (let [v (apply f (mapv (fn [x] (stack x x)) args))]
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
  (case (-find-pattern find)
    :rel (transient #{})
    :rel-first (transient [])
    :rel-coll (transient [])
    :rel-tuple (transient [])
    :pull (transient #{})))

(defn -group-datoms [datoms]
  (ex/loop-it [d datoms :let [r (transient []) acc (transient []) e nil]]
    (let [pd (u/-parse-datom d)]
      (cond
        (and (vector? pd) (or (ex/kw-identical? :? (first pd)) (ex/kw-identical? :c (first pd))))
        (let [de (first d)]
          (if (and e (not= e (first d)))
            (recur (conj! r (persistent! acc)) (transient [d]) de)
            (recur r (conj! acc d) de)))

        (ex/kw-identical? :or pd)
        (let [xs (-group-datoms (next d))]
          (case (count xs)
            1 (recur r (conj! acc d) e)
            (recur (ex/conj-some! r (not-empty (persistent! acc))) (transient [d]) e)))

        (ex/kw-identical? :and pd)
        (let [xs (-group-datoms (next d))]
          (case (count xs)
            1 (recur r (conj! acc d) e)
            (recur (ex/conj-some! r (not-empty (persistent! acc))) (transient [d]) e)))

        :else
        (recur r (conj! acc d) e)))
    (persistent! (ex/conj-some! r (not-empty (persistent! acc))))))

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
       (mapv (fn [x] (-parse-input input (conj args x) acc)) x)
       (and (vector? in) (not (vector? (first in)))
            (vector? x) (not (vector? (first x)))
            (= (count in) (count x)))
       (zipmap in x)
       (and (vector? in) (vector? x) )
       (mapv (fn [x] (-parse-input in [x] acc)) x)
       )
     acc)))

(defn -process-loop [dx [filterer & more] reducer acc stack prev]
  (ex/loop-it [me dx :let [acc acc stack (or prev stack)]]
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
     (ex/loop-it [in inputs :let [acc acc]]
       (recur (-process-loop dx filterers reducer acc stack in))
       acc))))

(def -querer
  (ex/memoize
    (fn [query]
      (let [pq (-query->map query)
            grouped-datoms (-group-datoms (get pq :where))
            filterers (mapv -datoms-matcher grouped-datoms)
            reducer (-reducer (get pq :find)
                              {:limit (some-> (get pq :limit) first)})]
        (fn [db args]
          (let [inputs (-parse-input (get pq :in) args)
                acc (-create-acc (get pq :find))]
            (-process db filterers reducer inputs acc)))))))

(defn -q [query db & args]
  (let [querer (-querer query)]
    (some-> db (querer args))))

(defn -mq [query db & args]
  (let [k (conj args query)
        cache_ (p/-cache db)
        where (get (-query->map query) :where)]
    (if (p/-has? @cache_ k)
      (let [cache' (p/-hit @cache_ k)
            item (get cache' k)]
        (p/-set-cache! db cache')
        @(.-delay ^CachedResult item))
      (let [d (delay (apply -q (conj args db query)))
            item #?(:clj  (CachedResult. d where) :cljs (u/CachedResult. d where))
            cache' (p/-miss @cache_ k item)]
        (p/-set-cache! db cache')
        @d))))
