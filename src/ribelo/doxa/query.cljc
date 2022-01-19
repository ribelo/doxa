(ns ribelo.doxa.query
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa.impl.protocols :as p]
   [ribelo.doxa :as dx]
   [ribelo.doxa.util :as u]
   [ribelo.doxa.pull :as dp])
  (:import
   (java.util HashMap)))

(set! *warn-on-reflection* true)

(defn -parse-double [[x y]]
  (if (list? x)
    :bind
    [(u/-patern x) (u/-patern y) nil]))

(def -resolve-fn
  {'< <
   '> >
   '+ +})

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

(defmulti -reducer (fn [xs] (-find-patern xs)))

(defmethod -reducer :rel
  [xs]
  (fn
    ([stack acc _db]
     (conj! acc (ex/-mapv (fn [s] (ex/-get* stack s)) xs)))
    ([acc] (persistent! acc))))

(defmethod -reducer :rel-first
  [xs]
  (let [k (ex/-first xs)]
    (fn
      ([stack _acc _db]
       (when-let [v (ex/-get* stack k)] (reduced v)))
      ([acc] acc))))

(defmethod -reducer :rel-coll
  [xs]
  (let [k (ex/-ffirst xs)]
    (fn
    ([stack acc _db]
     (conj! acc (ex/-get* stack k)))
    ([acc] (persistent! acc)))))

(defmethod -reducer :rel-tuple
  [xs]
  (fn
    ([stack acc _db]
     (conj! acc (ex/-mapv (fn [s] (ex/-get* stack s)) (ex/-first xs))))
    ([acc] (persistent! acc))))

(defmethod -reducer :pull
  [xs]
  (let [ref (-> xs ex/-first ex/-second)
        q   (-> xs ex/-first (nth 2))]
    (fn
      ([^HashMap stack acc dx]
       (conj! acc (dp/-pull dx q (ex/-get* stack ref))))
      ([acc] (persistent! acc)))))

(defmethod -reducer :default
  [xs]
  (ex-info "unrecognized patern" {:xs xs}))

(defn- -query->map [body]
  (ex/-loop [elem body :let [acc {} flag nil]]
    (if (keyword? elem)
      (recur acc elem)
      (recur (ex/-update acc flag ex/-conjv elem) flag))
    acc))

(defn -parse-datom [datom]
  (case (count datom)
    1 :filter
    2 (-parse-double datom)
    3 (ex/-mapv u/-patern datom)))

(defmulti -filterer (fn [x] (-parse-datom x)))

;; [?e :name]
(defmethod -filterer [:? :c nil]
  [[e a]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [oe (ex/-get* stack e)]
        (when (= oe ref)
          (when (ex/-get* m a)
            [me stack]))
        (when (ex/-get* m a)
          [me (ex/-assoc stack e ref)])))))

;; [?e :name "Ivan"]
(defmethod -filterer [:? :c :c]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [oe (ex/-get* stack e)]
        (when (= oe ref)
          (when (= v (ex/-get* m a))
            [me stack]))
        (when (= v (ex/-get* m a))
          [me (ex/-assoc stack e ref)])))))

;; [?e :name ?name]
(defmethod -filterer [:? :c :?]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [oe (ex/-get* stack e)]
        (when (= oe ref)
          (if-let [ov (ex/-get* stack v)]
            (when (= ov (ex/-get* m a))
              [me stack])
            [me (ex/-assoc stack v (ex/-get* m a))]))
        (let [nv (ex/-get* m a)]
          (if-let [ov (ex/-get* stack v)]
            (when (= ov nv)
              [me (ex/-assoc stack e ref)])
            [me (ex/-assoc stack e ref v nv)]))))))


;; [?e ?name "Ivan"]
(defmethod -filterer [:? :? :c]
  [[e a v]]
  (fn [[[ref m :as me] stack]]
    (when stack
      (if-let [oe (ex/-get* stack e)]
        (when (= oe ref)
          (if-let [oa (ex/-get* stack a)]
            ((u/-search-attr-in-map m v) oa)
            [me (ex/-assoc stack a (ex/-get* m a))]))
        (when-let [na (u/-search-attr-in-map m v)]
          [me (ex/-assoc stack e ref v na)])))))

;; [(> ?age 15)]
(defmethod -filterer :filter
  [[[f & args]]]
  (if-let [f (-resolve-fn f)]
    (fn [[[_ref _m :as me] stack]]
      (when stack
        (when (ex/-apply f (mapv (fn [x] (ex/-get* stack x x)) args))
          [me stack])))
    (throw (ex-info "can't resolve function" {:f f}))))

;; [(+ ?a ?b) ?c]]
(defmethod -filterer :bind
  [[[f & args] var]]
  (if-let [f (-resolve-fn f)]
    (fn [[[_ref _m :as me] stack]]
      (when stack
        (let [v (ex/-apply f (mapv (fn [x] (ex/-get* stack x x)) args))]
          [me (assoc stack var v)])))
    (throw (ex-info "can't resolve function" {:f f}))))

(defn -create-acc [find]
  (case (-find-patern find)
    :rel (transient #{})
    :rel-first (transient [])
    :rel-coll (transient [])
    :rel-tuple (transient [])
    :pull (transient #{})))

(defn -group-datoms [datoms]
  (ex/-loop [d datoms :let [r (transient []) acc (transient []) e nil]]
    (let [pd (-parse-datom d)]
      (cond
        (and (vector? pd) (ex/-kw-identical? :? (ex/-first pd)))
        (let [de (ex/-first d)]
          (if (and e (not= e (ex/-first d)))
            (recur (conj! r (persistent! acc)) (transient [d]) de)
            (recur r (conj! acc d) de)))
        :else
        (recur r (conj! acc d) e)))
    (persistent! (conj! r (persistent! acc)))))

;; TODO cartesian product
(defn -parse-input
  ([input args]
   (assert (= (count input) (count args)))
   (-parse-input input args {}))
  ([[in & more :as input] [x & args] acc]
   (if in
     (cond
       (and (u/-variable? in) (not (vector? x)))
       (recur more args (assoc acc in x))
       (and (u/-variable? in) (vector? x))
       (ex/-mapv (fn [x] (-parse-input input (conj args x) acc)) x)
       (and (vector? in) (vector? x))
       (ex/-mapv (fn [x] (-parse-input in [x] acc)) x))
     acc)))

(defn -datoms-matcher
  ([datoms]
   (ex/-apply ex/-comp (reverse (ex/-mapv -filterer datoms)))))

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
            reducer (-reducer (ex/-get* pq :find))]
        (fn [db args]
          (let [inputs (-parse-input (ex/-get* pq :in) args)
                acc (-create-acc (ex/-get* pq :find))]
            (-process db filterers reducer inputs acc)))))))

(defn -q [query db & args]
  (let [querer (-querer query)]
    (querer db args)))
