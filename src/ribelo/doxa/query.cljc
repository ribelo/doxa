(ns ribelo.doxa.query
  (:require
   [clojure.set :as set]
   [ribelo.exin :as ex]
   [ribelo.doxa.impl.protocols :as p]
   [ribelo.doxa :as dx]
   [ribelo.doxa.util :as u]
   [ribelo.doxa.pull :as dp]
   [ribelo.doxa.impl.map :as dxim])
  (:import
   (java.util HashMap)))

(set! *warn-on-reflection* true)

(defn -variable? [x]
  (and (symbol? x) (.startsWith (name x) "?")))

(defn -underscore? [x]
  (and (symbol? x) (.startsWith (name x) "_")))

(defn -src-variable? [x]
  (and (symbol? x) (.startsWith (name x) "$")))

(defn -plain-symbol? [x]
  (and (symbol? x) (not (-variable? x)) (not (-underscore? x)) (not (-src-variable? x))))

(defn -dot? [x]
  (= '. x))

(defn -dots? [x]
  (= '... x))

(defn -fn? [x]
  (and (list? x) (-plain-symbol? (ex/-first x))))

(defn -constant? [x]
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

(defn -resolve-variable [^HashMap acc x]
  (or (.get acc x) x))

(def -resolve-fn
  {'< <
   '> >
   '+ +})

(defn- -find-patern [xs]
  (cond
    (ex/-every? -variable? xs)
    :rel

    (and (-variable? (ex/-first xs)) (-dot? (ex/-second xs)))
    :rel-first

    (and (vector? (ex/-first xs)) (-variable? (ex/-ffirst xs)) (-dots? (-> xs ex/-first ex/-second)))
    :rel-coll

    (and (vector? (ex/-first xs)) (ex/-every? -variable? (ex/-first xs)))
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
    ([acc] acc)))

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
    (fn [acc]
      (persistent! (ex/-into! [] (ex/-get* acc k))))))

(defmethod -reducer :rel-tuple
  [xs]
  (fn [acc]
    (ex/-loop [s xs :let [acc (transient []) idx nil]]
      (let [set (ex/-get* acc s)]
        (if (u/-ref-lookup? (-> set ex/-iter .next))
          (let [m ()]))))))

(defmethod -reducer :pull
  [xs]
  (let [ref (-> xs ex/-first ex/-second)
        q   (-> xs ex/-first (nth 2))]
    (fn
      ([^HashMap stack acc dx]
       (conj! acc (dp/-pull dx q (ex/-get* stack ref))))
      ([acc] acc))))

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
    3 (ex/-mapv -patern datom)))

(defmulti -filterer (fn [x] (-parse-datom x)))

;; [?e :name]
(defmethod -filterer [:? :c nil]
  [[e a]]
  (fn [stack [ref m]]
    (if-let [oe (ex/-get* stack e)]
      (when (= oe ref)
        (when (ex/-get* m a)
          stack))
      (when (ex/-get* m a)
        (ex/-assoc stack e ref)))))

;; [?e :name "Ivan"]
(defmethod -filterer [:? :c :c]
  [[e a v]]
  (fn [stack [ref m]]
    (if-let [oe (ex/-get* stack e)]
      (when (= oe ref)
        (when (= v (ex/-get* m a))
          stack))
      (when (= v (ex/-get* m a))
        (ex/-assoc stack e ref)))))

;; [?e :name ?name]
(defmethod -filterer [:? :c :?]
  [[e a v]]
  (fn [stack [ref m]]
    (if-let [oe (ex/-get* stack e)]
      (when (= oe ref)
        (if-let [ov (ex/-get* stack v)]
          (when (= ov (ex/-get* m a))
            stack)
          (ex/-assoc stack v (ex/-get* m a))))
      (let [nv (ex/-get* m a)]
        (if-let [ov (ex/-get* stack v)]
          (when (= ov nv)
            (ex/-assoc stack e ref))
          (ex/-assoc stack e ref v nv))))))


;; [?e ?name "Ivan"]
(defmethod -filterer [:? :? :c]
  [[e a v]]
  (fn [stack [ref m]]
    (boolean
      (if-let [oe (ex/-get* stack e)]
        (when (= oe ref)
          (if-let [oa (ex/-get* stack a)]
            ((u/-search-attr-in-map m v) oa)
            (ex/-assoc stack a (ex/-get* m a))))
        (when-let [na (u/-search-attr-in-map m v)]
          (ex/-assoc stack e ref v na))))))

;; [(> ?age 15)]
(defmethod -filterer :filter
  [[[f & args]]]
  (if-let [f (-resolve-fn f)]
    (fn [stack [_ref _m]]
      (ex/-apply f (mapv (partial -resolve-variable stack) args)))
    (throw (ex-info "can't resolve function" {:f f}))))

;; [(+ ?a ?b) ?c]]
(defmethod -filterer :bind
  [[[f & args] var]]
  (if-let [f (-resolve-fn f)]
    (fn [stack [_ref _m]]
      (boolean (doto stack (.put var (ex/-apply f (mapv (partial -resolve-variable stack) args))))))
    (throw (ex-info "can't resolve function" {:f f}))))

(defn -create-acc [find]
  (case (-find-patern find)
    :rel (transient #{})
    :rel-first (transient [])
    :rel-coll (transient [])
    :pull (transient #{})))

(defn -group-datoms [datoms]
  (ex/-loop [d datoms :let [r (transient []) acc (transient []) e nil]]
    (let [pd (-parse-datom d)]
      (cond
        (ex/-kw-identical? :? (ex/-first pd))
        (let [de (ex/-first d)]
          (if (and e (not= e (ex/-first d)))
            (recur (conj! r (persistent! acc)) (transient [d]) de)
            (recur r (conj! acc d) de)))
        :else
        (recur r (conj! acc d) e)))
    (persistent! (conj! r (persistent! acc)))))

(defn -datoms-matcher
  ([datoms]
   (ex/-every-pred (ex/-mapv (fn [datom] (-filterer datom)) datoms))))

(def minidb (dx/create-dx (dxim/empty-db)
                          [{:db/id 2 :age 20 :salary 100}
                           {:db/id 3 :age 25 :salary 20}
                           {:db/id 4 :age 25 :salary 20}]))

(def miniddb (d/db-with (d/empty-db)
                        [{:db/id 2 :age 20 :salary 100}
                         {:db/id 3 :age 25 :salary 20}
                         {:db/id 4 :age 25 :salary 20}]))

(defn -process
  ([dx filterers reducer acc]
   (-process dx filterers reducer acc {} nil))
  ([dx filterers reducer acc stack]
   (-process dx filterers reducer acc stack nil))
  ([dx [filterer & more] reducer acc stack prev]
   (ex/-loop [me dx :let [acc acc stack (or prev stack)]]
     (if-let [stack (filterer stack me)]
       (if (seq more)
         (recur (-process dx more reducer acc {} stack) (or prev {}))
         (let [acc' (reducer stack acc dx)]
           (if (reduced? acc')
             @acc'
             (recur acc' (or prev {})))))
       (recur acc (or prev {})))
     (reducer acc))))

(def mm (ex/-mutable-map {:a 1}))
(vswap! mm (fn [m] (empty m)))
@mm
(empty (transient {:a 1}))

;; TODO
(defn -q [query dx]
  (let [pq (-query->map query)
        grouped-datoms (-group-datoms (ex/-get* pq :where))
        filterers (ex/-mapv -datoms-matcher grouped-datoms)
        reducer (-reducer (ex/-get* pq :find))
        acc (-create-acc (ex/-get* pq :find))
        result (-process dx filterers reducer acc)]
    (if (ex/-transient? result)
      (persistent! result)
      result)
    ))

(-q '[:find ?e2 ?age
      :where
      [?e1 :age ?age]
      [?e2 :salary ?age]]
    minidb)

(d/q '[:find ?e2 ?age
       :where
       [?e1 :age ?age]
       [?e2 :salary ?age]]
  miniddb)

(cc/quick-bench
  (let [s1 (u/-lookup db :name "Ivan")
        s2 (u/-lookup db :age 15)
        s3 (u/-lookup db :salary 500)]
    (ex/-intersection s1 s2 s3)))

(cc/quick-bench
  (-q '[:find ?e
        :where
        [?e :name "Ivan"]
        [?e :age 15]
        [?e :salary 500]]
      db))

(cc/quick-bench
  (d/q '[:find ?e
         :where
         [?e :name "Ivan"]
        [?e :age 15]
        [?e :salary 500]]
    ddb))


(def db (dx/create-dx (dxim/empty-db)
                      (into [{:db/id 1, :name "Ivan" , :age 15 :salary 500 :friend "Pixel"}
                             {:db/id 2, :name "Petr" , :age 37 }
                             {:db/id 3, :name "Ivan" , :age 37}]
                            (into [] (map (fn [i] {:db/id i :age (rand-int 80) :salary (rand-int 100)})) (range 4 1e3)))))


(require '[datascript.core :as d])
(def ddb (d/db-with (d/empty-db)
                    (into [{:db/id 1, :name "Ivan" , :age 15 :salary 500 :friend "Pixel"}
                           {:db/id 2, :name "Petr" , :age 37}
                           {:db/id 3, :name "Ivan" , :age 37}]
                          (into [] (map (fn [i] {:db/id i :age (rand-int 80) :salary (rand-int 100)})) (range 4 1e3)))))

