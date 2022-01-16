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
  (let [k (when (= 1 (count xs)) (ex/-first xs))]
    (fn [acc]
      (if k
        (persistent! (ex/-into! #{} (map vector) (ex/-get* acc k)))
        (ex/-cartesian-product #{} (ex/-mapv (fn [k] (ex/-get* acc k)) xs))))))

(defmethod -reducer :rel-first
  [xs]
  (let [k (ex/-first xs)]
    (fn [acc]
      (-> (ex/-get* acc k) ex/-iter .next))))

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
      ([^HashMap stack* acc dx]
       (conj! acc (dp/-pull dx q (.get stack* ref))))
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
  (fn [db acc]
    (let [refs (u/-lookup-attr db a)]
      (if (ex/-get* acc e)
        (ex/-update! acc e ex/-intersection refs)
        (ex/-assoc! acc e refs)))))

;; [?e :name "Ivan"]
(defmethod -filterer [:? :c :c]
  [[e a v]]
  (fn [db acc]
    (let [refs (u/-lookup-for db a v)]
      (if (ex/-get* acc e)
        (ex/-update! acc e ex/-intersection refs)
        (ex/-assoc! acc e refs)))))

;; [?e :name ?name]
(defmethod -filterer [:? :c :?]
  [[e a v]]
  (fn [db acc]
    (let [rs (u/-lookup-attr db a)]
      (if-let [oe (ex/-get* acc e)]
        (let [ie (ex/-intersection oe rs)
              vs (persistent!
                   (ex/-reduce
                     (fn [acc' ref]
                       (conj! acc' (p/-pick db ref a)))
                     (transient #{})
                     ie))]
          (if-let [ov (ex/-get* acc v)]
            (let [iv (ex/-intersection ov vs)
                  rv (ex/-reduce
                       (fn [acc' v] (ex/-union acc' (u/-lookup-for db a v)))
                       #{}
                       iv)
                  ie (ex/-intersection ie rv)]
              (ex/-assoc! acc e ie v iv))
            (ex/-assoc! acc e ie v vs)))
        (let [vs (persistent!
                   (ex/-reduce
                     (fn [acc' ref]
                       (conj! acc' (p/-pick db ref a)))
                     (transient #{})
                     rs))]
          (if-let [ov (ex/-get* acc v)]
            (let [iv (ex/-intersection ov vs)
                  rv (ex/-reduce
                       (fn [acc' v] (ex/-union acc' (u/-lookup-for db a v)))
                       #{}
                       iv)
                  ie (ex/-intersection rs rv)]
              (ex/-assoc! acc e ie v iv))
            (ex/-assoc! acc e rs v vs)))))))


;; [?e ?name "Ivan"]
(defmethod -filterer [:? :? :c]
  [[e a v]]
  (fn [db acc]
    (let [rs (u/-lookup-val db v)]
      (if-let [oe (ex/-get* acc e)]
        (let [ie (ex/-intersection oe rs)
              as (ex/-reduce
                   (fn [acc ref]
                     (ex/-union acc (u/-search-attr-in-map (p/-pick db ref) v)))
                   #{}
                   ie)]
          (if-let [oa (ex/-get* acc a)]
            (let [ia (ex/-intersection oa as)
                  ra (ex/-reduce
                       (fn [acc a] (ex/-union acc (u/-lookup-for db a v)))
                       #{}
                       ia)
                  ie (ex/-intersection ie ra)]
              (ex/-assoc! acc e ie a ia))
            (ex/-assoc! acc e ie a as)))
        (let [as (ex/-reduce
                   (fn [acc ref]
                     (ex/-union acc (u/-search-attr-in-map (p/-pick db ref) v)))
                   #{}
                   rs)]
          (if-let [oa (ex/-get* acc a)]
            (let [ia (ex/-intersection oa as)
                  rv (ex/-reduce
                       (fn [acc v] (ex/-union acc (u/-lookup-for db a v)))
                       #{}
                       ia)
                  ie (ex/-intersection rs rv)]
              (ex/-assoc! acc e ie a ia))
            (ex/-assoc! acc e rs a as)))))))

;; [(> ?age 15)]
(defmethod -filterer :filter
  [[[f & args]]]
  (if-let [f (-resolve-fn f)]
    (fn [^HashMap stack* [_ref _m]]
      (ex/-apply f (mapv (partial -resolve-variable stack*) args)))
    (throw (ex-info "can't resolve function" {:f f}))))

;; [(+ ?a ?b) ?c]]
(defmethod -filterer :bind
  [[[f & args] var]]
  (if-let [f (-resolve-fn f)]
    (fn [^HashMap stack* [_ref _m]]
      (boolean (doto stack* (.put var (ex/-apply f (mapv (partial -resolve-variable stack*) args))))))
    (throw (ex-info "can't resolve function" {:f f}))))

(defn -datoms-intersection
  ([datoms]
   (fn [db acc]
     ((ex/-reduce
        (fn [acc datom]
          (comp (partial (-filterer datom) db) acc))
        (comp)
        datoms)
      acc))))

(def minidb (dx/create-dx (dxim/empty-db)
                          [{:db/id 1 :age 15 :salary 50}
                           {:db/id 2 :age 20 :salary 100}
                           {:db/id 3 :salary 200}]))

(def miniddb (d/db-with (d/empty-db)
                        [{:db/id 1 :age 15 :salary 50}
                         {:db/id 2 :age 20 :salary 100}
                         {:db/id 3 :salary 200}]))

(require '[criterium.core :as cc])
(cc/quick-bench
  (let [filterer (-datoms-intersection '[[?e :name "Ivan"]])]
    (persistent! (filterer db (transient {})))))

;; TODO
(defn -q [query db]
  (let [pq (-query->map query)
        filterer (-datoms-intersection (ex/-get* pq :where))
        reducer (-reducer (ex/-get* pq :find))]
    (persistent! (filterer db (transient {})))))

(-q '[:find ?e ?salary
      :where
      [?e :salary ?salary]]
    minidb)

(d/q '[:find ?e ?salary
       :where
       [?e :salary ?salary]]
  miniddb)

(defn -oq [query dx]
  (let [pq (-query->map query)
        filterer (-datoms-matcher (ex/-get* pq :where))
        reducer (-reducer (ex/-get* pq :find))
        acc (-create-acc (ex/-get* pq :find) (ex/-get* pq :with))]
    (ex/-loop [me dx :let [acc acc]]
      (let [stack* (HashMap.)]
        (if (filterer stack* me)
          (let [acc' (reducer stack* acc dx)]
            (if (reduced? acc')
              @acc'
              (recur acc')))
          (recur acc)))
      (persistent! (reducer acc)))))

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

;; (require '[meander.epsilon :as me])
(cc/quick-bench
  (doall
    (me/search db
      {?ref {:name (me/some ?name)
             :age (me/some ?age)}}
      [?name ?age])))

(cc/quick-bench
  )

(cc/quick-bench
  (doall
    (me/search db
      (me/and {?e {:name "Ivan"
                  :age ?age
                  :salary 500
                  :friend "Pixel"}}
             (me/guard (> ?age 10)))
      ?e)))

;; 225 50
;; 235 16

(def db {[:db/id 1] {:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
         [:db/id 2] {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
         [:db/id 3] {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}})

(require '[criterium.core :as cc])
(require '[meander.epsilon :as me])
(require '[ribelo.doxa :as dx])

(def db (dx/create-dx (dxim/empty-db)
                      (into [{:db/id 1, :name "Ivan" , :age 15 :salary 500 :friend "Pixel"}
                             {:db/id 2, :name "Petr" , :age 37 }
                             {:db/id 3, :name "Ivan" , :age 37}]
                            (into [] (map (fn [i] {:db/id i :age (rand-int 80) :salary (rand-int 100)})) (range 4 1e3)))))

(def query
  '[[?e :friend ?friend]
    [?friend :name ?name]])

(group-by first query)

(defn -permutations [m]
  (ex/-mapv
    (fn [e]
      [[e] (dissoc m (key e))])
    m))

(-permutations {:a 1
                :b 2
                :c 3})

(require '[taoensso.encore :as enc])

(defn -process
  ([query db] (-process query db (transient [])))
  ([query db acc]
   (let [[e1 k1 v1] '[?e :friend ?friend]
         [e2 k2 v2] '[?friend :name ?name]]
     (ex/-loop [[ref1 m1] db :let [acc acc]]
       (recur
         (ex/-loop [[ref2 m2] (dissoc db ref1) :let [acc acc]]
           (if (= (ex/-get* m1 k1) ref2)
             (recur (conj! acc [ref1 (ex/-get* m2 k2)]))
             (recur acc))
           acc))
       (persistent! acc)))))

(cc/quick-bench (-process query db))

(require '[datascript.core :as d])
(def ddb (d/db-with (d/empty-db)
                    (into [{:db/id 1, :name "Ivan" , :age 15 :salary 500 :friend "Pixel"}
                           {:db/id 2, :name "Petr" , :age 37}
                           {:db/id 3, :name "Ivan" , :age 37}]
                          (into [] (map (fn [i] {:db/id i :age (rand-int 80) :salary (rand-int 100)})) (range 4 1e3)))))

(cc/quick-bench
  (d/q '[:find ?e
         :where
         [?e :age 20]]
    ddb))
