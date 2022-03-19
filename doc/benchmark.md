- [random data](#org9031495)
- [transactions](#orge987904)
  - [adding data one transaction at a time](#orgc479aa8)
  - [add all data in single transaction](#org3602cc5)
- [query](#orgb4a2f64)
  - [one condition](#org814df78)
  - [three conditions](#orgaa2ae2c)
  - [four conditions](#org09c3d31)
  - [one pred](#org39360a8)
  - [two preds](#org3176119)
  - [three preds](#org61b0d0c)
- [pull](#org74a4d55)



<a id="org9031495"></a>

# random data

```clojurescript

(require '[ribelo.extropy  :as  ex])
(require '[datascript.core :as   d])
(require '[ribelo.doxa     :as  dx])

```

```clojurescript

(let [next-eid (volatile! 0)]

  (defn random-fruit []
    {:fruit/id  (vswap! next-eid inc)
     :name      (rand-nth ["Avocado" "Grape" "Plum" "Apple" "Orange"])
     :weight    (rand-int 100)
     :size      (rand-int 100)
     :price     (rand-int 100)})

  (defn random-vegetable []
    {:vegetable/id (vswap! next-eid inc)
     :name         (rand-nth ["Onion" "Cabbage" "Pea" "Tomatto" "Lettuce"])
     :weight       (rand-int 100)
     :size         (rand-int 100)
     :price        (rand-int 100)})

  (defn random-animal []
    {:animal/id (vswap! next-eid inc)
     :name      (rand-nth ["Otter" "Dog" "Panda" "Lynx" "Cat" "Lion"])
     :weight    (rand-int 100)
     :size      (rand-int 100)
     :price     (rand-int 100)})

  (defn random-cat []
    {:cat/id    (vswap! next-eid inc)
     :name      (rand-nth ["Traditional Persian" "Ocicat" "Munchkin cat" "Persian cat" "Burmese cat"])
     :weight    (rand-int 100)
     :size      (rand-int 100)
     :price     (rand-int 100)})

  (defn random-dog []
    {:dog/id    (vswap! next-eid inc)
     :name      (rand-nth ["Croatian Shepherd" "Deutch Langhaar" "Miniature Pincher" "Italian Sighthound" "Jack Russell Terrier"])
     :weight    (rand-int 100)
     :size      (rand-int 100)
     :price     (rand-int 100)}))

(def fruit            (repeatedly random-fruit))
(def vegetable        (repeatedly random-vegetable))
(def animal           (repeatedly random-animal))
(def cat              (repeatedly random-cat))
(def dog              (repeatedly random-dog))


(def fruit20k         (shuffle (take 20000 fruit)))
(def vegetable20k     (shuffle (take 20000 vegetable)))
(def animal20k        (shuffle (take 20000 animal)))
(def cat20k           (shuffle (take 20000 cat)))
(def dog20k           (shuffle (take 20000 dog)))

(def data100k (ex/-into-all [] fruit20k vegetable20k  animal20k cat20k dog20k))

(def ds100k
  (persistent!
   (reduce
    (fn [acc m]
      (conj!
       acc
       (reduce-kv
        (fn [m k v]
          (if (= "id" (name k))
            (assoc m :db/id v)
            (assoc m k v)))
        {}
        m)))
    (transient [])
    data100k)))

```


<a id="orge987904"></a>

# transactions


<a id="orgc479aa8"></a>

## adding data one transaction at a time

```clojurescript

(defn datascript-add-1 [data]
  (ex/-qb 1
    (reduce
     (fn [db p]
       (-> db
           (d/db-with [[:db/add (:db/id p) :name      (:name p)]])
           (d/db-with [[:db/add (:db/id p) :weight    (:weight p)]])
           (d/db-with [[:db/add (:db/id p) :size      (:size p)]])
           (d/db-with [[:db/add (:db/id p) :price     (:price p)]])))
     (d/empty-db)
     data)))

(defn doxa-add-1 [data]
  (ex/-qb 1
    (reduce
     (fn [db p]
       (dx/commit db [[:dx/put p]]))
     {}
     data)))

;; result in ms
(let [ds (take 1e4 ds100k)
      dx (take 1e4 data100k)]
  [(datascript-add-1 ds) (doxa-add-1 dx)])
;; => [548 230]
```


<a id="org3602cc5"></a>

## add all data in single transaction

```clojurescript

(defn datascript-add-all [data]
  (ex/-qb 1
    (d/db-with (d/empty-db) data)))

(defn doxa-add-all [data]
  (ex/-qb 1
    (dx/commit {} [:dx/put data])))

(let [ds (take 1e4 ds100k)
      dx (take 1e4 data100k)]
  [(datascript-add-all ds) (doxa-add-all dx)])
;; => [532 223]

```


<a id="orgb4a2f64"></a>

# query

```clojurescript

(def ds100k
  (d/db-with (d/empty-db) ds100k))

(def dx100k (dx/create-dx {} data100k))

(require '[ribelo.doxa.cache :as dxc])
(def mdx100k (dx/create-dx {} data100k {::dx/cache (atom (dxc/doxa-cache))}))

```


<a id="org814df78"></a>

## one condition

```clojurescript

(defn datascript-q1 []
  (ex/-qb 1e1
    (d/q '[:find ?e
           :where [?e :name "Apple"]]
      ds100k)))

(defn dx-q1 []
  (ex/-qb 1e1
    (dx/q '[:find  ?e
            :where [?e :name "Apple"]]
      dx100k)))

(defn dx-q1-table []
  (ex/-qb 1e1
    (dx/q '[:find  ?e
            :where [?e :name "Apple"]]
      (dx/table dx100k :fruit/id))))

(defn dx-mq1 []
  (ex/-qb 1e1
    (dx/mq '[:find  ?e
             :where [?e :name "Apple"]]
      mdx100k)))

(defn transduce-q1 []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (= "Apple" (m :name))))
        (map :fruit/id))
      (vals dx100k))))

(defn transduce-q1-table []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (= "Apple" (m :name))))
        (map :fruit/id))
      (vals (dx/table dx100k :fruit/id)))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])
(zipmap ks [(datascript-q1) (dx-q1) (dx-mq1) (dx-q1-table) (transduce-q1)])
;; => {:datascript 251,
;;     :doxa 530,
;;     :memoized-doxa 0,
;;     :doxa-with-table 452,
;;     :transduce 636}
```


<a id="orgaa2ae2c"></a>

## three conditions

```clojurescript

(defn datascript-q3 []
  (ex/-qb 1e1
    (d/q '[:find ?e ?p
           :where
           [?e :name "Apple"]
           [?e :price ?p]
           [?e :weight 50]]
      ds100k)))

(defn dx-q3 []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [?e :weight 50]]
      dx100k)))

(defn dx-mq3 []
  (ex/-qb 1e1
    (dx/mq '[:find ?e ?p
             :where
             [?e :name "Apple"]
             [?e :price ?p]
             [?e :weight 50]]
      mdx100k)))

(defn dx-q3-table []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [?e :weight 50]]
      (dx/table dx100k :fruit/id))))

(defn transduce-q3 []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (and (= "Apple" (m :name))
                            (= 50 (m :weight)))))
        (map (juxt :fruit/id :price)))
      (vals dx100k))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])
(zipmap ks [(datascript-q3) (dx-q3) (dx-mq3) (dx-q3-table) (transduce-q3)])
;; => {:datascript 1092,
;;     :doxa 566,
;;     :memoized-doxa 0,
;;     :doxa-with-table 478,
;;     :transduce 689}
```


<a id="org09c3d31"></a>

## four conditions

```clojurescript

(defn datascript-q4 []
  (ex/-qb 1e1
    (d/q '[:find ?e ?p
           :where
           [?e :name "Apple"]
           [?e :price ?p]
           [?e :weight 50]
           [?e :size 50]]
      ds100k)))

(defn dx-q4 []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [?e :weight 50]
            [?e :size 50]]
      dx100k)))

(defn dx-mq4 []
  (ex/-qb 1e1
    (dx/mq '[:find ?e ?p
             :where
             [?e :name "Apple"]
             [?e :price ?p]
             [?e :weight 50]
             [?e :size 50]]
      mdx100k)))

(defn dx-q4-table []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [?e :weight 50]]
      (dx/table dx100k :fruit/id))))

(defn transduce-q4 []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (and (= "Apple" (m :name))
                            (= 50 (m :weight))
                            (= 50 (m :size)))))
        (map (juxt :fruit/id :price)))
      (vals dx100k))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])
(zipmap ks [(datascript-q4) (dx-q4) (dx-mq4) (dx-q4-table) (transduce-q4)])
;; => {:datascript 1428,
;;     :doxa 585,
;;     :memoized-doxa 0,
;;     :doxa-with-table 485,
;;     :transduce 693}

```


<a id="org39360a8"></a>

## one pred

```clojurescript

(defn datascript-qpred1 []
  (ex/-qb 1e1
    (d/q '[:find ?e ?p
           :where
           [?e :name "Apple"]
           [?e :price ?p]
           [(> ?p 50)]]
      ds100k)))

(defn dx-qpred1 []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [(> ?p 50)]]
      dx100k)))

(defn dx-mqpred1 []
  (ex/-qb 1e1
    (dx/mq '[:find ?e ?p
             :where
             [?e :name "Apple"]
             [?e :price ?p]
             [(> ?p 50)]]
      mdx100k)))

(defn dx-qpred1-table []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [?e :weight 50]]
      (dx/table dx100k :fruit/id))))

(defn transduce-qpred1 []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (and (= "Apple" (m :name))
                            (> (m :price) 50))))
        (map (juxt :fruit/id :price)))
      (vals dx100k))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])
(zipmap ks [(datascript-qpred1) (dx-qpred1) (dx-mqpred1) (dx-qpred1-table) (transduce-qpred1)])
;; => {:datascript 761,
;;     :doxa 636,
;;     :memoized-doxa 0,
;;     :doxa-with-table 470,
;;     :transduce 667}
```


<a id="org3176119"></a>

## two preds

```clojurescript

(defn datascript-qpred2 []
  (ex/-qb 1e1
    (d/q '[:find ?e ?p
           :where
           [?e :name "Apple"]
           [?e :price ?p]
           [(> ?p 50)]
           [?e :weight ?w]
           [(> ?w 50)]]
      ds100k)))

(defn dx-qpred2 []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [(> ?p 50)]
            [?e :weight ?w]
            [(> ?w 50)]]
      dx100k)))

(defn dx-mqpred2 []
  (ex/-qb 1e1
    (dx/mq '[:find ?e ?p
             :where
             [?e :name "Apple"]
             [?e :price ?p]
             [(> ?p 50)]
             [?e :weight ?w]
             [(> ?w 50)]]
      mdx100k)))

(defn dx-qpred2-table []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [(> ?p 50)]
            [?e :weight ?w]
            [(> ?w 50)]]
      (dx/table dx100k :fruit/id))))

(defn transduce-qpred2 []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (and (= "Apple" (m :name))
                            (> (m :price) 50)
                            (> (m :weight) 50))))
        (map (juxt :fruit/id :price)))
      (vals dx100k))))

(defn transduce-qpred2-table []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (and (= "Apple" (m :name))
                            (> (m :price) 50)
                            (> (m :weight) 50))))
        (map (juxt :fruit/id :price)))
      (vals (dx/table dx100k :fruit/id)))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])
(zipmap ks [(datascript-qpred2) (dx-qpred2) (dx-mqpred2) (dx-qpred2-table) (transduce-qpred2)])
;; => {:datascript 1247,
;;     :doxa 651,
;;     :memoized-doxa 0,
;;     :doxa-with-table 546,
;;     :transduce 637}
```


<a id="org61b0d0c"></a>

## three preds

```clojurescript

(defn datascript-qpred3 []
  (ex/-qb 1e1
    (d/q '[:find ?e ?p
           :where
           [?e :name "Apple"]
           [?e :price ?p]
           [(> ?p 50)]
           [?e :weight ?w]
           [(> ?w 50)]
           [?e :size ?s]
           [(> ?s 50)]]
      ds100k)))

(defn dx-qpred3 []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [(> ?p 50)]
            [?e :weight ?w]
            [(> ?w 50)]
            [?e :size ?s]
            [(> ?s 50)]]
      dx100k)))

(defn dx-mqpred3 []
  (ex/-qb 1e1
    (dx/mq '[:find ?e ?p
             :where
             [?e :name "Apple"]
             [?e :price ?p]
             [(> ?p 50)]
             [?e :weight ?w]
             [(> ?w 50)]
             [?e :size ?s]
             [(> ?s 50)]]
      mdx100k)))

(defn dx-qpred3-table []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]
            [(> ?p 50)]
            [?e :weight ?w]
            [(> ?w 50)]
            [?e :size ?s]
            [(> ?s 50)]]
      (dx/table dx100k :fruit/id))))

(defn transduce-qpred3 []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (and (= "Apple" (m :name))
                            (> (m :price) 50)
                            (> (m :weight) 50)
                            (> (m :size) 50))))
        (map (juxt :fruit/id :price)))
      (vals dx100k))))

(defn transduce-qpred3-table []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (and (= "Apple" (m :name))
                            (> (m :price) 50)
                            (> (m :weight) 50)
                            (> (m :size) 50))))
        (map (juxt :fruit/id :price)))
      (vals (dx/table dx100k :fruit/id)))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])
(zipmap ks [(datascript-qpred3) (dx-qpred3) (dx-mqpred3) (dx-qpred3-table) (transduce-qpred3)])
;; => {:datascript 1697,
;;     :doxa 703,
;;     :memoized-doxa 1,
;;     :doxa-with-table 549,
;;     :transduce 629}

```


<a id="org74a4d55"></a>

# pull

```clojurescript
(defn people
  ([n] (people n 1))
  ([n i]
   (if (< i n)
     {:db/id i
      :name (rand-nth ["Ivan" "Petr" "Sergei" "Oleg" "Yuri" "Dmitry" "Fedor" "Denis"])
      :friend (people n (inc i))}
     {:db/id i
      :name (rand-nth ["Ivan" "Petr" "Sergei" "Oleg" "Yuri" "Dmitry" "Fedor" "Denis"])})))

(def data1k (people 1001))

(def schema
  {:friend {:db/valueType   :db.type/ref}})

(def db1k
  (d/db-with (d/empty-db schema) [data1k]))

(def dx1k  (dx/create-dx {} [data1k]))
(def mdx1k (dx/create-dx {} [data1k] {::dx/cache (atom (dxc/doxa-cache))}))

```

```clojurescript

(defn make-query
  ([n] [(make-query n 1)])
  ([n i]
   (if (< i n)
     {:friend [(make-query n (inc i))]}
     {:friend [:name]})))

(make-query 3)
;; => [{:friend [{:friend [{:friend [:name]}]}]}]

(def q1   (make-query 1))
(def q10  (make-query 10))
(def q100 (make-query 100))
(def q999 (make-query 999))

(defn datascript-pull1 []
  (ex/-qb 1e3 (d/pull db1k q1 1)))

(defn dx-pull1 []
  (ex/-qb 1e3 (dx/pull dx1k q1 [:db/id 1])))

(defn dx-mpull1 []
  (ex/-qb 1e3 (dx/mpull mdx1k q1 [:db/id 1])))

(def ks [:datascript :doxa :materialised-doxa])
(zipmap ks [(datascript-pull1) (dx-pull1) (dx-mpull1)])
;; => {:datascript 3,
;;     :doxa 5,
;;     :memoized-doxa 2}

(defn datascript-pull10 []
  (ex/-qb 1e3 (d/pull db1k q10 1)))

(defn dx-pull10 []
  (ex/-qb 1e3 (dx/pull dx1k q10 [:db/id 1])))

(defn dx-mpull10 []
  (ex/-qb 1e3 (dx/mpull mdx1k q10 [:db/id 1])))

(zipmap ks [(datascript-pull10) (dx-pull10) (dx-mpull10)])
;; => {:datascript 22,
;;     :doxa 26,
;;     :materialised-doxa 6}

(defn datascript-pull100 []
  (ex/-qb 1e3 (d/pull db1k q100 1)))

(defn dx-pull100 []
  (ex/-qb 1e3 (dx/pull dx1k q100 [:db/id 1])))

(defn dx-mpull100 []
  (ex/-qb 1e3 (dx/mpull mdx1k q100 [:db/id 1])))

(zipmap ks [(datascript-pull100) (dx-pull100) (dx-mpull100)])
;; => {:datascript 168,
;;     :doxa 197,
;;     :materialised-doxa 13}

(defn datascript-pull999 []
  (ex/-qb 1e3 (d/pull db1k q999 1)))

(defn dx-pull999 []
  (ex/-qb 1e3 (dx/pull dx1k q999 [:db/id 1])))

(defn dx-mpull999 []
  (ex/-qb 1e3 (dx/mpull mdx1k q999 [:db/id 1])))

(zipmap ks [#_(datascript-pull999) (dx-pull999) #_(dx-mpull999)])
;; => {:datascript Maximum call stack size exceeded
;;     :doxa 2018,
;;     :materialised-doxa Maximum call stack size exceeded}

```
