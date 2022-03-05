- [transactions](#org07591a5)
  - [adding data one transaction at a time](#orgb4f1ca8)
  - [add all data in single transaction](#org5169aed)
- [query](#org7edf225)
  - [one condition](#org0c1c4e6)
  - [two conditions](#org9acb09d)
  - [three conditions](#orgd70bc04)
  - [four conditions](#org8fce681)
  - [one pred](#orgcf02878)
  - [two preds](#org171ad4f)
  - [three preds](#org4136aac)
- [pull](#orgdf3589a)



<a id="org07591a5"></a>

# transactions


<a id="orgb4f1ca8"></a>

## adding data one transaction at a time

```clojure

(defn datascript-add-1 [data]
  (ex/-qb 1
    (reduce
     (fn [db p]
       (-> db
           (d/db-with [[:db/add (:db/id p) :name      (:name p)]])
           (d/db-with [[:db/add (:db/id p) :last-name (:last-name p)]])
           (d/db-with [[:db/add (:db/id p) :age       (:age p)]])
           (d/db-with [[:db/add (:db/id p) :salary    (:salary p)]])))
     (d/empty-db schema)
     data)))

(defn doxa-add-1 [data]
  (ex/-qb 1
    (reduce
     (fn [db p]
       (dx/commit db [[:dx/put p]]))
     {}
     data)))

;; result in ms
[(datascript-add-1 people50k) (doxa-add-1 people50k)]
;; => [1020.39 334.32]
```


<a id="org5169aed"></a>

## add all data in single transaction

```clojure

(defn datascript-add-all []
  (ex/-qb 1
    (d/db-with (d/empty-db schema) people50k)))

(defn doxa-add-all []
  (ex/-qb 1
    (->> (into []
               (map (fn [p] [:dx/put p]))
               people50k)
         (dx/commit {}))))

(defn doxa-add-all []
  (ex/-qb 1
    (dx/commit {} [:dx/put people50k])))

[(datascript-add-all) (doxa-add-all)]
;; => [2817.18 274.18]

```


<a id="org7edf225"></a>

# query

```clojure

(def ds100k
  (d/db-with (d/empty-db)
             (mapv
               (fn [m]
                 (reduce-kv
                   (fn [acc k v]
                     (if (= :id (name k))
                       (assoc acc :db/id v)
                       (assoc acc k v)))
                   {}
                   m))
               data100k)))

(def dx100k (dx/create-dx {} data100k))

(require '[ribelo.doxa.cache :as dxc])
(def mdx100k (dx/create-dx {} data100k {::dx/cache (atom (dxc/doxa-cache))}))

```


<a id="org0c1c4e6"></a>

## one condition

```clojure

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
        (map :db/id))
      (vals dx100k))))

(defn transduce-q1-table []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (= "Apple" (m :name))))
        (map :db/id))
      (vals (dx/table dx100k :fruit/id)))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])

(zipmap ks [(datascript-q1) (dx-q1) (dx-mq1) (dx-q1-table) (transduce-q1)])
;; => {:datascript 33.01,
;;     :doxa 127.04,
;;     :memoized-doxa 0.04,
;;     :doxa-with-table 119.83,
;;     :transduce 85.87}

```


<a id="org9acb09d"></a>

## two conditions

```clojure

(defn datascript-q2 []
  (ex/-qb 1e1
    (d/q '[:find ?e ?p
           :where
           [?e :name "Apple"]
           [?e :price ?p]]
      ds100k)))

(defn dx-q2 []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]]
      dx100k)))

(defn dx-mq2 []
  (ex/-qb 1e1
    (dx/mq '[:find ?e ?p
             :where
             [?e :name "Apple"]
             [?e :price ?p]]
      mdx100k)))

(defn dx-q2-table []
  (ex/-qb 1e1
    (dx/q '[:find ?e ?p
            :where
            [?e :name "Apple"]
            [?e :price ?p]]
      (dx/table dx100k :fruit/id))))

(defn transduce-q2 []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (= "Apple" (m :name))))
        (map (juxt :fruit/id :price)))
      (vals dx100k))))

(defn transduce-q2-table []
  (ex/-qb 1e1
    (into []
      (comp
        (filter (fn [m] (= "Apple" (m :name))))
        (map (juxt :fruit/id :price)))
      (vals (dx/table dx100k :fruit/id)))))

(def ks [:datascript :doxa :memoized-doxa :doxa-with-table :transduce])
(zipmap ks [(datascript-q2) (dx-q2) (dx-mq2) (dx-q2-table) (transduce-q2)])
;; => {:datascript 123.66,
;;     :doxa 147.0,
;;     :memoized-doxa 0.04,
;;     :doxa-with-table 130.35,
;;     :transduce 91.19}

```


<a id="orgd70bc04"></a>

## three conditions

```clojure

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
;; => {:datascript 159.36,
;;     :doxa 146.08,
;;     :memoized-doxa 0.04,
;;     :doxa-with-table 111.84,
;;     :transduce 88.09}

```


<a id="org8fce681"></a>

## four conditions

```clojure

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
;; => {:datascript 203.11,
;;     :doxa 152.02,
;;     :memoized-doxa 0.04,
;;     :doxa-with-table 109.87,
;;     :transduce 85.63}

```


<a id="orgcf02878"></a>

## one pred

```clojure

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
;; => {:datascript 128.0,
;;     :doxa 159.31,
;;     :memoized-doxa 0.04,
;;     :doxa-with-table 109.35,
;;     :transduce 89.37}

```


<a id="org171ad4f"></a>

## two preds

```clojure

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
;; => {:datascript 207.46,
;;     :doxa 172.97,
;;     :memoized-doxa 0.04,
;;     :doxa-with-table 136.76,
;;     :transduce 88.25}

```


<a id="org4136aac"></a>

## three preds

```clojure

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
;; => {:datascript 280.67,
;;     :doxa 195.71,
;;     :memoized-doxa 0.04,
;;     :doxa-with-table 142.62,
;;     :transduce 88.63}

```


<a id="orgdf3589a"></a>

# pull

```clojure
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

```clojure

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
;; => {:datascript 6.16,
;;     :doxa 2.2,
;;     :materialised-doxa 1.62}

(defn datascript-pull10 []
  (ex/-qb 1e3 (d/pull db1k q10 1)))

(defn dx-pull10 []
  (ex/-qb 1e3 (dx/pull dx1k q10 [:db/id 1])))

(defn dx-mpull10 []
  (ex/-qb 1e3 (dx/mpull mdx1k q10 [:db/id 1])))

(zipmap ks [(datascript-pull10) (dx-pull10) (dx-mpull10)])
;; => {:datascript 7.82,
;;     :doxa 6.51,
;;     :materialised-doxa 3.18}

(defn datascript-pull100 []
  (ex/-qb 1e3 (d/pull db1k q100 1)))

(defn dx-pull100 []
  (ex/-qb 1e3 (dx/pull dx1k q100 [:db/id 1])))

(defn dx-mpull100 []
  (ex/-qb 1e3 (dx/mpull mdx1k q100 [:db/id 1])))

(zipmap ks [(datascript-pull100) (dx-pull100) (dx-mpull100)])
;; => {:datascript 68.56,
;;     :doxa 56.78,
;;     :materialised-doxa 18.04}

(defn datascript-pull999 []
  (ex/-qb 1e3 (d/pull db1k q999 1)))

(defn dx-pull999 []
  (ex/-qb 1e3 (dx/pull dx1k q999 [:db/id 1])))

(defn dx-mpull999 []
  (ex/-qb 1e3 (dx/mpull mdx1k q999 [:db/id 1])))

(zipmap ks [#_(datascript-pull999) (dx-pull999) (dx-mpull999)])
;; => {:datascript java.lang.StackOverflowError
;;     :doxa 581.97,
;;     :materialised-doxa 170.57}
```
