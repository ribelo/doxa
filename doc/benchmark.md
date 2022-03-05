- [transactions](#orgaef344e)
  - [adding data one transaction at a time](#orgd597f6c)
  - [add all data in single transaction](#org26a50c2)
- [query](#orgf7ec01c)
  - [one condition](#org2f7d78c)
  - [two conditions](#org77ba411)
  - [three conditions](#org70d3fe7)
  - [four conditions](#orgcf93b82)
  - [one pred](#org3c31a5c)
  - [two preds](#org9d43f14)
  - [three preds](#org3aab5ef)



<a id="orgaef344e"></a>

# transactions


<a id="orgd597f6c"></a>

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


<a id="org26a50c2"></a>

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


<a id="orgf7ec01c"></a>

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


<a id="org2f7d78c"></a>

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


<a id="org77ba411"></a>

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


<a id="org70d3fe7"></a>

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


<a id="orgcf93b82"></a>

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


<a id="org3c31a5c"></a>

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


<a id="org9d43f14"></a>

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


<a id="org3aab5ef"></a>

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
