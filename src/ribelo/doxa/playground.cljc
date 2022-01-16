(ns ribelo.doxa.playground
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa :as dx]
   [ribelo.doxa.query :as dxq]
   [ribelo.doxa.impl.protocols :as p]
   [ribelo.doxa.util :as u]
   [ribelo.doxa.impl.map :as dxim]
   [datascript.core :as d]))

(def next-eid (volatile! 0))

(defn random-man []
  {:db/id     (str (vswap! next-eid inc))
   :name      (rand-nth ["Ivan" "Petr" "Sergei" "Oleg" "Yuri" "Dmitry" "Fedor" "Denis"])
   :last-name (rand-nth ["Ivanov" "Petrov" "Sidorov" "Kovalev" "Kuznetsov" "Voronoi"])
   :sex       (rand-nth [:male :female])
   :age       (long (rand-int 100))  ;;data hike complains if they're integers
   :salary    (long (rand-int 100000))})


(def people (repeatedly random-man))


(def people20k (shuffle (take 20000 people)))

(def db100k
  (d/db-with (d/empty-db) people20k))

(def dxdb100k (dx/create-dx (dxim/empty-db) people20k))

(require '[criterium.core :as cc])

(defn ddq1 []
  (d/q '[:find ?e
         :where [?e :name "Ivan"]]
    db100k))

(defn dxq1 []
  (dxq/-q '[:find ?e
         :where [?e :name "Ivan"]]
          dxdb100k))

(do
  (println :datascript :q1)
  (cc/quick-bench (ddq1))
  (println "\n")
  (println :doxa :q1)
  (cc/quick-bench (dxq1))
  (println "\n"))

(defn ddq2 []
  (d/q '[:find ?e ?a
         :where
         [?e :name "Ivan"]
         [?e :age ?a]]
    db100k))

(defn dxq2 []
  (dxq/-q '[:find ?e ?a
            :where
            [?e :name "Ivan"]
            [?e :age ?a]]
          dxdb100k))


(do
  (println :datascript :q2)
  (cc/quick-bench (ddq2))
  (println "\n")
  (println :doxa :q2)
  (cc/quick-bench (dxq2))
  (println "\n"))

(defn ddq3 []
  (d/q '[:find ?e ?a
         :where [?e :name "Ivan"]
         [?e :age ?a]
         [?e :sex :male]]
    db100k))


(defn dxq3 []
  (dxq/-q '[:find ?e ?a
            :where [?e :name "Ivan"]
            [?e :age ?a]
            [?e :sex :male]]
          dxdb100k))

(do
  (println :datascript :q3)
  (cc/quick-bench (ddq3))
  (println "\n")
  (println :doxa :q3)
  (cc/quick-bench (dxq3))
  (println "\n"))

(defn ddq4 []
  (d/q '[:find ?e ?l ?a
         :where [?e :name "Ivan"]
         [?e :last-name ?l]
         [?e :age ?a]
         [?e :sex :male]]
    db100k))


(defn dxq4 []
  (dxq/-q '[:find ?e ?l ?a
            :where [?e :name "Ivan"]
            [?e :last-name ?l]
            [?e :age ?a]
            [?e :sex :male]]
          dxdb100k))

(do
  (println :datascript :q4)
  (cc/quick-bench (ddq4))
  (println "\n")
  (println :doxa :q4)
  (cc/quick-bench (dxq4))
  (println "\n"))

(defn ddq5 []
  (d/q '[:find ?e1 ?l ?a
         :where
         [?e :name "Ivan"]
         [?e :age ?a]
         [?e1 :age ?a]
         [?e1 :last-name ?l]]
    db100k))

;; TODO
(defn dxq5 []
  (dxq/-q '[:find ?e1 ?l ?a
            :where
            [?e :name "Ivan"]
            [?e :age ?a]
            [?e1 :age ?a]
            [?e1 :last-name ?l]]
          dxdb100k))

(do
  (println :datascript :q4)
  (cc/quick-bench (ddq4))
  (println "\n")
  (println :doxa :q4)
  (cc/quick-bench (dxq4))
  (println "\n"))

(defn ddqpred1 []
  (d/q '[:find ?e ?s
         :where
         [?e :salary ?s]
         [(> ?s 50000)]
         [(< ?s 500000)]
         [(< ?s 500000)]
         [(< ?s 500000)]
         [(< ?s 500000)]]
    db100k))

(defn dxqpred1 []
  (dxq/-q '[:find ?e ?s
            :where
            [?e :salary ?s]
            [(> ?s 50000)]
            [(< ?s 500000)]
            [(< ?s 500000)]
            [(< ?s 500000)]
            [(< ?s 500000)]]
          dxdb100k))


(do
  (println :datascript :qpred1)
  (cc/quick-bench (ddqpred1))
  (println "\n")
  (println :doxa :qpred1)
  (cc/quick-bench (dxqpred1))
  (println "\n"))


(defn qpred2 []
  (core/bench
    (d/q '[:find ?e ?s
           :in   $ ?min_s
           :where [?e :salary ?s]
                  [(> ?s ?min_s)]]
      db100k 50000)))



