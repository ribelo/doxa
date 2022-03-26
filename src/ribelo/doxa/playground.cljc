(ns ribelo.doxa.playground
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa :as dx]
   [ribelo.doxa.query :as dxq]
   [ribelo.doxa.pull-api :as dxp]
   [ribelo.doxa.protocols :as p]
   [ribelo.doxa.util :as u]
   [ribelo.doxa.map :as dxm]
   [ribelo.doxa.cache :as cache]
   [datascript.core :as d]
   [meander.epsilon :as m]
   [com.wotbrew.relic :as rel]
   [cljs-bean.core :refer [->js ->clj]]
   [cognitect.transit :as t]))

(def next-eid (volatile! 0))

(defn random-man []
  {:db/id     (str (vswap! next-eid inc))
   :name      (rand-nth ["Ivan" "Petr" "Sergei" "Oleg" "Yuri" "Dmitry" "Fedor" "Denis"])
   :last-name (rand-nth ["Ivanov" "Petrov" "Sidorov" "Kovalev" "Kuznetsov" "Voronoi"])
   :sex       (rand-nth ["male" "female"])
   :age       (long (rand-int 100))  ;;data hike complains if they're integers
   :salary    (long (rand-int 100000))})


(def people (repeatedly random-man))


(def people20k (shuffle (take 20000 people)))

(def people100 (shuffle (take 1000 people)))

(def db100k
  (d/db-with (d/empty-db) people20k))

(def dxdb100k (dx/create-dx (dxm/empty-db {:cache (cache/doxa-cache {:ttl-ms 0})}) people20k))

(def lmdb100k (lmdb/create-db {:path "playground_db10" :cache (cache/doxa-cache {:ttl-ms 0})}))
(def lmdbconn_ (p/-connect lmdb100k))

(ex/loop-it [x (partition-all 100 people20k) :let [acc 0]]
  (do (println :i acc)
      (dx/commit! lmdbconn_ [:dx/put x])
      (recur (inc acc)))
  acc)

(dx/commit! lmdbconn_ [:dx/put (second people20k)])
(count @lmdbconn_)

(+ 1 1)
(time (dx/commit! lmdbconn_ [:dx/put (first people20k)]))
(p/-put @lmdbconn_ [:db/id "13526"] (first people20k))

(def tmp (mapv (partial into []) (:db/id (.-index @lmdbconn_))))

(enc/qb 1
  (->js tmp)
  (->transit tmp))


(require '[criterium.core :as cc])
(require '[taoensso.encore :as enc])

(dx/filter (fn [{:keys [name]}] (= name "Ivan")) dxdb100k)
(u/-ref-lookup? (first dxdb100k))

(loop [acc 0]
  (when false (recur (inc acc)))
  acc)

(defn ddq1 []
  (d/q '[:find ?e
         :where [?e :name "Ivan"]]
    db100k))

(defn dxq1 []
  (time (dxq/-mq '[:find ?e
                  :where [?e :name "Ivan"]]
                 @lmdbconn_)))

(ex/loop-it [me @lmdbconn_ :let [acc []]]
  (recur (conj acc me))
  acc)

(def it (-iterator @lmdbconn_))
(od (.hasNe) (.next it))


(defn relq1 []
  (doall (rel/q reldb100k [[:from :people] [:where [= :name "Ivan"]]])))

(defn relq1 []
  (doall (rel/mat reldb100k [[:from :people] [:where [= :name "Ivan"]]])))

(do
  (println :datascript :q1)
  (cc/quick-bench (ddq1))
  (enc/qb 1e1 (ddq1))
  (println :doxa :q1)
  (enc/qb 1e1 (dxq1))
  (println :relic :q1)
  (cc/quick-bench (relq1))
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

(defn relq2 []
  (rel/q reldb100k [[:from :people]
                    [:where
                     [= :name "Ivan"]]
                    [:select :age :db/id]
                    ]))

(do
  (println :datascript :q2)
  (enc/qb 1e1 (ddq2))
  (println :doxa :q2)
  (enc/qb 1e1 (dxq2))
  (println :relic :q2)
  (cc/quick-bench (relq2))
  (println "\n"))

(defn ddq3 []
  (d/q '[:find ?e ?a
         :where
         [?e :name "Ivan"]
         [?e :age ?a]
         [?e :sex "male"]]
    db100k))

(defn dxq3 []
  (dxq/-mq '[:find ?e ?a
            :where
            [?e :name "Ivan"]
            [?e :age ?a]
            [?e :sex "male"]]
           @lmdbconn_))

(defn relq3 []
  (rel/q reldb100k [[:from :people]
                    [:where
                     [= :name "Ivan"]
                     [= :sex "male"]]
                    [:select :age :db/id]]))

(do
  (require '[taoensso.encore :as enc])
  (println :datascript :q3)
  (time (dotimes [n 10] (ddq3)))
  (println :doxa :q3)
  (time (dotimes [n 10] (dxq3)))
  ;; (println :relic :q3)
  ;; (cc/quick-bench (relq3))
  (println "\n"))

(require '[clj-async-profiler.core :as prof])
(prof/serve-files 8080)
(prof/profile (dotimes [_ 1e3] (dxq3)))

(defn ddq4 []
  (d/q '[:find ?e ?l ?a
         :where
         [?e :name "Ivan"]
         [?e :last-name ?l]
         [?e :age ?a]
         [?e :sex "male"]]
    db100k))

(defn dxq4 []
  (dxq/-q '[:find ?e ?l ?a
            :where
            [?e :name "Ivan"]
            [?e :last-name ?l]
            [?e :age ?a]
            [?e :sex "male"]]
          dxdb100k))

(defn relq4 []
  (rel/q reldb100k [[:from :people]
                    [:where
                     [= :name "Ivan"]
                     [= :sex "male"]]
                    [:select :age :db/id :last-name]
                    ]))

(do
  (println :datascript :q4)
  (cc/quick-bench (ddq4))
  (println "\n")
  (println :doxa :q4)
  (cc/quick-bench (dxq4))
  ;; (println :relic :q4)
  ;; (cc/quick-bench (relq4))
  (println "\n"))

(defn ddq5 []
  (d/q '[:find ?e1 ?l ?a
         :where
         [?e :name "Ivan"]
         [?e :age ?a]
         [?e1 :age ?a]
         [?e1 :last-name ?l]]
    db100k))

(defn dxq5 []
  (time (dxq/-q '[:find ?e1 ?l ?a
             :where
             [?e :name "Ivan"]
             [?e :age ?a]
             [?e1 :age ?a]
             [?e1 :last-name ?l]]
           dxdb100k)))

(do
  (println :datascript :q5)
  (ddq5)
  (println "\n")
  (println :doxa :q5)
  (dxq5)
  (println "\n"))

(def idb (.-db dxdb100k))

(count (m/search idb
   {?e {:name ?name
        :age ?a}
    ?e1 {:age ?a
         :last-name ?l}}
         [?e ?l ?a]))

(do
  (println :datascript :q5)
  (cc/quick-bench (ddq5))
  (println "\n")
  (println :doxa :q4)
  (cc/quick-bench (dxq5))
  (println "\n"))

(defn ddqpred1 []
  (d/q '[:find ?e ?s
         :where [?e :salary ?s]
         [(> ?s 50000)]]
    db100k))


(defn dxqpred1 []
  (dxq/-q '[:find ?e ?s
            :where
            [?e :salary ?s]
            [(> ?s 50000)]]
          dxdb100k))

(do
  (println :datascript :qpred1)
  (enc/qb 1e1 (ddqpred1))
  (println "\n")
  (println :doxa :qpred1)
  (enc/qb 1e1 (dxqpred1))
  (println "\n"))



(defn qpred2 []
  (d/q '[:find ?e ?s
         :in   $ ?min_s
         :where [?e :salary ?s]
         [(> ?s ?min_s)]]
    db100k 50000))
