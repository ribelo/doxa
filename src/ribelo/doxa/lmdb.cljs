(ns ribelo.doxa.lmdb
  (:require
   [ribelo.doxa.impl :as impl]
   ["lmdb" :as lmdb]
   [cognitect.transit :as t]
   [cljs-bean.core :refer [->js ->clj]]))

(defn ->transit [x]
  (let [w (t/writer :json)]
    (t/write w x)))

(defn <-transit [x]
  (let [r (t/reader :json)]
    (t/read r x)))

(defn path->key [x]
  (cond
    (keyword? x)
    (str x)

    (vector? x)
    (.join (into-array (mapv path->key x)) "__")

    :else
    x))

(defn key->path [x]
  (if (pos? (.indexOf x "__"))
    (mapv key->path (.split x "__"))
    (if (zero? (.indexOf x ":"))
      (keyword (.substring x 1))
      (let [n (js/parseFloat x)]
        (if (js/isNaN n) x n)))))

(deftype LMDB [db]
  impl/IDoxa
  (-put-in [this ks v]
    (case (count ks)
      2 (let [fk (path->key (nth ks 0))
              ks' (path->key ks)
              xs (or (.get db fk) #js [])]
          (.push xs ks')
          (.put db fk (to-array (into #{} xs)))
          (.put db ks' (->transit v))
          this)
      3 (let [m (impl/-get-in this (pop ks))]
          (impl/-put-in this (path->key (pop ks)) (assoc m (peek ks) v)))))

  (-get-in [this ks]
    (case (count ks)
      1 (let [k (path->key (nth ks 0))
              xs (.get db k)]
          (reduce
            (fn [acc k]
              (assoc-in acc (key->path k) (<-transit (.get db k))))
            {}
            xs))
      2 (<-transit (.get db (path->key ks)))
      3 (.get (impl/-get-in this (pop ks)) (peek ks))))

  (-del-in [this ks]
    (case (count ks)
      1 (let [k (path->key (nth ks 0))
              xs (.get db k)]
          (run! (fn [k] (.del db k)) xs))
      2 (let [fk (path->key (nth ks 0))
              ks' (path->key ks)
              xs (or (.get db fk) #js [])]
          (.put db fk (to-array (into #{} (remove #{ks'}) xs)))
          (.del db ks')
          this)
      3 (let [m (impl/-get-in this (pop ks))]
          (impl/-put-in this (pop ks) (dissoc m (peek ks)))))))

(defn create-db [{:keys [path]}]
  (LMDB. (.open lmdb path)))

(def db_ (create-db {:path "testdb1"}))
(impl/-put-in db_ [:db/id 1] {:a 1 :b 2 :c 3})
(impl/-put-in db_ [:db/id 2] {:a 1 :b 2 :c 3})
(impl/-get-in db_ [:db/id 1])
(impl/-del-in db_ [:db/id 1 :a])

(run! println (.getRange (.-db db_)))


(run! #(.del db_ %) (.getKeys db_))

(into [] (.getKeys db_))
(into [] (.getValues db_))
(.get db_ ":db/id__1")


(require '[taoensso.encore :as enc])

(def m {:sex {:a {:a 1 :b 2 :c 3 :d 4 :e 5 :f [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
               :g [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
               :h [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]}
              :b {:a 1 :b 2 :c 3 :d 4 :e 5 :f [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
               :g [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
                  :h [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]}}})

(def b (->clj (->js {:sex {:a {:a 1 :b 2 :c 3 :d 4 :e 5 :f [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
                         :g [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
                         :h [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]}
                     :b {:a 1 :b 2 :c 3 :d 4 :e 5 :f [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
                         :g [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]
                         :h [1 2 3 :a :b :c {:a 1 :b 2 :c 3}]}}})))

(-> {:a/b 1} ->js ->clj)

(impl/-put db_ :sex (:sex m))
(.put db_ ["a" "b"] 1)

(enc/qb 1e4
  (get m :sex)
  (.get db_ ":sex")
  (impl/-get db_ :sex))

(enc/qb 1e3
  (-> m ->js ->clj)
  (-> m ->transit <-transit))
