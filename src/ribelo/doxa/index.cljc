(ns ribelo.doxa.index
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa.util :as u])
  (:import
   (java.util HashMap HashSet)))

(defn -add-attr [index k e]
  (ex/-update-in index [:a k] ex/-conjs e))

(defn -add-val [index k e]
  (ex/-update-in index [:v k] ex/-conjs e))

(defn -del-attr [index k e]
  (ex/-update-in index [:a k] disj e))

(defn -del-val [index k e]
  (ex/-update-in index [:v k] disj e))

(defn -update-index [index diffs]
  (ex/-loop [[e kind a v :as diff] diffs :let [acc index]]
    (case kind
      :+ (recur (-> acc (-add-attr a e) (-add-val v e)))
      :- (recur (-> acc (-del-attr a e) (-del-val v e))))
    acc))
