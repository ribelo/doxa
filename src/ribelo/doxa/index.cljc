(ns ribelo.doxa.index
  (:require
   [ribelo.exin :as ex])
  (:import
   [ribelo.doxa.util DoxaDBChange]))

(defn -update-index [index changes]
  (ex/-loop [^DoxaDBChange change changes :let [acc index]]
    (let [[tid _ :as ref] (.-e change)]
      (if (ex/-kw-identical? tid (.-a change))
        (case (.-kind change)
          :+ (recur (ex/-update acc tid ex/-conjs ref))
          :- (recur (ex/-update acc tid disj ref)))
        (recur acc)))
    acc))
