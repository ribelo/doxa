(ns ribelo.doxa.index
  (:require
   [ribelo.exin :as ex]))

(defn -update-index [index diffs]
  (ex/-loop [[[tid eid :as ref] kind a v :as diff] diffs :let [acc index]]
    (if (ex/-kw-identical? tid a)
      (case kind
        :+ (recur (ex/-update acc tid ex/-conjs ref))
        :- (recur (ex/-update acc tid disj ref)))
      (recur acc))
    acc))
