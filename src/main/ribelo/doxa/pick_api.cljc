(ns ribelo.doxa.pick-api
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.util :as u]))

(defn -pick
  ([db ref]
   (-pick db :* ref))
  ([db query ^clojure.lang.IPersistentVector ref]
   (let [xs (persistent! (-pick db query (transient []) ref))]
     (when (seq xs)
       (if (= 1 (count xs))
         (first xs)
         xs))))
  ([db query ^clojure.lang.ITransientVector acc ^clojure.lang.IPersistentVector ref]
   (when db
     (cond
       (= :* query)
       (ex/conj-some! acc (get db ref))

       (keyword? query)
       (ex/conj-some! acc (get-in db [ref query]))

       (vector? query)
       (ex/conj-some! acc
         (not-empty
          (ex/ensure-persisten!
           (reduce
            (fn [acc' q]
              (cond
                (keyword? q)
                (if-let [v (get-in db [ref q])]
                  (assoc! acc' q v)
                  (reduced nil))
                (map? q)
                (if-let [m (not-empty (-pick db q ref))]
                  (reduce conj! acc' m)
                  (reduced nil))))
            (transient {})
            query))))

       (map? query)
       (ex/loop-it [[k query'] query :let [acc acc]]
         (recur
          (if-let [v (get-in db [ref k])]
            (cond
              (u/-ref-lookup? v)
              (-pick db query' acc v)

              (u/-ref-lookups? v)
              (reduce (fn [acc ref] (-pick db query' acc ref)) acc v))
            acc))
         acc)))))
