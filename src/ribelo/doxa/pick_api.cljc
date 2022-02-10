(ns ribelo.doxa.pick-api
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.util :as u]))

(defn -pick
  ([db ref]
   (-pick db :* ref))
  ([db query ^clojure.lang.IPersistentVector ref]
   (let [xs (-pick db query (transient []) ref)]
     (when (ex/-not-empty xs)
       (if (= 1 (ex/-count* xs))
         (ex/-first (persistent! xs))
         (persistent! xs)))))
  ([db query ^clojure.lang.ITransientVector acc ^clojure.lang.IPersistentVector ref]
   (cond
     (= :* query)
     (ex/-conj-some! acc (ex/-get* db ref))

     (keyword? query)
     (ex/-conj-some! acc (ex/-get-in db [ref query]))

     (vector? query)
     (ex/-conj-some! acc
       (ex/-not-empty
         (persistent!
           (ex/-reduce
             (fn [acc' q]
               (cond
                 (keyword? q)
                 (ex/-assoc-some! acc' q (ex/-get-in db [ref q]))
                 (map? q)
                 (ex/-merge! acc' (-pick db q ref))))
             (transient {})
             query))))

     (map? query)
     (let [k (ex/-first-key query)
           query' (ex/-first-val query)]
       (if-let [v (ex/-get-in db [ref k])]
         (cond
           (u/-ref-lookup? v)
           (-pick db query' acc v)

           (u/-probably-ref-lookups? v)
           (ex/-reduce (fn [acc ref] (-pick db query' acc ref)) acc v))
         acc)))))
