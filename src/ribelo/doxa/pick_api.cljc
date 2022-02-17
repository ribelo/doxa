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
         (ex/-ensure-persisten!
           (ex/-reduce
             (fn [acc' q]
               (cond
                 (keyword? q)
                 (if-let [v (ex/-get-in db [ref q])]
                   (ex/-assoc!* acc' q v)
                   (reduced nil))
                 (map? q)
                 (if-let [m (ex/-not-empty (-pick db q ref))]
                   (ex/-merge! acc' m)
                   (reduced nil))))
             (transient {})
             query))))

     (map? query)
     (ex/-loop [me query :let [acc acc]]
       (let [k (ex/-k* me)
             query' (ex/-v* me)]
         (recur
           (if-let [v (ex/-get-in db [ref k])]
             (cond
               (u/-ref-lookup? v)
               (-pick db query' acc v)

               (u/-ref-lookups? v)
               (ex/-reduce (fn [acc ref] (-pick db query' acc ref)) acc v))
             acc)))
       acc))))
