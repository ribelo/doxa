(ns ribelo.doxa.pull-api
  (:require
   [clojure.walk :as walk]
   [ribelo.extropy :as ex]
   [ribelo.doxa.protocols :as p]
   [ribelo.doxa.util :as u])
  #?(:clj
     (:import
      (ribelo.doxa.util CachedResult))))

(declare -pull)

(defn --pull
  ([db query]
   (--pull db query nil {}))
  ([db query parent]
   (--pull db query parent {}))
  ([db query parent {:keys [lazy?] :as opts}]
   (let [recur-pull (if lazy? -pull --pull)]
     (cond
       (ex/-every? u/-ref-lookup? parent)
       (mapv #(recur-pull db query %) parent)

       (= [:*] query)
       (when-let [m (db parent)]
         (persistent!
           (reduce-kv
             (fn [acc k v] (if (not (u/-rvd->key k)) (assoc! acc k v) acc))
             (transient {})
             m)))

       (not (u/-ref-lookup? parent))
       (mapv #(recur-pull db query %) (u/-eid-search db parent))

       :else
       (let [qit (ex/-iter query)]
         (loop [r {} ref parent]
           (if (.hasNext qit)
             (let [elem (.next qit)]
               (cond
                 (and (map? elem) (u/-ref-lookup? (ex/-first-key elem)))
                 (recur (recur-pull (ex/-first-val elem) (ex/-first-key elem) nil opts) ref)

                 (and ref (#{:*} elem))
                 (recur (u/-flatten-map (db ref)) ref)

                 :else
                 (let [k (u/-rvd->key elem)]
                   (cond
                     (some? k)
                     (let [x (get-in db [ref elem])]
                       (if (or (u/-ref-lookup? x) (u/-ref-lookups? x))
                         (recur (assoc r elem x) ref)
                         (recur r ref)))

                     (and ref (keyword? elem) (not k))
                     (let [v (get-in db [ref elem])
                           one? (some-> (meta v) ::one?)
                           v' (if one? (nth v 0) v)]
                       (recur (ex/-assoc-some r elem v') ref))

                     (and ref (map? elem))
                     (let [k (ex/-first-key elem)
                           ?rk (u/-rvd->key k)
                           ref' (if-not ?rk
                                  (get-in db [parent k])
                                  (get-in db [ref k]))
                           one? (some-> (meta ref') ::one?)
                           ref' (if one? (nth ref' 0) ref')]
                       (cond
                         (and ref (or one? (u/-ref-lookup? ref')))
                         (recur (ex/-assoc-some r k (recur-pull db (ex/-first-val elem) ref' opts)) ref)

                         (and ref (u/-ref-lookups? ref') (not ?rk))
                         (recur
                          (ex/-assoc-some
                           r (ex/-first-key elem)
                           (seq
                            (persistent!
                             (reduce
                              (fn [acc x]
                                (let [k (nth x 0)
                                      v (ex/-first-val elem)]
                                  (cond
                                    (map? v)
                                    (if-let [q (get v k)]
                                      (conj! acc (recur-pull db q x opts))
                                      acc)

                                    (u/-ref-lookup? x)
                                    (if-let [r (not-empty (recur-pull db v x opts))]
                                      (conj! acc r)
                                      acc))))
                              (transient [])
                              ref'))))
                          ref)

                         (and ref (u/-ref-lookups? ref') ?rk)
                         (let [xs (mapv (fn [ref] (recur-pull db (ex/-first-val elem) ref opts)) ref')
                               n (count xs)]
                           (recur
                            (ex/-assoc-some
                             r (ex/-first-key elem)
                             (cond
                               (> n 1)
                               (ex/-filter seq xs)

                               (= n 1)
                               (seq (nth xs 0))))
                            ref))

                         ref
                         (recur r ref)))))))
             r)))))))

(defn -pull
  ([db query]
   (-pull db query nil))
  ([db query parent]
   (-pull db query parent {}))
  ([db query parent {:keys [lazy?] :as opts}]
   (let [cache_ (delay (--pull db query parent opts))]
     (if lazy?
       #?(:clj
          (reify
            clojure.lang.ILookup
            (valAt [_ k]
              (get @cache_ k))

            clojure.lang.IDeref
            (deref [_]
              (walk/postwalk (fn [x] (if (instance? clojure.lang.IDeref x) @x x)) @cache_)))

          :cljs
          (reify
            ILookup
            (-lookup [_ k]
              (.get @cache_ k))

            IDeref
            (-deref [_]
              (walk/postwalk (fn [x] (if (satisfies? IDeref x) @x x)) @cache_))))
       @cache_))))

(def -pull->datalog
  (ex/-memoize
    (fn
      ([query id] (persistent! (-pull->datalog (transient #{}) id query)))
      ([acc id query]
       (ex/-loop [q query :let [acc acc]]
         (cond
           (map-entry? q)
           (recur (-pull->datalog (conj! acc [id (ex/-k q) '_]) id (ex/-v q)))
           (vector? q)
           (recur (mapv (partial -pull->datalog acc id) q))
           (keyword? q)
           (recur (conj! acc [id q '_]))
           (map? q)
           (recur (-pull->datalog acc '_ q)))
         acc)))))

(defn -mpull [db query parent]
  (let [k [query parent]
        cache_ (p/-cache db)
        where (-pull->datalog query parent)]
    (if (p/-has? @cache_ k)
      (let [cache' (p/-hit @cache_ k)
            item (get cache' k)]
        (p/-set-cache! db cache')
        @(.-delay ^CachedResult item))
      (let [d (delay (-pull db query parent))
            item #?(:clj  (CachedResult. d where)
                    :cljs (u/CachedResult. d where))
            cache' (p/-miss @cache_ k item)]
        (p/-set-cache! db cache')
        @d))))
