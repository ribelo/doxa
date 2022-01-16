(ns ribelo.doxa.pull
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa.impl.protocols :as p]
   [ribelo.doxa.util :as u]))


(declare pull)

(defn -pull
  ([dx query]
   (-pull dx query nil {}))
  ([dx query parent]
   (-pull dx query parent {}))
  ([dx query parent {:keys [lazy?] :as opts}]
   (let [recur-pull (if lazy? pull -pull)]
     (cond
       (u/-probably-ref-lookups? parent)
       (ex/-mapv #(recur-pull dx query %) parent)

       (= [:*] query)
       (p/-pick dx parent)

       (not (u/-ref-lookup? parent))
       (ex/-mapv #(recur-pull dx query %) (u/-eid-search dx parent))

       :else
       (let [qit (ex/-iter query)]
         (loop [r {} ref parent]
           (if (.hasNext qit)
             (let [elem (.next qit)]
               (cond
                 (and (map? elem) (u/-ref-lookup? (ex/-first-key elem)))
                 (recur (recur-pull (ex/-first-val elem) (ex/-first-key elem) nil opts) ref)

                 (and ref (#{:*} elem))
                 (recur (u/-flatten-map (p/-pick dx ref)) ref)

                 :else
                 (let [k (u/-rev->key elem)]
                   (cond
                     ;; (some? k)
                     ;; (if-let [paths (seq (-reverse-search dx k ref))]
                     ;;   (if (= 1 (count paths))
                     ;;     (recur (assoc r elem (ex/-ffirst paths)) ref)
                     ;;     (recur (assoc r elem (mapv ex/-first paths)) ref))
                     ;;   (recur r ref))

                     (and ref (keyword? elem) (not k))
                     (let [v (p/-pick dx ref elem)
                           one? (some-> (meta v) ::one?)
                           v' (if one? (nth v 0) v)]
                       (recur (ex/-assoc-some r elem v') ref))

                     (and ref (and (ex/-not-empty elem) (ex/-first-val elem) (ex/-every? keyword? (ex/-first-val elem))))
                     (let [k (ex/-first-key elem)
                           xs (ex/-first-val elem)]
                       (persistent!
                         (reduce
                           (fn [acc k']
                             (if-let [rk (u/-rev->key k)]
                               (if-let [paths nil #_(seq (-reverse-search dx rk ref))]
                                 (if (= 1 (count paths))
                                   (assoc! acc elem (nth paths 0))
                                   (assoc! acc elem paths))
                                 acc)
                               (if-let [v (p/-pick dx ref k')]
                                 (assoc! acc k' v))))
                           (transient {})
                           xs)))

                     (and ref (map? elem))
                     (let [k (ex/-first-key elem)
                           ?rk (u/-rev->key k)
                           ref' (if-not ?rk
                                  (p/-pick dx parent k)
                                  (p/-pick dx [:person/id 1])
                                  ;; (mapv ex/-first (-reverse-search dx ?rk ref))
                                  )
                           one? (some-> (meta ref') ::one?)
                           ref' (if one? (nth ref' 0) ref')]
                       (cond
                         (and ref (or one? (u/-ref-lookup? ref')))
                         (recur (ex/-assoc-some r k (recur-pull dx (ex/-first-val elem) ref' opts)) ref)

                         (and ref (u/-probably-ref-lookups? ref') (not ?rk))
                         (ex/-assoc-some
                           r (ex/-first-key elem)
                           (ex/-not-empty
                             (persistent!
                               (reduce
                                 (fn [acc ref]
                                   (let [k (nth ref 0)
                                         v (ex/-first-val elem)]
                                     (cond
                                       (map? v)
                                       (if-let [q (ex/-get v k)]
                                         (conj! acc (recur-pull dx q ref opts))
                                         acc)

                                       (vector? v)
                                       (if-let [r (not-empty (recur-pull dx v ref' opts))]
                                         (conj! acc r)
                                         acc))))
                                 (transient [])
                                 ref'))))

                         (and ref (u/-probably-ref-lookups? ref') ?rk)
                         (let [xs (ex/-mapv (fn [ref] (recur-pull dx (ex/-first-val elem) ref opts)) ref')
                               n (count xs)]
                           (recur
                             (ex/-assoc-some
                               r (ex/-first-key elem)
                               (cond
                                 (> n 1)
                                 (ex/-filterv not-empty xs)

                                 (= n 1)
                                 (ex/-not-empty (nth xs 0))))
                             ref))

                         ref
                         (recur r ref)))))))
             r)))))))

(defn pull
  ([dx query]
   (pull dx query nil))
  ([dx query parent]
   (pull dx query parent {}))
  ([dx query parent {:keys [lazy?] :as opts}]
   (let [cache_ (delay (-pull dx query parent opts))]
     (if lazy?
       #?(:clj
          (reify
            clojure.lang.ILookup
            (valAt [_ k]
              #?(:clj  (.valAt ^clojure.lang.ILookup @cache_ k)
                 :cljs (.get @cache_ k)))

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
              (postwalk (fn [x] (if (satisfies? IDeref x) @x x)) @cache_))))
       @cache_))))
