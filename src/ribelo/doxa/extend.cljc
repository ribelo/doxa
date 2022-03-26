(ns ribelo.doxa.extend
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.protocols :as p]
   [ribelo.doxa.util :as u]))

(extend-protocol p/IDoxa
  #?@(:clj
      [clojure.lang.IPersistentMap
       (p/-put
        ([this e m]
         (let [ndb (assoc this e m)
               oe (this e {})
               ne (ndb e {})
               diff (u/-diff-entity oe ne)]
           (vary-meta ndb update :ribelo.doxa/tx into diff))))
       (p/-del
        ([this e]
         (let [ndb (dissoc this e)
               oe (this e {})
               diff (u/-diff-entity oe {})]
           (vary-meta ndb update :ribelo.doxa/tx into diff))))

       (p/-tx [this] (if-let [meta (meta this)] (meta :ribelo.doxa/tx) []))
       (p/-clear-tx [this] (vary-meta this assoc :ribelo.doxa/tx []))
       (p/-cache [this] (some-> this meta :ribelo.doxa/cache))
       (p/-set-cache! [this cache] (let [cache_ (some-> this meta :ribelo.doxa/cache)] (when cache_ (reset! cache_ cache)) this))
       (p/-index [this] (get (meta this) :ribelo.doxa/index {}))
       (p/-reindex [this] (vary-meta this assoc :ribelo.doxa/index (u/-update-index (p/-index this) (p/-tx this))))
       (p/-make-index [this] (vary-meta this assoc :ribelo.doxa/index (u/-make-index this)))
       (p/-connect [this] (atom this))]

      :cljs
      [PersistentArrayMap
       (p/-put
        ([this e m]
         (let [ndb (assoc this e m)
               oe (this e {})
               ne (ndb e {})
               diff (u/-diff-entity oe ne)]
           (vary-meta ndb update :ribelo.doxa/tx into diff))))
       (p/-del
        ([this e]
         (let [ndb (dissoc this e)
               oe (get this e {})
               diff (u/-diff-entity oe {})]
           (vary-meta ndb update :ribelo.doxa/tx into diff))))

       (p/-tx [this] (if-let [meta (meta this)] (meta :ribelo.doxa/tx) []))
       (p/-clear-tx [this] (vary-meta this assoc :ribelo.doxa/tx []))
       (p/-cache [this] (some-> this meta :ribelo.doxa/cache))
       (p/-set-cache! [this cache] (let [cache_ (some-> this meta :ribelo.doxa/cache)] (when cache_ (reset! cache_ cache)) this))
       (p/-index [this] (get (meta this) :ribelo.doxa/index {}))
       (p/-reindex [this] (vary-meta this assoc :ribelo.doxa/index (u/-update-index (p/-index this) (p/-tx this))))
       (p/-make-index [this] (vary-meta this assoc :ribelo.doxa/index (u/-make-index this)))
       (p/-connect [this] (atom this))

       PersistentHashMap
       (p/-put
        ([this e m]
         (let [ndb (assoc this e m)
               oe (this e {})
               ne (ndb e {})
               diff (u/-diff-entity oe ne)]
           (vary-meta ndb update :ribelo.doxa/tx into diff))))
       (p/-del
        ([this e]
         (let [ndb (dissoc this e)
               oe (get this e {})
               diff (u/-diff-entity oe {})]
           (vary-meta ndb update :ribelo.doxa/tx into diff))))

       (p/-tx [this] (if-let [meta (meta this)] (meta :ribelo.doxa/tx) []))
       (p/-clear-tx [this] (vary-meta this assoc :ribelo.doxa/tx []))
       (p/-cache [this] (some-> this meta :ribelo.doxa/cache))
       (p/-set-cache! [this cache] (let [cache_ (some-> this meta :ribelo.doxa/cache)] (when cache_ (reset! cache_ cache)) this))
       (p/-index [this] (get (meta this) :ribelo.doxa/index {}))
       (p/-reindex [this] (vary-meta this assoc :ribelo.doxa/index (u/-update-index (p/-index this) (p/-tx this))))
       (p/-make-index [this] (vary-meta this assoc :ribelo.doxa/index (u/-make-index this)))
       (p/-connect [this] (atom this))]))

(extend-type nil
  p/IDoxa
  (-del
    ([this _] this))

  p/IDoxaCache
  (-refresh [this _] this))
