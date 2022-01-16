(ns ribelo.doxa.impl.map
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa.util :as u]
   [ribelo.doxa.impl.protocols :as p]
   [ribelo.doxa.index :as idx])
  (:import
   (java.util Map)))

(set! *warn-on-reflection* true)

(deftype Doxa [db index tx cache max-cache-size ttl-ms listeners_ meta_]
  Object
  (equals [_ obj] (.equals db obj))
  (hashCode [_] (.hashCode db))
  (toString [_] (.toString db))

  Map
  clojure.lang.Counted
  Iterable
  (iterator [_] (.iterator ^Iterable db))

  clojure.lang.Seqable
  (seq [_] (.seq ^clojure.lang.Seqable db))

  clojure.lang.IPersistentCollection
  (count [_] (.count ^clojure.lang.IPersistentCollection db))
  (equiv [_ o] (.equiv ^clojure.lang.IPersistentCollection db o))

  clojure.lang.ILookup
  (valAt [_ o] (.valAt ^clojure.lang.ILookup db o))
  (valAt [_ o not-found] (.valAt ^clojure.lang.ILookup db o not-found))

  clojure.lang.Associative
  (assoc [_ _ _]
      (throw (ex-info "not suported" {})))

  clojure.lang.IReference
  (meta [_] @meta_)
  (alterMeta [_ f args]
    (swap! meta_ f args))
  (resetMeta [_ metadata-map]
    (reset! meta_ metadata-map))

  clojure.lang.IReduce
  (reduce [_ f]
    (reduce f db))

  clojure.lang.IReduceInit
  (reduce [_ f v]
    (reduce f v db))

  clojure.lang.IKVReduce
  (kvreduce [_ f init]
    (reduce-kv f init db))

  p/IDoxa
  (p/-put [_ e m]
    (let [ndb (assoc db e m)
          oe (ex/-get db e {})
          ne (ex/-get ndb e {})
          diff (u/-diff-entity oe ne)
          index' (idx/-update-index index diff)]
      (Doxa. ndb index' (into tx diff) cache max-cache-size ttl-ms listeners_ meta_)))

  (p/-put [this e a v]
    (p/-put this e (p/-put (or (p/-pick this e) {}) a v)))

  (p/-pick [_ e]
    (.valAt ^clojure.lang.ILookup db e))
  (p/-pick [this e a]
    (p/-pick (p/-pick this e) a))
  (p/-pick [this e a v]
    (p/-pick (p/-pick this e a) v))

  (p/-del [_ e]
    (let [ndb (dissoc db e e)
          oe (ex/-get db e {})
          diff (u/-diff-entity oe {})
          index' (idx/-update-index index diff)]
      (Doxa. ndb index' (into tx diff) cache max-cache-size ttl-ms listeners_ meta_)))
  (p/-del [this e a]
    (p/-put this e (p/-del (p/-pick this e) a)))
  (p/-del [this e a v]
    (if-let [x (p/-pick this e a)]
      (if (= x v)
        (p/-del this e a)
        (if (coll? x)
          (p/-put this e a (p/-del x v))
          this))
      this))

  (p/-connect [this] (atom this))

  (p/-listen! [_ k f]
    (swap! listeners_ assoc k f)
    (fn [] (dissoc @listeners_ k)))

  ;; (p/-unlisten! [_ k]
  ;;             (dissoc @listeners_ k))

  (p/-tx [_] tx)

  (p/-clear-tx [_]
    (Doxa. db index [] cache max-cache-size ttl-ms listeners_ meta_))

  ;; (p/-keys [_] (keys db))

  ;; (p/-entities [_] (vals db))

  (p/-index [_] index))

(defn empty-db
  ([] (empty-db {}))
  ([{:keys [cache max-cache-size ttl-ms]
      :or {max-cache-size 128}}]
   (Doxa. {} {} [] cache max-cache-size ttl-ms (atom {}) (atom {}))))
