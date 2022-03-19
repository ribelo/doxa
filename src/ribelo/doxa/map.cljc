(ns ribelo.doxa.map
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.util :as u]
   [ribelo.doxa.protocols :as p])
  #?(:clj
     (:import
      (java.util Map))))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (deftype Doxa [db cache_ index tx listeners_]
     Object
     (equals [_ obj] (.equals db obj))
     (hashCode [_] (.hashCode db))
     (toString [_] (.toString db))

     Map
     Iterable
     (iterator [_] (.iterator ^Iterable db))

     clojure.lang.Counted

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

     clojure.lang.IReduce
     (reduce [_ f]
       (reduce f db))

     clojure.lang.IReduceInit
     (reduce [_ f v]
       (reduce f v db))

     clojure.lang.IKVReduce
     (kvreduce [_ f init]
       (reduce-kv f init db))

     clojure.lang.IFn
     (invoke [_ k]
       (.valAt ^clojure.lang.ILookup db k))
     (invoke [_ k not-found]
       (.valAt ^clojure.lang.ILookup db k not-found))

     p/IDoxa
     (p/-put [_ e m]
       (let [ndb (assoc db e m)
             oe (get db e {})
             ne (get ndb e {})
             diff (u/-diff-entity oe ne)]
         (Doxa. ndb cache_ index (into tx diff) listeners_)))

     (p/-del [_ e]
       (let [ndb (dissoc db e e)
             oe (get db e {})
             diff (u/-diff-entity oe {})]
         (Doxa. ndb cache_ index (into tx diff) listeners_)))

     (p/-connect [this] (atom this))

     (p/-listen! [_ k f]
       (swap! listeners_ assoc k f)
       (fn [] (dissoc @listeners_ k)))

     (p/-unlisten! [_ k]
       (dissoc @listeners_ k))

     (p/-tx [_] tx)

     (p/-clear-tx [_]
       (Doxa. db cache_ index [] listeners_))

     (p/-cache [_]
       cache_)

     (p/-set-cache! [this cache]
       (reset! cache_ cache)
       this)

     (p/-index [_] index)
     (p/-reindex [_]
       (Doxa. db cache_ (u/-update-index index tx) tx listeners_))))

#?(:cljs
   (deftype Doxa [db cache_ index tx listeners_]
     Object
     (equiv [_ other] (-equiv db other))
     (toString [_] (.toString db))
     (keys [_] (keys db))
     (vals [_] (vals db))

     IPrintWithWriter
     (-pr-writer [_ writer opts]
       (-pr-writer db writer opts))

     IWithMeta
     (-with-meta [this meta]
       (if (identical? meta (-meta db))
         this
         (Doxa. (-with-meta db meta) cache_ index tx listeners_)))
     IMeta
     (-meta [_]
       (-meta db))

     IMap
     ICounted
     (-count [_]
       (-count db))

     IIterable
     (-iterator [_]
       (-iterator db))

     ISeqable
     (-seq [_] (-seq db))

     IEquiv
     (-equiv [_ other]
       (-equiv db other))

     ILookup
     (-lookup [_ k]
       (-lookup db k))
     (-lookup [_ k not-found]
       (-lookup db k not-found))

     IAssociative
     (-contains-key? [_ k]
       (-contains-key? db k))
     (-assoc [_ _ _]
       (throw (ex-info "unsuported operation" {})))

     ISeq
     (-first [_]
       (-first db))
     (-rest [_]
       (-rest db))

     IReduce
     (-reduce [_ f ]
       (-reduce db f))
     (-reduce [_ f init]
       (-reduce db f init))

     IKVReduce
     (-kv-reduce [_ f init]
       (-kv-reduce db f init))

     IHash
     (-hash [_]
       (-hash db))

     IFn
     (-invoke [_ k]
       (-lookup db k))
     (-invoke [_ k not-found]
       (-lookup db k not-found))

     p/IDoxa
     (p/-put [_ e m]
       (let [ndb (assoc db e m)
             oe (get db e {})
             ne (get ndb e {})
             diff (u/-diff-entity oe ne)]
         (Doxa. ndb cache_ index (into tx diff) listeners_)))

     (p/-del [_ e]
       (let [ndb (dissoc db e e)
             oe (get db e {})
             diff (u/-diff-entity oe {})]
         (Doxa. ndb cache_ index (into tx diff) listeners_)))

     (p/-connect [this] (atom this))

     (p/-listen! [_ k f]
       (swap! listeners_ assoc k f)
       (fn [] (dissoc @listeners_ k)))

     (p/-unlisten! [_ k]
       (dissoc @listeners_ k))

     (p/-tx [_] tx)

     (p/-clear-tx [_]
       (Doxa. db cache_ index [] listeners_))

     (p/-cache [_]
       cache_)

     (p/-set-cache! [this cache]
       (reset! cache_ cache)
       this)

     (p/-index [_]
       index)
     (p/-reindex [_]
       (Doxa. db cache_ (u/-update-index index tx) tx listeners_))
     ))

(defn empty-db
  ([] (empty-db {}))
  ([{:keys [cache]}]
   (Doxa. {} (atom cache) {} [] (atom {}))))


#?(:cljs
   (def transit-writer-handler
     {Doxa
      (reify Object
        (tag [_ _] "dx/map")
        (rep [_ ^js dx] (js/Array (.-db dx) (empty @(.-cache_ dx)) (.-index dx)))
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))}))

(def -transit-read-handler
  {"dx/map" (fn [[m cache index]] (Doxa. m (atom cache) index [] (atom {})))})
