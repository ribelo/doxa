(ns ribelo.doxa.cache
  (:require
   [ribelo.extropy :as ex]
   [ribelo.doxa.protocols :as p]
   [ribelo.doxa.util :as u])
  #?(:clj
     (:import
      [ribelo.doxa.util CachedResult])))

(deftype TickedCacheEntry [item ^long udt ^long tick-lru ^long tick-lfu])

(declare -refresh-cache)

(deftype DoxaCache [m ^long tick ^long cache-size ^long ttl-ms]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [_ k] (.-item ^TickedCacheEntry (get m k)))

       clojure.lang.IFn
       (invoke [_ k] (.-item ^TickedCacheEntry (get m k)))

       clojure.lang.IPersistentCollection
       (empty [_] (DoxaCache. {} 0 cache-size ttl-ms))]

      :cljs
      [ILookup
       (-lookup [_ k] (.-item (get m k)))

       IFn
       (-invoke [_ k] (.-item (get m k)))

       IEmptyableCollection
       (-empty [_] (DoxaCache. {} 0 cache-size ttl-ms))])

  p/IDoxaCache
  (p/-has? [this k]
    (let [instant (ex/now-udt)]
      (when-let [^TickedCacheEntry ?e (get m k)]
        (if (or (zero? ttl-ms) (< (- instant (.-udt ?e)) ttl-ms))
          true
          (not (boolean (p/-evict this k)))))))

  (p/-hit [_ args]
    (let [^TickedCacheEntry ?e (get m args)
          tick' (inc tick)
          m' (assoc m args (TickedCacheEntry. (.-item ?e) (.-udt ?e) (.-tick-lru ?e) (inc (.-tick-lfu ?e))))]
      (DoxaCache. m' tick' cache-size ttl-ms)))

  (p/-evict [this args]
    (p/-run-gc this)
    (DoxaCache. (dissoc m args) tick cache-size ttl-ms))

  (p/-miss [this k result]
    (let [this' (p/-run-gc this)
          tick' (inc tick)
          instant (ex/now-udt)
          m' (assoc (.-m ^DoxaCache this') k (TickedCacheEntry. result instant tick' 1))]
      (DoxaCache. m' tick' cache-size ttl-ms)))

  (p/-gc-now? [_]
    #?(:clj  (<= (java.lang.Math/random) (/ 1.0 16000))
       :cljs (<=       (.random js/Math) (/ 1.0 16000))))

  (p/-run-gc [this]
    (if (p/-gc-now? this)
      (let [instant (ex/now-udt)
            m' (persistent!
                 (reduce-kv
                   (fn [acc k ^TickedCacheEntry e]
                     (if (and (pos? ttl-ms) (> (- instant (.-udt e)) ttl-ms))
                       (dissoc! acc k)
                       acc))
                   (transient (or m {}))
                   m))]
        (DoxaCache. m' tick cache-size ttl-ms))
      this))

  (p/-refresh [_ changes]
    (-refresh-cache m tick cache-size ttl-ms changes)))

(defn -refresh-cache [m tick cache-size ttl-ms changes]
  (let [m' (ex/loop-it [[k v] m :let [acc (transient m)]]
               (let [^TickedCacheEntry e v
                     datoms (.-datoms ^CachedResult (.-item e))]
                 (if (u/-datoms-match-changes? datoms changes)
                   (recur (dissoc! acc k))
                   (recur acc)))
               (persistent! acc))]
    (DoxaCache. m' tick cache-size ttl-ms)))

(defn doxa-cache
  ([] (doxa-cache {}))
  ([{:keys [cache-size ttl-ms]
     :or {cache-size 1024
          ttl-ms 0}}]
   (DoxaCache. {} 0 cache-size ttl-ms)))

#?(:clj
   (def transit-write-handlers
     {DoxaCache (fn [^DoxaCache cache] [(.-m cache) (.-tick cache) (.-cache-size cache) (.-ttl-ms cache)])})

   :cljs
   (def transit-write-handlers
     {DoxaCache
      (reify Object
        (tag [_ _] "dx/cache")
        (rep [_ ^js cache] (js/Array. (.-m cache) (.-tick cache) (.-cache-size cache) (.-ttl-ms cache)))
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))

      TickedCacheEntry
      (reify Object
        (tag [_ _] "dx/tce")
        (rep [_ ^js entry] (js/Array. (.-item entry) (.-udt entry) (.-tick-lru entry) (.-tick-lfu entry)))
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))}))

(def transit-read-handler
  {"dx/cache" (fn [[m tick cache-size ttl-ms]]   (DoxaCache. m tick cache-size ttl-ms))
   "dx/tce"   (fn [[item udt tick-lru tick-lfu]] (TickedCacheEntry. item udt tick-lru tick-lfu))})

(comment

  (require '[cognitect.transit :as t])


  (defn -write-transit [s]
    (t/write (t/writer :json {:handlers transit-write-handler}) s))

  (defn -read-transit [s]
    (t/read (t/reader :json {:handlers transit-read-handler}) s))

  (-read-transit (-write-transit (doxa-cache))))
