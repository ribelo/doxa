(ns ribelo.doxa.cache
  (:require
   [ribelo.exin :as ex]
   [ribelo.doxa.impl.protocols :as p]
   [ribelo.doxa.util :as u])
  #?(:clj
     (:import
      [java.util.concurrent CountDownLatch])))

(deftype TickedCacheEntry [delay datoms ^long udt ^long tick-lru ^long tick-lfu])

(deftype DoxaCache [cache_ latch_ tick_ ^long cache-size ^long ttl-ms]
  p/IDoxaCache
  (p/-hit [_ args]
    (ex/-get* @cache_ args))
  (p/-miss [this args f datoms]
    (p/-miss this args f false))
  (p/-miss [_ args f datoms fresh?]
    (let [tick (swap! tick_ inc)
          instant (ex/-udt)]
      (swap! cache_
        (fn [^clojure.lang.IPersistentMap m]
          #?(:clj (when-let [latch @latch_] (.await ^CountDownLatch latch)))
          (let [^TickedCacheEntry ?e (ex/-get* m args)]
            (if (or (nil? ?e) fresh? (> (- instant (.-udt ^TickedCacheEntry ?e)) ttl-ms))
              (assoc m args (TickedCacheEntry. (delay (ex/-apply f args)) datoms instant tick 1))
              (assoc m args (TickedCacheEntry. (.-delay ?e) datoms (.-udt ?e) tick (.-tick-lfu ?e)))))))))

  (p/-gc-now? [_]
    #?(:clj  (<= (java.lang.Math/random) ~(/ 1.0 16000))
       :cljs (<=       (.random js/Math) ~(/ 1.0 16000))))

  (p/-run-gc [this]
    (when (p/-gc-now? this)
      (let [instant (ex/-udt)
            latch #?(:clj (CountDownLatch. 1) :cljs nil)]
        (when (compare-and-set! latch_ nil latch)
          (swap! cache_
            (fn [m]
              (persistent!
               (reduce-kv
                (fn [acc k ^TickedCacheEntry e]
                  (if (> (- instant (.-udt e)) ttl-ms)
                    (dissoc! acc k)
                    acc))
                (transient (or m {}))
                m))))
          #?(:clj (.countDown latch))
          #?(:clj (reset! latch_ nil)))))))

(defn cache
  ([] (cache {}))
  ([{:keys [cache-size ttl-ms]
     :or {cache-size 1024
          ttl-ms 0}}]
   (DoxaCache. (atom {}) (atom nil) (atom 0) cache-size ttl-ms)))
