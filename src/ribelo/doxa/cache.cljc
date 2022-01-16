(ns ribelo.doxa.cache
  (:require
   [ribelo.doxa.util :as u])
  #?(:clj
     (:import
      [java.util.concurrent CountDownLatch])))

(defprotocol IDoxaCache
  (-hit [_ args])
  (-miss  [_ args f] [_ args f fresh?])
  (-evict [_ args])
  (-gc-now? [_])
  (-run-gc [_]))

(deftype TickedCacheEntry [delay ^long udt ^long tick-lru ^long tick-lfu])

(deftype DoxaCache [cache_ latch_ tick_ ^long cache-size ^long ttl-ms]
  IDoxaCache
  (-hit [_ args]
    (u/-get @cache_ args))

  (-miss [this args f]
    (-miss this args f false))
  (-miss [_ args f fresh?]
    (let [tick (swap! tick_ inc)
          instant (u/-instant)]
      (swap! cache_
        (fn [^clojure.lang.IPersistentMap m]
          #?(:clj (when-let [latch @latch_] (.await ^CountDownLatch latch)))
          (let [^TickedCacheEntry ?e (u/-get m args)]
            (if (or (nil? ?e) fresh? (> (- instant (.-udt ^TickedCacheEntry ?e)) ttl-ms))
              (assoc m args (TickedCacheEntry. (delay (u/-apply f args)) instant tick 1))
              (assoc m args (TickedCacheEntry. (.-delay ?e) (.-udt ?e) tick (.-tick-lfu ?e)))))))))

  (-gc-now? [_]
    #?(:clj  (<= (java.lang.Math/random) ~(/ 1.0 16000))
       :cljs (<=       (.random js/Math) ~(/ 1.0 16000))))

  (-run-gc [_]
    (let [gc-now? #?(:clj  (<= (java.lang.Math/random) ~(/ 1.0 16000))
                     :cljs (<= (.random js/Math) ~(/ 1.0 16000)))]
      (when gc-now?
        (let [instant (u/-instant)
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
            #?(:clj (reset! latch_ nil))))))))

(defn cache
  ([] (cache {}))
  ([{:keys [cache-size ttl-ms]
     :or {cache-size 1024
          ttl-ms 0}}]
   (DoxaCache. (atom {}) (atom nil) (atom 0) cache-size ttl-ms)))
