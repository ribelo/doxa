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
  (p/-tick [_ args tick-lru tick-lfu]
    (swap! cache_
      (fn [m]
        (when-let [^TickedCacheEntry ?e (ex/-get* m args)]
          #?(:clj (when-let [latch @latch_] (.await ^CountDownLatch latch)))
          (assoc m args (TickedCacheEntry. (.-delay ?e) (.-datoms ?e) (.-udt ?e) tick-lru tick-lfu))))))

  (p/-has? [_ k]
    (let [instant (ex/-udt)]
      (when-let [^TickedCacheEntry ?e (ex/-get @cache_ k)]
        (or (zero? ttl-ms) (< (- instant (.-udt ?e)) ttl-ms)))))

  (p/-hit [this args]
    (p/-run-gc this)
    (let [^TickedCacheEntry ?e (ex/-get @cache_ args)
          tick (swap! tick_ inc)]
      (p/-tick this args tick (inc (.-tick-lfu ?e)))
      (.-delay ?e)))

  (p/-miss [this k f args datoms]
    (p/-run-gc this)
    (let [tick (swap! tick_ inc)
          instant (ex/-udt)
          d (delay (ex/-apply f args))]
      (swap! cache_
        (fn [m]
          #?(:clj (when-let [latch @latch_] (.await ^CountDownLatch latch)))
          (assoc m k (TickedCacheEntry. d datoms instant tick 1))))
      d))

  (p/-refresh [this changes]
    (println :changes changes)
    (p/-run-gc this)
    (swap! cache_
      (fn [m]
        (println :cache m)
        (ex/-loop [me m :let [acc (transient m)]]
          (let [k (ex/-k me)
                ^TickedCacheEntry e (ex/-v me)]
            (if-not (u/-datoms-match-changes? (.-datoms e) changes)
              (recur (dissoc! m k))
              (recur acc)))))))

  (p/-gc-now? [_]
    #?(:clj  (<= (java.lang.Math/random) (/ 1.0 16000))
       :cljs (<=       (.random js/Math) (/ 1.0 16000))))

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
                  (if (and (pos? ttl-ms) (> (- instant (.-udt e)) ttl-ms))
                    (dissoc! acc k)
                    acc))
                (transient (or m {}))
                m))))
          #?(:clj (.countDown latch))
          #?(:clj (reset! latch_ nil)))))))

(defn doxa-cache
  ([] (doxa-cache {}))
  ([{:keys [cache-size ttl-ms]
     :or {cache-size 1024
          ttl-ms 0}}]
   (DoxaCache. (atom {}) (atom nil) (atom 0) cache-size ttl-ms)))
