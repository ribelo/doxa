(ns ribelo.doxa.impl.protocols)

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IDoxa
  (-put         [_ e  m] [_ e a v])
  (-pick        [_ e   ] [_ e a  ] [_ e a v])
  (-del         [_ e   ] [_ e a  ] [_ e a v])
  (-connect     [_     ] [_ uri  ])
  (-listen!     [_ k  f])
  (-unlisten!   [_ k   ])
  (-tx          [_     ])
  (-clear-tx    [_     ])
  (-cache       [_     ])
  (-keys        [_     ])
  (-entities    [_     ])
  (-index       [_     ]))

(defprotocol IDoxaCache
  (-tick [_ k lru lfu])
  (-has? [_ k])
  (-hit [_ k])
  (-miss [_ k f args datoms])
  (-refresh [_ changes])
  (-evict [_ args])
  (-gc-now? [_])
  (-run-gc [_]))

(extend-type nil
  IDoxa
  (-pick
    ([this e] this)
    ([this e a] this)
    ([this e a v] this))

  (-del
    ([this e] this)
    ([this e a] this)
    ([this e a v] this))

  (-keys [this] this))

(extend-protocol IDoxa
  #?@(:clj
      [clojure.lang.IPersistentMap
       (-put
         ([this e m]
          (assoc this e m))
         ([this e a v]
          (-put this e (-put (or (-pick this e) {}) a v))))

       (-pick
         ([this e]
          (.valAt this e))
         ([this e a]
          (-pick (.valAt this e) a))
         ([this e a v]
          (-pick (-pick this e a) v)))

       (-del
         ([this e]
          (dissoc this e))
         ([this e a]
          (-put this e (-del (-pick this e) a)))
         ([this e a v]
          (if-let [x (-pick this e a)]
            (if (= x v)
              (-del this e a)
              (if (coll? x)
                (-put this e a (-del x v))
                this))
            this)))

       (-connect [this _] (atom this))

       (-keys [this] (keys this))]

      :cljs
      [PersistentArrayMap
       (-put
         ([this e m]
          (assoc this e m))
         ([this e a v]
          (-put this e (-put (or (-pick this e) {}) a v))))

       (-pick
         ([this e]
          (.get this e))
         ([this e a]
          (-pick (-pick this e) a))
         ([this e a v]
          (-pick (-pick this e a) v)))

       (-del
         ([this e]
          (dissoc this e))
         ([this e a]
          (-put this e (-del (-pick this e) a)))
         ([this e a v]
          (if-let [x (-pick this e a)]
            (if (= x v)
              (-del this e a)
              (if (coll? x)
                (-put this e a (-del x v))
                this))
            this)))

       (-keys [this] (keys this))

       PersistentHashMap
       (-put
         ([this e m]
          (assoc this e m))
         ([this e a v]
          (-put this e (-put (or (-pick this e) {}) a v))))

       (-pick
         ([this e]
          (.get this e))
         ([this e a]
          (-pick (-pick this e) a))
         ([this e a v]
          (-pick (-pick this e a) v)))

       (-del
         ([this e]
          (dissoc this e))
         ([this e a]
          (-put this e (-del (-pick this e) a)))
         ([this e a v]
          (if-let [x (-pick this e a)]
            (if (= x v)
              (-del this e a)
              (if (coll? x)
                (-put this e a (-del x v))
                this))
            this)))

       (-keys [this] (keys this))]))

(extend-type #?(:clj clojure.lang.IPersistentVector :cljs PersistentVector)
  IDoxa
  (-pick [this v]
    (reduce (fn [_ x] (if (= x v) (reduced x) nil)) nil this))

  (-del [this v]
    (into [] (remove #{v}) this)))

(extend-type #?(:clj clojure.lang.IPersistentSet :cljs PersistentHashSet)
  IDoxa
  (-pick [this v]
    (reduce (fn [_ x] (if (= x v) (reduced x) nil)) nil this))

  (-del [this v]
    (into #{} (remove #{v}) this)))
