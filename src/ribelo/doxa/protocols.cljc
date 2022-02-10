(ns ribelo.doxa.protocols
  (:require
   [ribelo.extropy :as ex]))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IDoxa
  (-put           [_ e     m])
  (-del           [_ e      ] [_ e a  ] [_ e a v])
  (-connect       [_        ] [_ uri  ])
  (-listen!       [_ k     f])
  (-unlisten!     [_ k      ])
  (-tx            [_        ])
  (-clear-tx      [_        ])
  (-cache         [_        ])
  (-set-cache!    [_   cache])
  (-index         [_        ])
  (-reindex       [_        ]))

(defprotocol IDoxaCache
  (-has? [_ k])
  (-hit [_ k])
  (-evict [_ k])
  (-miss [_ k item])
  (-gc-now? [_])
  (-run-gc [_])
  (-refresh [_ changes]))

(extend-type nil
  IDoxa
  (-del
    ([this e] this)
    ([this e a] this)
    ([this e a v] this))

  IDoxaCache
  (-refresh [this _] this))

(extend-protocol IDoxa
  #?@(:clj
      [clojure.lang.IPersistentMap
       (-put
        ([this e m]
         (ex/-assoc* this e m)))
       (-del
        ([this e]
         (ex/-dissoc* this e)))

       (-tx [_] nil)
       (-clear-tx [this] this)
       (-cache [_] nil)
       (-set-cache! [this _] this)
       (-index [this] nil)
       (-reindex [this] this)

       (-connect [this _] (atom this))]

      :cljs
      [PersistentArrayMap
       (-put
        ([this e m]
         (assoc this e m)))

       (-del
        ([this e]
         (dissoc this e)))

       (-connect [this] (atom this))
       (-tx [_] nil)
       (-clear-tx [this] this)
       (-cache [_] nil)
       (-set-cache! [this _] this)
       (-index [this] nil)
       (-reindex [this] this)

       PersistentHashMap
       (-put
        ([this e m]
         (ex/-assoc* this e m)))

       (-del
        ([this e]
         (ex/-dissoc* this e)))

       (-connect [this] (atom this))
       (-tx [_] nil)
       (-clear-tx [this] this)
       (-cache [_] nil)
       (-set-cache! [this _] this)
       (-index [this] nil)
       (-reindex [this] this)]))
