(ns ribelo.doxa.protocols
  (:require
   [ribelo.extropy :as ex]))

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
  (-reindex       [_        ])
  (-make-index    [_        ]))

(defprotocol IDoxaCache
  (-has? [_ k])
  (-hit [_ k])
  (-evict [_ k])
  (-miss [_ k item])
  (-gc-now? [_])
  (-run-gc [_])
  (-refresh [_ changes]))
