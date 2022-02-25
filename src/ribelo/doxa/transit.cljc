(ns ribelo.doxa.transit
  (:require
   [ribelo.doxa.util :as u]
   [ribelo.doxa.ordered-set :as dxos]
   [ribelo.doxa.cache :as dxc]))

(def write-handlers
  (merge
    u/tranit-write-handlers
    dxos/transit-write-handlers
    dxc/transit-write-handlers))

(def read-handlers
  (merge
    u/transit-read-handlers
    dxos/transit-read-handlers
    dxc/transit-read-handler))
