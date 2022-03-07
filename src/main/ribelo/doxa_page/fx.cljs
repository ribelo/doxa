(ns ribelo.doxa-page.fx
  (:require
   [re-frame.core :as rf]
   [re-frame.db :refer [app-db]]
   [ribelo.doxa :as dx]))

(rf/reg-fx
  :commit
  (fn [data]
    (dx/commit! app-db data)))
