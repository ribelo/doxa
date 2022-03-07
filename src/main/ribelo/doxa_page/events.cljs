(ns ribelo.doxa-page.events
  (:require
   [re-frame.core :as rf]
   [ribelo.doxa :as dx]))

(rf/reg-event-fx
  ::change-screen
  (fn [_ [_ screen]]
    {:commit [:dx/put [:ui/id :main] :screen screen]}))
