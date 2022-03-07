(ns ribelo.doxa-page.subs
  (:require
   [re-frame.core :as rf]
   [ribelo.doxa :as dx]))

(rf/reg-sub
  ::current-screen
  (fn [db _]
    (dx/pick db :screen [:ui/id :main])))
