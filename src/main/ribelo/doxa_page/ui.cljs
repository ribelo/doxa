(ns ribelo.doxa-page.ui
  (:require
   [re-frame.core :as rf]
   [ribelo.doxa :as dx]
   [ribelo.doxa-page.events :as evt]
   [ribelo.doxa-page.examples.worldcities :as world]))

(defn entry [{:keys [title on-click]}]
  [:div {:class [:flex :flex-col]}
   [:div {:class [:my-2 :px-4 :cursor-pointer :text-nord-5 :hover:bg-nord-2]
          :on-click (fn [_] 
                      (when on-click (on-click))
                      (rf/dispatch [::evt/change-screen title]))}
    (name title)]])

(defn sidebar []
  [:div {:class [:flex :flex-col :w-36 :h-full :bg-nord-1]}
   [:div {:class [:m-4 :text-nord-5]}
    "examples"]
   (for [elem [{:title "transactions"}
               {:title "world cities" :on-click #(rf/dispatch [::world/download-cities])}
               {:title "binary clock"}]]
     [entry elem])])
