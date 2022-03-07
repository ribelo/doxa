(ns ribelo.doxa-page.ui
  (:require
   [re-frame.core :as rf]
   [ribelo.doxa :as dx]
   [ribelo.doxa-page.events :as evt]))

(defn entry [{:keys [title]}]
  [:div {:class [:flex :flex-col]}
   [:div {:class [:my-2 :px-4 :cursor-pointer :text-nord-5 :hover:bg-nord-2]
          :on-click #(rf/dispatch [::evt/change-screen title])}
    (name title)]])

(defn sidebar []
  [:div {:class [:flex :flex-col :w-36 :h-full :bg-nord-1]}
   [:div {:class [:m-4 :text-nord-5]}
    "expamples"]
   (for [elem [:binary-clock :transaction]]
     [entry {:title elem}])])
