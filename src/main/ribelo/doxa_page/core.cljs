(ns ribelo.doxa-page.core
  (:require
   [reagent.core :as r]
   [reagent.ratom :as ra]
   [reagent.dom :as rd]
   [re-frame.core :as rf]
   [ribelo.doxa :as dx]
   [ribelo.doxa-page.fx]
   [ribelo.doxa-page.subs :as sub]
   [ribelo.doxa-page.ui :as ui]
   [ribelo.doxa-page.examples.worldcities :as worldcities]
   [ribelo.doxa-page.examples.binary-clock :as binary-clock]
   [ribelo.doxa-page.examples.transactions :as transactions]))

(defn view []
  (let [screen @(rf/subscribe [::sub/current-screen])]
    [:div {:class [:flex :flex-col :w-screen :h-screen]}
     [:div {:class [:flex :flex-row :w-full :h-full]}
      [ui/sidebar]
      [:div {:class [:flex :w-full :h-full]}
       (case screen
         (nil "transactions")
         [transactions/view]

         "binary clock"
         [binary-clock/view]

         "world cities"
         [worldcities/view]
         [:div "lost"])]]]))

(defn mount-components []
  (js/console.info "mount-components")
  (rd/render [#'view] (.getElementById js/document "app")))

(defn ^:export init []
  (js/console.info "init")
  ;; (rf/dispatch-sync [::evt/initialize-db])
  (mount-components))
