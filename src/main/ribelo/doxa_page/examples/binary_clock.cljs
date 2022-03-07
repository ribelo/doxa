(ns ribelo.doxa-page.examples.binary-clock
  (:require
   [reagent.core :as r]
   [re-frame.core :as rf]
   [ribelo.doxa :as dx]))

(rf/reg-event-fx
  ::update-clock
  (fn [_ [_ time]]
    {:commit [:dx/put [:app/id :binary-clock] :time time]}))

(rf/reg-sub
  ::time
  (fn [db _]
    (dx/pick db :time [:app/id :binary-clock])))

(defn start-clock! []
  (let [id (js/setInterval #(rf/dispatch [::update-clock (js/Date.)]) 1000)]
    (fn []
      (js/clearInterval id))))


;; stolen from
;; https://github.com/funcool/rumext/blob/master/examples/rumext/examples/binary_clock.cljs
(defn bit
  [{:keys [n b] :as props}]
  (if (bit-test n b)
    [:td {:class [:bg-nord-1 :w-6 :h-6]}]
    [:td {:class [:w-6 :w-6]}]))

(defn view
  []
  (let [ts @(rf/subscribe [::time])]
    (r/with-let [dispose! (start-clock!)]
      (let [msec (mod ts 1000)
            sec  (mod (quot ts 1000) 60)
            min  (mod (quot ts 60000) 60)
            hour (mod (quot ts 3600000) 24)
            hh   (quot hour 10)
            hl   (mod  hour 10)
            mh   (quot min 10)
            ml   (mod  min 10)
            sh   (quot sec 10)
            sl   (mod  sec 10)
            msh  (quot msec 100)
            msm  (->   msec (quot 10) (mod 10))
            msl  (mod  msec 10)]
        [:div {:class [:flex :justify-center :items-center :w-full :h-full]}
         [:table
          [:tbody
           [:tr {:class [:h-6]}
            [:td] [bit {:n hl :b 3}] [:th]
            [:td] [bit {:n ml :b 3}] [:th]
            [:td] [bit {:n sl :b 3}] [:th]
            [bit {:n msh :b 3}]
            [bit {:n msm :b 3}]
            [bit {:n msl :b 3}]]
           [:tr
            [:td] [bit {:n hl :b 2}] [:th]
            [bit {:n mh :b 2}]
            [bit {:n ml :b 2}] [:th]
            [bit {:n sh :b 2}]
            [bit {:n sl :b 2}] [:th]
            [bit {:n msh :b 2}]
            [bit {:n msm :b 2}]
            [bit {:n msl :b 2}]]
           [:tr
            [bit {:n hh :b 1}]
            [bit {:n hl :b 1}] [:th]
            [bit {:n mh :b 1}]
            [bit {:n ml :b 1}] [:th]
            [bit {:n sh :b 1}]
            [bit {:n sl :b 1}] [:th]
            [bit {:n msh :b 1}]
            [bit {:n msm :b 1}]
            [bit {:n msl :b 1}]]
           [:tr
            [bit {:n hh :b 0}]
            [bit {:n hl :b 0}] [:th]
            [bit {:n mh :b 0}]
            [bit {:n ml :b 0}] [:th]
            [bit {:n sh :b 0}]
            [bit {:n sl :b 0}] [:th]
            [bit {:n msh :b 0}]
            [bit {:n msm :b 0}]
            [bit {:n msl :b 0}]]
           [:tr
            [:th hh]
            [:th hl]
            [:th]
            [:th mh]
            [:th ml]
            [:th]
            [:th sh]
            [:th sl]
            [:th]
            [:th msh]
            [:th msm]
            [:th msl]]]]])
      (finally (dispose!)))))
