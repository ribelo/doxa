(ns ribelo.doxa-page.examples.worldcities
  (:require
   [reagent.core :as r]
   [re-frame.core :as rf]
   [ribelo.extropy :as ex]
   [ribelo.doxa :as dx]
   [ribelo.doxa.cache :as dxc]))

(rf/reg-event-fx
 ::store-cities
 (fn [{:keys [db]} [_ data]]
   (tap> [::store-cities])
   {:db (assoc db [:app/id :world-cities]
               (dx/create-dx {} data {::dx/cache (atom (dxc/doxa-cache))}))}))

(defn download-and-store [{:keys [on-success]}]
  (-> (js/fetch "https://ribelo.github.io/doxa/resources/worldcities.csv")
      (.then (fn [^js resp]
               (.. resp text)))
      (.then (fn [^js csv]
               (into []
                     (comp
                      (map (fn [line]
                             (zipmap [:city :country]
                                     (-> line
                                         (.replaceAll "\"" "")
                                         (.split ",")
                                         (.slice 1 3)))))
                      (map-indexed (fn [i m] (assoc m :city/id i))))
                     (.split csv "\n"))))
      (.then (fn [data] (on-success data)))))

(rf/reg-event-fx
 ::download-cities
 (fn [{:keys [db]} _]
   (download-and-store {:on-success (fn [data] [:aa] (rf/dispatch [::store-cities data]))})
   {}))

(rf/reg-sub
 ::data
 (fn [db _]
   (get db [:app/id :world-cities])))

(rf/reg-sub
 ::filter-normal
 (fn [db [_ city]]
   (let [t0 (.getTime (js/Date.))
         data (get db [:app/id :world-cities])]
     (if (not-empty city)
       (let [rgx (str "(?iu)" city)
             r (dx/q '[:find ?id ?city ?country
                       :in ?x
                       :where
                       [?e :city/id ?id]
                       [?e :city ?city]
                       [(re-pattern ?x) ?rgx]
                       [(re-find ?rgx ?city)]
                       [?e :country ?country]]
                     data rgx)]
         [(into [] (take 100) r) (- (.getTime (js/Date.)) t0)])
       [[] (- (.getTime (js/Date.)) t0)]))))

(rf/reg-sub
 ::filter-materialized
 (fn [db [_ city]]
   (let [t0 (.getTime (js/Date.))
         data (get db [:app/id :world-cities])]
     (if (not-empty city)
       (let [rgx (str "(?iu)" city)
             r (dx/mq '[:find ?id ?city ?country
                        :in ?x
                        :where
                        [?e :city/id ?id]
                        [?e :city ?city]
                        [(re-pattern ?x) ?rgx]
                        [(re-find ?rgx ?city)]
                        [?e :country ?country]]
                      data rgx)]
         [(into [] (take 100) r) (- (.getTime (js/Date.)) t0)])
       [[] (- (.getTime (js/Date.)) t0)]))))

(comment
  @(rf/subscribe [::filter "aachen"]))

(defn query [k s]
  (let [query (r/atom "")]
    (fn [k]
      (let [[data ms] @(rf/subscribe [k @query])]
        [:div {:class [:flex :flex-col "w-1/3"]}
         [:div {:class [:flex :bg-nord-2 :text-nord-5 :px-2 :mx-4 :mb-4]}
          (str s " - " ms " ms")]
         [:input {:class [:border :border-nord-2 :px-2]
                  :default-value @query
                  :on-change (fn [^js e]
                               (reset! query (-> e .-target .-value)))
                  :placeholder "find city"}]
         [:div {:class [:flex-auto :overflow-auto]}
          (for [[id city country] data]
            ^{:key id}
            [:div (str id " - " city " - " country)])]]))))

(defn view []
  [:div {:class [:flex :flex-col :grow :m-12]}
   [:div "queries and pull requests can be cached and materialised. this makes
     each subsequent search instantaneous. it also makes it easier to use a
     pure reagent and, in the case of re-frames, allows subscriptions to be
     flattened."]
   [:div {:class [:flex :flex-row :h-full :justify-around :my-12]}

    [query ::filter-normal "normal"]
    [query ::filter-materialized "materialized"]]])
