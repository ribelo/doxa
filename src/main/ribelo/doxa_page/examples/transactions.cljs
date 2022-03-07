(ns ribelo.doxa-page.examples.transactions
  (:require
   [reagent.core :as r]
   [re-frame.core :as rf]
   [ribelo.doxa :as dx]
   [ribelo.doxa.util :as dxu]))

(defn random-name []
  (rand-nth ["Ivan" "Petr" "Ivan" "Oleg" "Lucy"]))

(defn random-age []
  (rand-int 100))

(defn random-aka []
  (rand-nth ["Tupen", "Devil", "Goofy" "Donald" "Micky"]))

(defn random-parent [id]
  (when-let [id (rand-int id)] {:person/id id}))

(defn random-person [id]
  {:person/id id
   :name (random-name)
   :age (random-age)
   :aka (vec (repeatedly (rand-int 3) random-aka))
   :child [(random-parent id)]})

(rf/reg-event-fx
  ::put-person
  (fn [{:keys [db]} _]
    (let [id (or (dx/pick db :person-id [:app/id :transactions]) 0)
          person (random-person id)]
      {:commit [[:dx/put person]
                [:dx/update [:app/id :transactions] :person-id (fnil inc 0)]]})))

(rf/reg-event-fx
  ::delete-person
  (fn [_ [_ ref]]
    {:commit [:dx/delete ref]}))

(rf/reg-event-fx
  ::merge-person
  (fn [{:keys [db]} _]
    (let [id (or (dx/pick db :person-id [:app/id :transactions]) 0)
          person (random-person id)]
      {:commit [[:dx/merge person]
                [:dx/update [:app/id :transactions] :person-id (fnil inc 0)]]})))

(rf/reg-event-fx
  ::clear-people
  (fn [{:keys [db]} _]
    {:commit [[:dx/delete :person/id]
              [:dx/delete [:app/id :transactions] :person-id (fnil inc 0)]]}))

(rf/reg-sub
  ::people
  (fn [db _]
    (dx/table db :person/id)))

(rf/reg-event-fx
  ::delete-key
  (fn [_ [_ ref k]]
    {:commit [:dx/delete ref k]}))

(rf/reg-event-fx
  ::put-random-aka
  (fn [_ [_ ref]]
    {:commit [:dx/merge ref :aka [(random-aka)]]}))

(rf/reg-event-fx
  ::delete-aka
  (fn [_ [_ ref aka]]
    {:commit [:dx/delete ref :aka aka]}))

(rf/reg-event-fx
  ::delete-child
  (fn [_ [_ ref k parent]]
    {:commit [:dx/delete ref k parent]}))

(rf/reg-event-fx
  ::change-name
  (fn [_ [_ ref]]
    {:commit [:dx/put ref :name (random-name)]}))

(rf/reg-event-fx
  ::inc-age
  (fn [_ [_ ref]]
    {:commit [:dx/update ref :age inc]}))

(rf/reg-event-fx
  ::dec-age
  (fn [_ [_ ref]]
    {:commit [:dx/update ref :age dec]}))

(defn db-view []
  (let [table @(rf/subscribe [::people])]
    [:div {:class [:overflow-y-auto]}
     (doall
       (for [[i [ref m]] (map-indexed vector table)]
         ^{:key i}
         [:div {:class [:flex :flex-row]}
          [:div "{"]
          [:div {:class [:w-32 :hover:text-nord-11 :cursor-pointer]
                 :on-click #(rf/dispatch [::delete-person ref])}
           (str ref)]
          [:div {:class [:flex]}
           [:div "{"]
           [:div {:class [:flex-col]}
            (doall
              (for [[i [k v]] (map-indexed vector m)]
                ^{:key i}
                [:div
                 (case k
                   :name
                   [:div {:class [:flex :flex-row]}
                    [:div {:class [:w-24 :hover:text-nord-11 :cursor-pointer]
                           :on-click #(rf/dispatch [::delete-key ref k])}
                     (str k)]
                    [:div {:class [:hover:text-nord-9 :cursor-pointer]
                           :on-click #(rf/dispatch [::change-name ref])}
                     (str v)]]

                   :age
                   [:div {:class [:flex :flex-row]}
                    [:div {:class [:w-24 :hover:text-nord-11 :cursor-pointer]
                           :on-click #(rf/dispatch [::delete-key ref k])}
                     (str k)]
                    [:div {:class [:mx-1 :font-bold :text-nord-11]
                           :on-click #(rf/dispatch [::dec-age ref])}
                     "-"]
                    [:div {:class [:hover:text-nord-9 :cursor-pointer]}
                     (str v)]
                    [:div {:class [:mx-1 :font-bold :text-nord-9]
                           :on-click #(rf/dispatch [::inc-age ref])}
                     "+"]]

                   :aka
                   [:div {:class [:flex :flex-row]}
                    [:div {:class [:w-24 :hover:text-nord-11 :cursor-pointer]
                           :on-click #(rf/dispatch [::delete-key ref k])}
                     (str k)]
                    [:div {:class [:flex]}
                     [:div {:class [:flex]}
                      [:div "["]
                      (doall
                        (for [[i x] (map-indexed vector v)]
                          ^{:key i}
                          [:div {:class [:mx-1 :hover:text-nord-11 :cursor-pointer]
                                 :on-click #(rf/dispatch [::delete-aka ref x])}
                           x]))
                      [:div "]"]]
                     [:div {:class [:mx-1 :font-bold :text-nord-9]
                            :on-click #(rf/dispatch [::put-random-aka ref])}
                      "+"]]]

                   (:child :_child)
                   [:div {:class [:flex :flex-row]}
                    [:div {:class [:w-24 :hover:text-nord-11 :cursor-pointer]
                           :on-click #(rf/dispatch [::delete-key ref k])}
                     (str k)]
                    [:div {:class [:flex]}
                     [:div {:class [:flex]}
                      [:div "#ordered/set #{"]
                      (cond
                        (dxu/-ref-lookup? v)
                        [:div {:class [:mx-1 :hover:text-nord-11 :cursor-pointer]
                               :on-click #(rf/dispatch [::delete-child ref k v])}
                         (str v)]
                        (dxu/-ref-lookups? v)
                        (doall
                          (for [[i x] (map-indexed vector v)]
                            ^{:key i}
                            [:div {:class [:mx-1 :hover:text-nord-11 :cursor-pointer]
                                   :on-click #(rf/dispatch [::delete-child ref k x])}
                             (str x)])))
                      [:div "}"]]]]

                   [:div {:class [:flex :flex-row]}
                    [:div {:class [:w-24 :hover:text-nord-11 :cursor-pointer]
                           :on-click #(rf/dispatch [::delete-key ref k])}
                     (str k)]
                    [:div
                     (str v)]])]))]
           [:div {:class [:self-end]}
            "}"]]
          [:div {:class [:self-end]}
           "}"]]))]))

(defn view []
  [:div {:class [:flex :flex-col :grow :m-12]}
   [:div {:class [:flex :flex-row :justify-center :mb-12]}
    [:div {:class [:bg-nord-2 :hover:bg-nord-3 :text-nord-5 :px-2 :mx-4 :cursor-pointer]
           :on-click #(rf/dispatch [::put-person])}
     "put random person"]
    [:div {:class [:bg-nord-2 :hover:bg-nord-3 :text-nord-5 :px-2 :mx-4 :cursor-pointer]
           :on-click #(rf/dispatch-sync [::merge-person])}
     "merge random person"]
    [:div {:class [:bg-nord-2 :hover:bg-nord-3 :text-nord-5 :px-2 :mx-4 :cursor-pointer]
           :on-click #(rf/dispatch-sync [::clear-people])}
     "delete all person"]]
   [:p {:class [:my-4 :self-center]}
    "try clicking here and there"]
   [db-view]])
