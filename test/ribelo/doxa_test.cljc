(ns ribelo.doxa-test
  ;; #?(:cljs (:require-macros [ribelo.doxa-test :refer [generate-matched-tests]]))
  (:require
   [ribelo.doxa :as dx]
   [ribelo.doxa.protocols :as p]
   [ribelo.doxa.map :as dxm]
   [ribelo.doxa.cache :as dxc]
   [ribelo.doxa.query :as dxq]
   [ribelo.doxa.util :as u]
   #?(:clj [clojure.test :as t] :cljs [cljs.test :as t])))

;; * util

(t/deftest normalize
  (t/is (= [[[:db/id 2] {:_friend [:db/id 1], :db/id 2, :name "Ivan"}] [[:db/id 1] {:db/id 1, :name "Petr", :friend [:db/id 2]}]]
           (dx/normalize {:db/id 1 :name "Petr" :friend {:db/id 2 :name "Ivan"}}))))

(t/deftest denormalize
  (t/is (= {[:db/id 1] {:db/id 1, :name "Petr", :friend {:db/id 2, :name "Ivan"}}
            [:db/id 2] {:db/id 2, :name "Ivan"}}
           (dx/denormalize {[:db/id 1] {:db/id 1 :name "Petr" :friend [:db/id 2]}
                            [:db/id 2] {:db/id 2 :name "Ivan"}}))))

(t/deftest search-in-map
  (t/is (= [:b :c]
           (u/-search-in-map {:a 1 :b 2 :c [1 2]} 2)))
  (t/is (= :b
           (u/-search-in-map {:a 1 :b 2 :c [1 2]} :b 2)))
  (t/is (= :c
           (u/-search-in-map {:a 1 :b 2 :c [1 2]} :c 2))))

(t/deftest eid-search
  (t/is (= [[:db/id 1]]
           (u/-eid-search {[:db/id 1] {:db/id 1 :name "Petr" :friend [:db/id 2]}
                           [:db/id 2] {:db/id 2 :name "Ivan"}}
                          1))))

#_(t/deftest diff-entity
  (t/is (= [[:- :a 1] [:+ :b 2]]
           (mapv vec (u/-diff-entity {:a 1} {:b 2}))))
  (t/is (= [[:- :a 1] [:+ :a 2]]
           (mapv vec (u/-diff-entity {:a 1} {:a 2}))))
  (t/is (= [[[:db/id 1] :- :a 1] [[:db/id 1] :+ :a 2]]
           (u/-diff-entity [:db/id 1] {:a 1} {:a 2}))))

#_(t/deftest diff-dbs
  (t/is (= [[[:db/id 1] :+ :b 2] [[:db/id 1] :- :b 2]]
           (dx/diff-dbs {[:db/id 1] {:a 1} [:db/id 2] {:b 2}} {[:db/id 1] {:a 1 :b 2}}))))

(t/deftest merge-entity
  (t/is (= {[:db/id 1] {:db/id 1, :name "Ivan", :friend [:db/id 2]}, [:db/id 2] {:db/id 2, :name "Petr" :_friend [:db/id 1]}}
           (u/-merge-entity {[:db/id 1] {:db/id 1 :name "Ivan"}} {:db/id 1 :friend {:db/id 2 :name "Petr"}}))))

(t/deftest put-entity
  (t/is (= {[:db/id 1] {:db/id 1, :name "Ivan", :friend [:db/id 2]}, [:db/id 2] {:db/id 2, :name "Petr" :_friend [:db/id 1]}}
           (u/-put-entity {} {:db/id 1 :name "Ivan" :friend {:db/id 2 :name "Petr"}}))))

(t/deftest safe-put
  (t/is (= {:db/id 1, :name "Ivan", [:db/id 1] {:db/id 1, :name "Petr"}}
           (u/-safe-put-kv {:db/id 1 :name "Ivan"} [:db/id 1] :name "Petr"))))

(t/deftest clearing-delete
  (t/is (= {[:db/id 1] {:db/id 1, :friend [[:db/id 2]]}}
           (u/-clearing-delete {[:db/id 1] {:db/id 1 :name "Ivan" :friend [[:db/id 2]]}} [[:db/id 1] :name])))
  (t/is (= {[:db/id 1] {:db/id 1, :name "Ivan"}}
           (u/-clearing-delete {[:db/id 1] {:db/id 1 :name "Ivan" :friend [[:db/id 2]]}} [[:db/id 1] :friend] [:db/id 2])))
  (t/is (= {}
           (u/-clearing-delete {[:db/id 1] {:db/id 1 :name "Ivan"}} [[:db/id 1] :name])))
  (t/is (= {[:db/id 2] {:db/id 2, :name "Petr"}}
           (u/-clearing-delete {[:db/id 1] {:db/id 1 :name "Ivan"}
                                   [:db/id 2] {:db/id 2 :name "Petr"}}
                               [[:db/id 1] :name]))))

(t/deftest delete-entity
  (t/is (= {[:db/id 1] {:db/id 1, :name "Ivan"}}
           (u/-delete-entity {[:db/id 1] {:db/id 1 :name "Ivan" :friend [:db/id 2]}
                              [:db/id 2] {:db/id 2 :name "Petr" :_friend [:db/id 1]}}
                             [:db/id 2]))))

(comment
  (def db (dx/create-dx {} [{:db/id 1 :name "Petr" :aka ["Devil"]}])))

(t/deftest commit
  (let [db (dx/create-dx {} [{:db/id 1 :name "Petr" :aka ["Devil"]}])]
    (t/testing "testing put"
      (t/is (= {[:db/id 1] {:db/id 1 :name "David", :aka ["Devil"]}}
               (dx/commit {} [[:dx/put {:db/id 1 :name "David" :aka ["Devil"]}]])))
      (t/is (= {[:db/id 1] {:db/id 1 :aka ["Tupen"]}}
               (dx/commit {} [[:dx/put [:db/id 1] :aka ["Tupen"]]])))
      (t/is (= {[:db/id 1] {:db/id 1 :aka ["Tupen"] :name "Oleg"}}
               (dx/commit {} [[:dx/put [:db/id 1] :aka ["Tupen"] :name "Oleg"]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "David", :aka ["Devil"]}}
               (dx/commit {} [[:dx/put [:db/id 1] {:name "David" :aka ["Devil"]}]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "David", :aka ["Devil"]}}
               (dx/commit db [[:dx/put [:db/id 1] :name "David"]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "David"}}
               (dx/commit db [[:dx/put [:db/id 1] {:name "David"}]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Tupen"]}}
               (dx/commit db [[:dx/put [:db/id 1] :aka ["Tupen"]]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"], :friend #{[:db/id 2] [:db/id 3]}}
                [:db/id 2] {:db/id 2, :name "Ivan" :_friend [:db/id 1]}
                [:db/id 3] {:db/id 3, :name "Lucy" :_friend [:db/id 1]}}
               (dx/commit db [[:dx/put [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Lucy"}]]])))
      (t/is (= {[:db/id 1] {:db/id 1 :a {:b 1, :c 2}}}
               (dx/commit {} [[:dx/put [:db/id 1] :a {:b 1 :c 2}]])))
      (t/is (= {[:db/id 1] {:db/id 1, :a [:db/id 2]}, [:db/id 2] {:b 1, :c 2, :db/id 2, :_a [:db/id 1]}}
               (dx/commit {} [[:dx/put [:db/id 1] :a {:b 1 :c 2 :db/id 2}]])))
      (t/is (= {[:db/id 1] {:db/id 1 :a #{[:db/id 2] [:db/id 3]}}
                [:db/id 2] {:b 1, :c 2, :db/id 2 :_a [:db/id 1]}
                [:db/id 3] {:b 3, :c 4, :db/id 3 :_a [:db/id 1]}}
               (dx/commit {} [[:dx/put [:db/id 1] :a [{:b 1 :c 2 :db/id 2}
                                                      {:b 3 :c 4 :db/id 3}]]])))
      (t/is (= {[:db/id 1] {:db/id 1, :_friend [:db/id 3], :name "Ivan", :friend [:db/id 2]}
                [:db/id 2] {:db/id 2, :_friend [:db/id 1], :name "Petr", :friend [:db/id 3]}
                [:db/id 3] {:db/id 3, :_friend [:db/id 2], :name "Lucy", :friend [:db/id 1]}}
               (dx/commit {} [:dx/put {:db/id 1 :name "Ivan" :friend {:db/id 2 :name "Petr" :friend {:db/id 3 :name "Lucy" :friend [:db/id 1]}}}])))
      (t/is (= {[:db/id 1] {:_friend #{[:db/id 2] [:db/id 3]}, :db/id 1, :name "Ivan", :friend [:db/id 2]},
                [:db/id 2] {:_friend [:db/id 1], :db/id 2, :name "Petr", :friend #{[:db/id 1] [:db/id 3]}}
                [:db/id 3] {:_friend [:db/id 2], :db/id 3, :name "Lucy", :friend [:db/id 1]}},
               (dx/commit {} [:dx/put {:db/id 1 :name "Ivan" :friend {:db/id 2 :name "Petr" :friend [{:db/id 1} {:db/id 3 :name "Lucy" :friend [:db/id 1]}]}}]))))

    (t/testing "testing merge"
      (t/is (= {[:db/id 1] {:db/id 1 :name "David", :aka ["Devil" "Devil"]}}
               (dx/commit db [[:dx/merge {:db/id 1 :name "David" :aka "Devil"}]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "David", :aka ["Devil" "Devil"]}}
               (dx/commit db [[:dx/merge {:db/id 1 :name "David" :aka ["Devil"]}]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name ["David" "Petr"], :aka ["Devil"]}}
               (dx/commit db [[:dx/merge {:db/id 1 :name ["David"]}]]))))

    (t/testing "testing delete"
      (t/is (= {}
               (dx/commit db [[:dx/delete [:db/id 1]]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr"}}
               (dx/commit db [[:dx/delete [:db/id 1] :aka]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr"}}
               (dx/commit db [[:dx/delete [:db/id 1] :aka "Devil"]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}}
               (dx/commit db [[:dx/delete [:db/id 1] :AKA "Devil"]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}}
               (dx/commit db [[:dx/put [:db/id 1] :friend {:db/id 2 :name "Ivan"}]
                              [:dx/delete [:db/id 2]]])))
      (t/is (= {[:db/id 2] {:db/id 2, :name "Ivan"}}
               (dx/commit db [[:dx/put [:db/id 1] :friend {:db/id 2 :name "Ivan"}]
                              [:dx/delete [:db/id 1]]])))
      (t/is (= {[:db/id 2] {:db/id 2, :name "Ivan"}, [:db/id 3] {:db/id 3, :name "Petr"}}
               (dx/commit db [[:dx/put [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Petr"}]]
                              [:dx/delete [:db/id 1]]]))))

    (t/testing "testing update"
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka "Tupen"}}
               (dx/commit db [[:dx/update [:db/id 1] assoc :aka "Tupen"]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil" "Tupen"]}}
               (dx/commit db [[:dx/update [:db/id 1] :aka conj "Tupen"]]))))

    (t/testing "testing match"
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}}
               (dx/commit db [[:dx/match  [:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}}
               (dx/commit db [[:dx/match  [:db/id 1] :aka ["Devil"]]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Tupen"]}}
               (dx/commit db [[:dx/match  [:db/id 1] :aka ["Devil"]]
                              [:dx/put    [:db/id 1] :aka ["Tupen"]]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}}
               (dx/commit db [[:dx/match  [:db/id 1] :aka ["Tupen"]]
                              [:dx/delete [:db/id 1] :aka]])))
      (t/is (= {[:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"] :sex :male}}
               (dx/commit db [[:dx/match  [:db/id 1] :aka ["Tupen"]]
                              [:dx/put    [:db/id 1] :age 15]
                              [:dx/match  [:db/id 1] :name "Petr"]
                              [:dx/put    [:db/id 1] :sex :male]]))))))

(def people-docs
  [{:db/id 1, :name "Petr", :aka ["Devil" "Tupen"] :child [[:db/id 2] [:db/id 3]]}
   {:db/id 2, :name "David", :father [:db/id 1]}
   {:db/id 3, :name "Thomas", :father [:db/id 1]}
   {:db/id 4, :name "Lucy" :friend [[:db/id 5]], :enemy [[:db/id 6]]}
   {:db/id 5, :name "Elizabeth" :friend [[:db/id 6]], :enemy [[:db/id 7]]}
   {:db/id 6, :name "Matthew", :father [:db/id 3], :friend [[:db/id 7]], :enemy [[:db/id 8]]}
   {:db/id 7, :name "Eunan", :friend [[:db/id 8]], :enemy [[:db/id 4]]}
   {:db/id 8, :name "Kerri"}
   {:db/id 9, :name "Rebecca"}])

(def part-docs
  [{:db/id 10, :part-name "Part A"}
   {:db/id 11, :part-name "Part A.A", :part-of [:db/id 10]}
   {:db/id 12, :part-name "Part A.A.A", :part-of [:db/id 11]}
   {:db/id 13, :part-name "Part A.A.A.A", :part-of [:db/id 12]}
   {:db/id 14, :part-name "Part A.A.A.B", :part-of [:db/id 12]}
   {:db/id 15, :part-name "Part A.B", :part-of [:db/id 10]}
   {:db/id 16, :part-name "Part A.B.A", :part-of [:db/id 15]}
   {:db/id 17, :part-name "Part A.B.A.A", :part-of [:db/id 16]}
   {:db/id 18, :part-name "Part A.B.A.B", :part-of [:db/id 16]}])

;; * datascript
;; ** pull

(comment
  (def db (dx/create-dx {} (into people-docs part-docs)))
  #_(def db2 (dx/commit! (dx/create-dx [] {::dx/with-diff? true})
                         (into [] (map (fn [tx] [:dx/put tx]))  (into people-docs part-docs)))))

(t/deftest test-pull
  (let [db (dx/create-dx {} (into people-docs part-docs))]
    (t/testing "test pull attr"
      (t/is (= {:name "Petr" :aka ["Devil" "Tupen"]}
               (dx/pull db [:name :aka] [:db/id 1])))

      (t/is (= {:name "Matthew" :father [:db/id 3] :db/id 6}
               (dx/pull db [:name :father :db/id] [:db/id 6])))

      (t/is (= {:name "Matthew" :father {:name "Thomas"} :db/id 6}
               (dx/pull db [:name :db/id {:father [:name]}] [:db/id 6])))

      (t/is (= [{:name "Petr"}
                {:name "Elizabeth"}
                {:name "Eunan"}
                {:name "Rebecca"}]
               (dx/pull db [:name] [[:db/id 1] [:db/id 5] [:db/id 7] [:db/id 9]]))))

    (t/testing "test pull reverse attr"
      (t/is (= {:name "David" :_child [:db/id 1]}
               (dx/pull db [:name :_child] [:db/id 2])))

      (t/is (= {:name "David" :_child {:name "Petr"}}
               (dx/pull db [:name {:_child [:name]}] [:db/id 2]))))

    (t/testing "reverse non-component references yield collections"
      (t/is (= {:name "Thomas" :_father [:db/id 6]}
               (dx/pull db [:name :_father] [:db/id 3])))

      (t/is (or (= {:name "Petr" :_father #{[:db/id 3] [:db/id 2]}}
                   (dx/pull db [:name :_father] [:db/id 1]))
                (= {:name "Petr" :_father #{[:db/id 2] [:db/id 3]}}
                   (dx/pull db [:name :_father] [:db/id 1]))))

      (t/is (= {:name "Thomas" :_father {:name "Matthew"}}
               (dx/pull db [:name {:_father [:name]}] [:db/id 3])))

      (t/is (or (= {:name "Petr" :_father [{:name "Thomas"} {:name "David"}]}
                   (dx/pull db [:name {:_father [:name]}] [:db/id 1]))
                (= {:name "Petr" :_father [{:name "David"} {:name "Thomas"}]}
                   (dx/pull db [:name {:_father [:name]}] [:db/id 1])))))

    (t/testing "test pull component attr"
      (t/is (= {:part-name "Part A.A", :part-of [:db/id 10]}
               (dx/pull db [:part-name :part-of] [:db/id 11])))
      (t/is (= {:part-name "Part A.A", :_part-of [:db/id 12]}
               (dx/pull db [:part-name :_part-of] [:db/id 11])))
      (t/is (= {:part-name "Part A.A", :part-of {:part-name "Part A"}}
               (dx/pull db [:part-name {:part-of [:part-name]}] [:db/id 11]))))

    (t/testing "test-pull-wildcard"
      (t/is (= {:db/id 1, :name "Petr", :aka ["Devil" "Tupen"], :child #{[:db/id 2] [:db/id 3]}}
               (dx/pull db [:*] [:db/id 1])))
      (t/is (= {:db/id 2, :_child [:db/id 1], :name "David", :father [:db/id 1]}
               (dx/pull db [:* :_child] [:db/id 2]))))

    (t/testing "test-pull-map"
      (t/testing "Single attrs yield a map"
        (t/is (= {:name "Matthew" :father {:name "Thomas"}}
                 (dx/pull db [:name {:father [:name]}] [:db/id 6]))))

      (t/testing "Multi attrs yield a collection of maps"
        (t/is (= {:name "Petr" :child [{:name "David"}
                                       {:name "Thomas"}]}
                 (dx/pull db [:name {:child [:name]}] [:db/id 1]))))

      (t/testing "Missing attrs are dropped"
        (t/is (= {:name "David"}
                 (dx/pull db [:name {:child [:name]}] [:db/id 2]))))

      (t/testing "Non matching results are removed from collections"
        (t/is (= {:name "Petr"}
                 (dx/pull db [:name {:child [:foo]}] [:db/id 1]))))

      (t/testing "Map specs can override component expansion"
        (t/is (or (= {:part-name "Part A", :_part-of [{:part-name "Part A.B"} {:part-name "Part A.A"}]}
                     (dx/pull db [:part-name {:_part-of [:part-name]}] [:db/id 10]))
                  (= {:part-name "Part A", :_part-of [{:part-name "Part A.A"} {:part-name "Part A.B"}]}
                     (dx/pull db [:part-name {:_part-of [:part-name]}] [:db/id 10]))))))

    ;; (t/testing "eql"
    ;;   (t/is (= {:name "Petr"}
    ;;            (dx/pull db {[:db/id 1] [:name]})
    ;;            (dx/pull db [:name] [:db/id 1]))))

    ))

(comment
  (let [tx {:chat/id 0
            :chat/entries
            [{:message/id           0
              :message/text         "foo"
              :chat.entry/timestamp "1234"}
             {:message/id           1
              :message/text         "bar"
              :chat.entry/timestamp "1235"}
             {:audio/id             0
              :audio/url            "audio://asdf.jkl"
              :audio/duration       1234
              :chat.entry/timestamp "4567"}
             {:photo/id             0
              :photo/url            "photo://asdf_10x10.jkl"
              :photo/height         10
              :photo/width          10
              :chat.entry/timestamp "7890"}]}]
    (def db (dx/dx-with [tx]))))


(t/deftest pull-union
  (let [tx {:chat/id 0
            :chat/entries
            [{:message/id           0
              :message/text         "foo"
              :chat.entry/timestamp "1234"}
             {:message/id           1
              :message/text         "bar"
              :chat.entry/timestamp "1235"}
             {:audio/id             0
              :audio/url            "audio://asdf.jkl"
              :audio/duration       1234
              :chat.entry/timestamp "4567"}
             {:photo/id             0
              :photo/url            "photo://asdf_10x10.jkl"
              :photo/height         10
              :photo/width          10
              :chat.entry/timestamp "7890"}]}
        db (dx/dx-with [tx])]
    (t/is (or (= {:chat/entries
                  [{:photo/id 0, :photo/url "photo://asdf_10x10.jkl" :photo/width 10, :photo/height 10, :chat.entry/timestamp "7890"}
                   {:message/id 1, :message/text "bar" :chat.entry/timestamp "1235"}
                   {:message/id 0, :message/text "foo" :chat.entry/timestamp "1234"}]}
                 (dx/pull db [{:chat/entries {:message/id [:message/id :message/text :chat.entry/timestamp]
                                              :photo/id   [:photo/id :photo/url :photo/width :photo/height :chat.entry/timestamp]}}]
                          [:chat/id 0]))
              ;; cljs
              (= {:chat/entries
                  [{:message/id 0, :message/text "foo" :chat.entry/timestamp "1234"}
                   {:message/id 1, :message/text "bar" :chat.entry/timestamp "1235"}
                   {:photo/id 0, :photo/url "photo://asdf_10x10.jkl" :photo/width 10, :photo/height 10, :chat.entry/timestamp "7890"}]}
                 (dx/pull db [{:chat/entries {:message/id [:message/id :message/text :chat.entry/timestamp]
                                              :photo/id   [:photo/id :photo/url :photo/width :photo/height :chat.entry/timestamp]}}]
                          [:chat/id 0]))))))

;; * query

(comment
  (def db (dx/dx-with [{:db/id 1, :name "Ivan" :age 15 :friend [[:db/id 2] [:db/id 3]]}
                       {:db/id 2, :name "Petr" :age 37 :friend [:db/id 3]}
                       {:db/id 3, :name "Ivan" :age 37}
                       {:db/id 4, :age 15}])))

(t/deftest test-joins
  (let [db (dx/dx-with [{:db/id 1, :name "Ivan", :age 15 :friend [[:db/id 2] [:db/id 3]]}
                        {:db/id 2, :name "Petr", :age 37 :friend [:db/id 3]}
                        {:db/id 3, :name "Ivan", :age 37}
                        {:db/id 4, :age 15}])]

    (t/is (= #{[[:db/id 1]] [[:db/id 3]] [[:db/id 2]]}
             (dx/q '[:find ?e
                     :where [?e :name]]
               db)))
    (t/is (= #{[[:db/id 1]] [[:db/id 3]]}
             (dx/q '[:find ?e
                     :where [?e ?name "Ivan"]]
               db)))
    (t/is (= #{[[:db/id 1] 15] [[:db/id 3] 37]}
             (dx/q '[:find ?e ?v
                     :where
                     [?e :name "Ivan"]
                     [?e :age ?v]] db)
             (dx/q '[:find ?e ?v
                     :in ?name
                     :where
                     [?e :name ?name]
                     [?e :age ?v]]
               db "Ivan")
             (let [x "Ivan"]
               (dx/q '[:find ?e ?v
                       :in ?name
                       :where
                       [?e :name ?name]
                       [?e :age ?v]]
                 db x))))

    (t/is (= #{[[:db/id 3] [:db/id 1]] [[:db/id 2] [:db/id 2]] [[:db/id 1] [:db/id 1]] [[:db/id 3] [:db/id 3]] [[:db/id 1] [:db/id 3]]}
             (dx/q '[:find ?e1 ?e2
                     :where
                     [?e1 :name ?n]
                     [?e2 :name ?n]] db)))

    (t/is (= #{[[:db/id 1] [:db/id 1] "Ivan"] [[:db/id 3] [:db/id 3] "Ivan"] [[:db/id 3] [:db/id 2] "Petr"]}
             (dx/q '[:find ?e1 ?e2 ?n
                     :where
                     [?e1 :name "Ivan"]
                     [?e1 :age      ?a]
                     [?e2 :age      ?a]
                     [?e2 :name     ?n]]
               db)))

    (t/is (= #{[[:db/id 2] "Petr"] [[:db/id 3] "Ivan"]}
             (dx/q '[:find ?f ?fname
                     :where
                     [?f :name    ?fname]
                     [?e :name    "Ivan"]
                     [?e :friend ?friend]
                     [(some #{?f} ?friend)]]
               db)))
    (t/is (= #{[[:db/id 1]]}
             (dx/q '[:find ?e
                     :where
                     [?e :name    "Ivan"]
                     [?e :friend  ?f]]
               db)))
    (t/is (= #{[[:db/id 1]]}
             (dx/q '[:find ?e
                     :in ?f
                     :where
                     [?e :name    "Ivan"]
                     [?e :friend  ?fs]
                     [(some #{?f} ?fs)]]
               db [:db/id 3])))
    (t/is (= 1
             (count (dx/q '[:find ?e
                      :where
                      [?e :name    "Ivan"]
                      :limit 1]
                      db))))))

(comment

  (def db (dx/dx-with [{:db/id 1
                        :name  "Ivan"
                        :aka   ["ivolga" "pi"]}
                       {:db/id 2
                        :name  "Petr"
                        :aka   ["porosenok" "pi"]}])))

(t/deftest test-q-many
  (let [db (dx/dx-with [{:db/id 1
                         :name  "Ivan"
                         :aka   ["ivolga" "pi"]}
                        {:db/id 2
                         :name  "Petr"
                         :aka   ["porosenok" "pi"]}])]
    (t/is (= #{["Ivan" "Petr"] ["Petr" "Ivan"] ["Petr" "Petr"] ["Ivan" "Ivan"]}
             (dx/q '[:find ?n1 ?n2
                     :where
                     [?e1 :aka ?xs]
                     [?e2 :aka ?ys]
                     [(intersect? ?xs ?ys)]
                     [?e1 :name ?n1]
                     [?e2 :name ?n2]]
               db)))))

(comment
  (def db (dx/dx-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                       {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                       {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])))

(t/deftest test-q-in
  (let [db (dx/dx-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                        {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                        {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])]

    (t/is (= #{[[:db/id 1]] [[:db/id 3]]}
             (dx/q '[:find  ?e
                     :in    ?attr ?value
                     :where [?e ?attr ?value]]
               db :name "Ivan")))

    (t/is (= #{[[:db/id 1]] [[:db/id 3]] [[:db/id 2]]}
             (dx/q '[:find  ?e
                    :in    ?attr [?value]
                    :where [?e ?attr ?value]]
               db :name ["Ivan" "Petr"])))

    (t/is (= #{[[:db/id 3]] [[:db/id 2]]}
             (dx/q '[:find ?e
                    :in    ?attr ?value
                    :where [?e ?attr ?value]]
               db :age 37)))

    (t/is (= #{[[:db/id 2] "petr@gmail.com"]
               [[:db/id 3] "ivan@mail.ru"]
               [[:db/id 1] "ivan@mail.ru"]}
             (dx/q '[:find ?e ?email
                    :in [[?n ?email]]
                    :where
                    [?e :name ?n]
                    [?e :email ?email]]
               db
               [["Ivan" "ivan@mail.ru"]
                ["Petr" "petr@gmail.com"]])))))

;; (t/deftest test-q-keys
;;   (let [db (dx/dx-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
;;                         {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
;;                         {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])]
;;     (t/is (= #{{:name "Ivan" :email "ivan@mail.ru" :age 15}
;;                {:name "Petr" :email "petr@gmail.com" :age 37}
;;                {:name "Ivan" :email "ivan@mail.ru" :age 37}}
;;              (dx/q [:find ?name ?email ?age
;;                     :keys [:name :email :age]
;;                     :where
;;                     [?e :name ?name]
;;                     [?e :age ?age]
;;                     [?e :email ?email]]
;;                db)))))

(comment
  (def db (dx/dx-with
            [{:db/id 1 :follow [:db/id 2]}
             {:db/id 2 :follow [[:db/id 3] [:db/id 4]]}
             {:db/id 3 :follow [:db/id 4]}
             {:db/id 4 :follow [:db/id 6]}
             {:db/id 5 :follow [:db/id 3]}])))

#_(t/deftest test-q-rules
  (let [db (dx/dx-with
            [{:db/id 1 :follow [:db/id 2]}
             {:db/id 2 :follow [[:db/id 3] [:db/id 4]]}
             {:db/id 3 :follow [:db/id 4]}
             {:db/id 4 :follow [:db/id 6]}
             {:db/id 5 :follow [:db/id 3]}])]
    (t/is (= #{[1 2] [2 3] [3 4] [2 4] [5 3] [4 6]}
             (dx/q [:find ?e1 ?e2
                    :in %
                    :where (follow ?e1 ?e2)]
               db
               '[[(follow ?x ?y)
                  [?x :follow [_ ?y]]]])))
    (t/is (= #{[3 2] [6 4] [4 2]}
             (dx/q [:find ?y ?x
                    :in %
                    :where
                    [_ _ ?x]
                    (rule ?x ?y)
                    [(even? ?x)]]
               db
               '[[(rule ?a ?b)
                  [?a :follow [_ ?b]]]])))
    (t/is (= #{[:db/id] [:follow]}
             (dx/q [:find ?x
                    :in %
                    :where
                    [?e _ _]
                    (rule ?x)]
               db
               '[[(rule ?e)
                  [_ ?e _]]])))
    (t/is (= #{[2] [3] [4]}
             (dx/q [:find ?e2
                    :in ?e1 %
                    :where (follow ?e1 ?e2)]
               db
               1
               '[[(follow ?e2 ?e1)
                  [?e2 :follow [_ ?e1]]]
                 [(follow ?e2 ?e1)
                  [?e2 :follow [_ ?t]]
                  [?t :follow [_ ?e1]]]])))
    #_(t/is (= #{[2] [3] [4] [6]}
               (dx/q [:find ?e2
                      :in ?e1 %
                      :where (follow ?e1 ?e2)]
                 db
                 1
                 '[[(follow ?e1 ?e2)
                    [?e1 :follow ?e2]]
                   [(follow ?e1 ?e2)
                    [?e1 :follow ?t]
                    (follow ?t ?e2)]])))))

(comment
  (def db (dx/dx-with [{:db/id 1 :name "Ivan" :age 10}
                       {:db/id 2 :name "Ivan" :age 20}
                       {:db/id 3 :name "Oleg" :age 10}
                       {:db/id 4 :name "Oleg" :age 20}
                       {:db/id 5 :name "Ivan" :age 10}
                       {:db/id 6 :name "Ivan" :age 20}])))

(t/deftest test-q-or
  (let [db (dx/dx-with [{:db/id 1 :name "Ivan" :age 10}
                        {:db/id 2 :name "Ivan" :age 20}
                        {:db/id 3 :name "Oleg" :age 10}
                        {:db/id 4 :name "Oleg" :age 20}
                        {:db/id 5 :name "Ivan" :age 10}
                        {:db/id 6 :name "Ivan" :age 20}])]
    (t/is (= #{[[:db/id 1]] [[:db/id 3]] [[:db/id 4]] [[:db/id 5]]}
             (dx/q '[:find ?e
                     :where
                     (or [?e :name "Oleg"]
                         [?e :age 10])]
               db)))
    (t/is (= #{[[:db/id 3]] [[:db/id 4]]}
             (dx/q '[:find ?e
                     :where
                     (or [?e :name "Oleg"]
                         [?e :age 30])]
               db)))
    (t/is (= #{}
             (dx/q '[:find ?e
                    :where
                    (or [?e :name "Petr"]
                        [?e :age 30])]
               db)))
    (t/is (= #{[[:db/id 1]] [[:db/id 5]]}
             (dx/q '[:find ?e
                    :where
                    [?e :name "Ivan"]
                    (or [?e :name "Oleg"]
                        [?e :age 10])]
               db)))
    #_(t/is (= #{[5] [1] [4]}
             (dx/q '[:find ?e
                     :where
                    (or (and [?e :age ?a]
                             [?e :name "Ivan"]
                             [[:db/id 1] :age ?a])
                        (and [?e :age ?a]
                             [?e :name "Oleg"]
                             [[:db/id 2] :age ?a]))]
               db)))
    #_(t/is (= #{[5] [1] [4]}
             (dx/q '[:find ?e
                    :where
                    (or (and [?e :name "Ivan"]
                             [[:db/id 1] :age ?a])
                        (and [?e :name "Oleg"]
                             [[:db/id 1] :age ?a]))
                    [?e :age ?a]]
               db)))
    ;; diffrent order
    #_(t/is (= #{[5] [1] [4]}
             (dx/q [:find ?e
                    :where
                    (or (and [[:db/id 1] :age ?a]
                             [?e :name "Ivan"])
                        (and [?e :name "Oleg"]
                             [[:db/id 1] :age ?a]))
                    [?e :age ?a]]
               db)))))

;; crux

;; #?(:clj
   ;; (def bond-db
   ;;   (let [data (->> (read-string (slurp "./resources/james-bond.edn"))
   ;;                   (mapv (fn [m]
   ;;                           (-> m
   ;;                               (update :film/director   (partial vector :db/id))
   ;;                               (update :film/bond       (partial vector :db/id))
   ;;                               (update :film/vehicles   (fn [xs] (mapv (partial vector :db/id) xs)))
   ;;                               (update :film/box        (partial vector :db/id))
   ;;                               (update :film/bond-girls (fn [xs] (mapv (partial vector :db/id) xs)))
   ;;                               (update :film/box        (partial vector :db/id))))))]
   ;;     (-> (dx/create-dx {})
   ;;         (dx/commit (into [] (map (fn [tx] [:dx/put tx])) data)))))
   ;; )

;; #?(:clj
;;    (t/testing "project fn"
;;      (t/is (= #:film{:name "Spectre", :year "2015"}
;;               (dx/pull bond-db [:film/name :film/year] [:db/id :spectre])))))

;; #?(:clj
;;    (t/deftest query-pull
;;      (t/testing "query"
;;        (t/is (= #{{}}
;;                 (dx/q [:find (pull [] [?table ?e])
;;                        :where [?table ?e :vehicle/brand "Aston Martin"]]
;;                   bond-db)))

;;        (let [expected #{{:vehicle/brand "Aston Martin", :vehicle/model "DB10"}
;;                         {:vehicle/brand "Aston Martin", :vehicle/model "DBS V12"}
;;                         {:vehicle/brand "Aston Martin", :vehicle/model "DB5"}
;;                         {:vehicle/brand "Aston Martin", :vehicle/model "V12 Vanquish"}
;;                         {:vehicle/brand "Aston Martin", :vehicle/model "V8 Vantage Volante"}
;;                         {:vehicle/brand "Aston Martin", :vehicle/model "DBS"}}]
;;          (t/is (= expected
;;                   (dx/q [:find  (pull [:vehicle/brand :vehicle/model] [?table ?e])
;;                          :where [?table ?e :vehicle/brand "Aston Martin"]]
;;                     bond-db)))))))

(t/deftest gh-7
  ;; https://github.com/ribelo/doxa/issues/7
  (let [entity-id  "6037b7a5-5a77-48a3-a294-8dba786d8e9d"
        gql-entity (with-meta
                     {:__typename "release"
                      :id         entity-id
                      :created    "2021-05-10T09:39:28"
                      :release/id entity-id}
                     {::dx/entity-key :release/id})
        db         (dx/commit (dx/create-dx) [[:dx/put gql-entity]])]
    (t/is (= {:__typename "release", :created "2021-05-10T09:39:28"}
             (dx/pull db [:__typename :created] [:release/id entity-id])))))

;; (t/deftest gh-8
;;   ;; https://github.com/ribelo/doxa/issues/8
;;   (let [person1 {:person/id 1
;;                  :gender    "MALE"
;;                  :name      "Bob"
;;                  :face      {:eyes "BLUE"}}
;;         person2 {:person/id 2
;;                  :gender    "FEMALE"
;;                  :name      "Joanne"
;;                  :face      {:eyes "GREEN"}}
;;         db      (dx/commit (dx/create-dx) [[:dx/put person1]
;;                                            [:dx/put person2]])]
;;     (t/is (= #{[1 "Bob" "BLUE"]}
;;              (dx/q '[:find ?person ?name ?eye-color
;;                     :in ?table ?color
;;                     :where
;;                     [?table ?person :name ?name]
;;                     [?table ?person :face {:eyes ?eye-color}]
;;                     [(= ?color ?eye-color)]]
;;                db :person/id "BLUE")))))

(t/deftest gh-14
  (let [db  (dx/create-dx {} [{:db/id 1, :name "Ivan" :age 15}
                              {:db/id 2, :name "Petr" :age 37}])
        age 15]
    (t/is (= #{["Ivan"]}
             (dx/q '[:find ?name
                     :in ?age
                     :where
                     [?e :name ?name]
                     [?e :age ?age]]
               db age)))))

(t/deftest gh-16
  ;; https://github.com/ribelo/doxa/issues/16
  (let [db (dx/create-dx {} [{:db/id 4 :name "mike" :cars []}
                             {:db/id 1 :name "ivan" :cars [{:db/id 10 :name "tesla"}
                                                           {:db/id 11 :name "ferrari"}]}
                             {:db/id 2 :name "petr" :cars [{:db/id 10 :name "peugot"}]}
                             {:db/id 3 :name "mike" :cars {:db/id 10 :name "peugot"}}])]

    (t/is (vector? (:cars (dx/pull db [:name {:cars [:name]}] [:db/id 2])))
          "pull returns vector when 1 entity in join")))

(t/deftest gh-17
  ;; https://github.com/ribelo/doxa/issues/17
  (let [db (dx/create-dx {} [{:db/id 1 :name "ivan" :car {:db/id 10 :name "tesla"}}])]
    (t/is (map? (:car (dx/pull db [:name {:car [:name]}] [:db/id 1]))))))

;; (defmacro generate-matched-tests [datom]
;;   (m/rewrite datom
;;     (m/or
;;      [(m/or (m/and ?table (m/not (m/pred dx/qsymbol?))) (m/let [?table :db/id]))
;;       (m/or (m/and ?e     (m/not (m/pred dx/qsymbol?))) (m/let [?e          1]))
;;       (m/or (m/and ?a     (m/not (m/pred dx/qsymbol?))) (m/let [?a         :a]))
;;       (m/or (m/and ?v     (m/not (m/pred dx/qsymbol?))) (m/let [?v          1]))]
;;      (m/and
;;       (m/let [?table :db/id])
;;       [(m/or (m/and ?e     (m/not (m/pred dx/qsymbol?))) (m/let [?e         1]))
;;        (m/or (m/and ?a     (m/not (m/pred dx/qsymbol?))) (m/let [?a        :a]))
;;        (m/or (m/and ?v     (m/not (m/pred dx/qsymbol?))) (m/let [?v         1]))]))
;;     (do (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms [[[~?table] :+ {~?e {~?a ~?v}}]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table] :r {~?e {~?a ~?v}}]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table ~?e] :+ {~?a ~?v}]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table ~?e] :r {~?a ~?v}]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table ~?e ~?a] :+ ~?v]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table ~?e ~?a] :r ~?v]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table] :-]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table ~?e] :-]])
;;                 ~datom)))
;;         (t/is (true?
;;                (dx/-datoms-match-datom?
;;                 (dx/-edits->datoms  [[[~?table ~?e ~?a] :-]])
;;                 ~datom))))))

#_(t/deftest match-changes
  (let [db (dx/create-dx [] {::dx/with-diff? true})]
    (t/testing "generated maching tests"
      (doseq [table ['?table :db/id]
              e     ['?e          1]
              a     ['?a         :a]
              v     ['?v          1]]
        (generate-matched-tests [table e a v])))

    (t/testing "match-datom"
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[?table ?e ?a v]]))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[:db/id ?e ?a ?v]]))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[?table ?e ?a "ivan"]]))))
      (t/is (false?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[?table ?e ?a "petr"]]))))
      (t/is (true?
             (-> (dx/with-commit db [[:dx/put    [:db/id 1] :name "ivan"]
                                     [:dx/put    [:db/id 2] :name "petr"]])
                 (dx/with-commit    [:dx/delete [:db/id 1] :name])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[?table ?e ?a "ivan"]]))))
      (t/is (true?
             (-> (dx/with-commit db [[:dx/put    [:db/id 1] :name "ivan"]
                                     [:dx/put    [:db/id 2] :name "petr"]])
                 (dx/with-commit    [:dx/delete [:db/id 1] :name])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[?table ?e ?a "petr"]]))))
      (t/is (true?
             (-> (dx/with-commit db [[:dx/put    [:db/id 1] :name "ivan"]
                                     [:dx/put    [:db/id 2] :name "petr"]])
                 (dx/with-commit    [:dx/delete [:db/id 1] :name])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[:db/id ?e ?a ?v]]))))
      (t/is (false?
             (-> (dx/with-commit db [[:dx/put    [:db/id 1] :name "ivan"]
                                     [:dx/put    [:db/id 2] :name "petr"]])
                 (dx/with-commit    [:dx/delete [:db/id 1] :name])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[:person/id ?e ?a ?v]]))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] :name "ivan"])
                 (dx/with-commit    [:dx/delete [:db/id 1] :name])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-where?
                  '[[?ta ?e ?a ?v]])))))

    (t/testing "match-query"
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr ?v]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [:db/id ?e ?attr ?v]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [:db/id 1 ?attr ?v]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [:db/id 1 :name ?v]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [:db/id 1 :name "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table 1 :name "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e :name "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr "ivan"]])))))
      (t/is (false?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [:person/id ?e ?attr ?v]])))))
      (t/is (false?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table 2 ?attr ?v]])))))
      (t/is (false?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e :age ?v]])))))
      (t/is (false?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr "petr"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 (dx/with-commit    [:dx/put [:db/id 1] :name "david"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr "petr"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr "ivan"]
                     [(f)]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [(f)]
                     [?table ?e ?attr "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 (dx/with-commit    [:dx/put [:db/id 1] :name "petr"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put [:db/id 1] :name "ivan"])
                 (dx/with-commit    [:dx/put [:db/id 1] :name "petr"])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr "petr"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 (dx/with-commit    [:dx/delete [:db/id 1] :name])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e ?attr "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e :name "ivan"]
                     [?table ?e :age  ?age]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [?table ?e :age  ?age]
                     [?table ?e :name "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [(adult? ?age)]
                     [?table ?e :age  ?age]
                     [?table ?e :name "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:where
                     [(adult? ?age)]
                     [?table ?e :age  ?age]
                     [?table ?e :name "ivan"]])))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:in ?table
                     :where
                     [?table ?e :age  ?age]
                     [?table ?e :name "ivan"]]
                   :db/id)))))
      (t/is (false?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:in ?table
                     :where
                     [?table ?e :age  ?age]
                     [?table ?e :name "ivan"]]
                   :person/id)))))
      (t/is (true?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:in [?table]
                     :where
                     [?table ?e :age  ?age]
                     [?table ?e :name "ivan"]]
                   [:db/id :person/id])))))
      (t/is (false?
             (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                 meta
                 ::dx/txs
                 (dx/-datoms-since 0)
                 (dx/-datoms-match-query?
                  (dx/parse-query
                   '[:in [?table]
                     :where
                     [?table ?e :age  ?age]
                     [?table ?e :name "ivan"]]
                   [:person/id :car/id])))))
      (t/is (true?
             (let [tables [:db/id :person/id]]
               (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                   meta
                   ::dx/txs
                   (dx/-datoms-since 0)
                   (dx/-datoms-match-query?
                    (dx/parse-query
                     '[:in [?table]
                       :where
                       [?table ?e :age  ?age]
                       [?table ?e :name "ivan"]]
                     tables))))))
      (t/is (false?
             (let [tables [:person/id :car/id]]
               (-> (dx/with-commit db [:dx/put    [:db/id 1] {:name "ivan" :age 18}])
                   meta
                   ::dx/txs
                   (dx/-datoms-since 0)
                   (dx/-datoms-match-query?
                    (dx/parse-query
                     '[:in [?table]
                       :where
                       [?table ?e :age  ?age]
                       [?table ?e :name "ivan"]]
                     tables)))))))))

(comment
  (def conn_ (dx/connect! (dx/create-dx {} [] {::dx/cache (atom (dxc/doxa-cache))}))))

;; (t/deftest cached-query
;;   (let [conn_ (dx/connect! (dx/create-dx {} [] {::dx/cache (atom (dxc/doxa-cache))}))]
;;     (dx/commit! conn_ [:dx/put [:db/id 1] {:name "ivan"}])
;;     (Thread/sleep 10)
;;     (t/is (true?  (::dx/fresh? (dx/mq '[:find [?e ...] :where [?e :name "ivan"]] @conn_))))
;;     (t/is (false? (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))
;;     (dx/commit! conn_ [:dx/put [:db/id 2] {:name "ivan"}])
;;     (Thread/sleep 10)
;;     (t/is (true?  (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))
;;     (dx/commit! conn_ [:dx/put [:dog/id 1] {:name "pixel"}])
;;     (t/is (false? (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))
;;     (dx/commit! conn_ [:dx/put [:db/id 3] {:name "ivan"}])
;;     (t/is (true?  (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))))

(t/deftest gh-23
  ;; https://github.com/ribelo/doxa/issues/23
  (let [db (->>
             ^{::dx/entity-key :person/id}
             {:id        "10"
              :person/id "10"
              :name      "Enzo"
              :car
              ^{::dx/entity-key :automobile/id}
              {:id            "20"      ; << comment this out and pull works
               :automobile/id "20"
               :name          "Audi"}}
             (vector :dx/put)
             (dx/commit (dx/create-dx)))]
    (t/is (= {:name "Enzo", :car {:name "Audi"}}
             (dx/pull db [:name {:car [:autombile/id :name]}] [:person/id "10"])))))

(t/deftest gh-24 []
  (let [db1 (dxm/empty-db {:cache (dxc/doxa-cache)})
        people (fn [db]
                 (dxq/-mq '[:find [?name ...]
                            :where
                            [?e :name ?name]]
                          db))
        r1 (people db1)
        db2 (->> [{:person/id   "20"
                   :name "Chris"
                   :age  15}]
                 (vector :dx/put)
                 (dx/commit db1)
                 )
        r2 (people db2)
        r3 (people db2)]
    (t/is (= [] r1))
    (t/is (= ["Chris"] r2 r3))))

(comment
  (def db (dx/commit {} [:dx/merge [{:product/id 1
                                     :product/name "product1"
                                     :product/offer {:offer/id 1 :offer/name "offer1" :offer/price 1.0 :offer/market {:market/id 1 :market/name "market"}}}
                                    {:product/id 2
                                     :product/name "product2"
                                     :product/offer {:offer/id 2 :offer/name "offer1" :offer/price 2.0 :offer/market {:market/id 1 :market/name "market"}}}]]))
  (dx/pick db [:market/name] [:market/id 1])
  (tap> [:ok (dx/pick db {:offer/_market [:offer/price {:product/_offer [:product/name]}]} [:market/id 1])])
  (tap> (+ 1 1))
  )

;; (let [db (dx/create-dx {} [{:id       1
;;                             :name   "Olympics"
;;                             :sports [{:id   2
;;                                       :name "Boardercross"}
;;                                      {:id   3
;;                                       :name "Curling"}]}]
;;                        {::dx/cache (atom (dxc/doxa-cache))})]
;;   (dx/mpull db [:id {:sports [:id :name]}] [:id 1]))
;; => {:id 1, :sports [{:id 2, :name "Boardercross"} {:id 3, :name "Curling"}]}
