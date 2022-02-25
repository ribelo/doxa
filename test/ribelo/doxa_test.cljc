(ns ribelo.doxa-test
  ;; #?(:cljs (:require-macros [ribelo.doxa-test :refer [generate-matched-tests]]))
  (:require
   [ribelo.doxa :as dx]
   [clojure.test :as t]
   [meander.epsilon :as m]))

;; * datalog->meander

(t/deftest parse-query
  (t/testing "find"
    (t/is (= {:find '[?x ?y]}
             (dx/parse-find '[?x ?y])))
    (t/is (= {:find '[?x ?y]     :mapcat? true :first? true}
             (dx/parse-find '[?x ?y .])))
    (t/is (= {:find '[?x ?y]     :mapcat? true}
             (dx/parse-find '[?x ?y ...])))
    (t/is (= {:find '[?table ?e]                            :pull {:q [:*] :ident '[?table ?e]}}
             (dx/parse-find '[(pull [:*] [?table ?e])])))
    (t/is (= {:find '[?table ?e] :mapcat? true :first? true :pull {:q [:*] :ident '[?table ?e]}}
             (dx/parse-find '[(pull [:*] [?table ?e]) .])))
    (t/is (= {:find '[?table ?e] :mapcat? true              :pull {:q [:*] :ident '[?table ?e]}}
             (dx/parse-find '[(pull [:*] [?table ?e]) ...]))))

  (t/testing "keys"
    (t/is (= {:keys [:a :b :c] :args []}
             (dx/parse-query [:keys [:a :b :c]]))))

  (t/testing "build-args-map"
    (t/is (= '{?name "Ivan"}
             (-> (dx/parse-query '[:where [?e :name ?name]
                                   :in ?name]
                                 "Ivan")
                 (dx/build-args-map))))
    (t/is (= '{?name "ivan" ?age 35}
             (-> (dx/parse-query '[:in ?name ?age] "ivan" 35)
                 (dx/build-args-map))))
    (t/is (= '{?name [:or ["ivan" "petr"]] ?age [:or [30 20]]}
             (-> (dx/parse-query '[:in [?name] [?age]] ["ivan" "petr"] [30 20])
                 (dx/build-args-map))))
    (t/is (= '[{?name "ivan" ?age 20} {?name "petr" ?age 30}]
             (-> (dx/parse-query '[:in [[?name ?age]]] [["ivan" 20] ["petr" 30]])
                 (dx/build-args-map))))
    (t/is (= '{?attr :name ?value [:or ["ivan" "petr"]]}
             (-> (dx/parse-query '[:in ?attr [?value]]
                                 :name ["ivan" "petr"])
                 (dx/build-args-map))))))

(t/deftest datalog->meander
  (t/testing "where"
    (t/is (= '{?table_?e {?e {:name "Ivan"}}}
             (-> (dx/parse-query '[:where [?e :name "Ivan"]])
                 (dx/datalog->meander))))
    (t/is (= '(meander.epsilon/and
               {?table_?e1 {?e1 {:name "Ivan"}}}
               {?table_?e2 {?e2 {:name "Petr"}}})
             (-> (dx/parse-query '[:where
                                   [?e1 :name "Ivan"]
                                   [?e2 :name "Petr"]])
                 (dx/datalog->meander))))
    (t/is (= '(meander.epsilon/and
              {?table_?e {?e {:name "Ivan", :age (meander.epsilon/some ?age)}}}
              (meander.epsilon/guard (> ?age 18)))
             (-> (dx/parse-query '[:where
                                   [?e :name "Ivan"]
                                   [?e :age ?age]
                                   [(> ?age 18)]])
                 (dx/datalog->meander))))
    (t/is (= '(meander.epsilon/and
               {?table_?e {?e {:name "Ivan" :age (meander.epsilon/some ?age)}}}
               (meander.epsilon/let [?adult (> ?age 18)]))
             (-> (dx/parse-query '[:where
                                   [?e :name "Ivan"]
                                   [?e :age ?age]
                                   [(> ?age 18) ?adult]])
                 (dx/datalog->meander)))))
  (t/testing "in & args"
    (t/is (= '{?table_?e {?e {:name (meander.epsilon/and ?name "Ivan")}}}
             (-> (dx/parse-query '[:where [?e :name ?name]
                                   :in ?name]
                                 "Ivan")
                 (dx/datalog->meander))))
    (t/is (= '{?table_?e {?e {:name (meander.epsilon/or "Ivan" "Petr")}}}
             (-> (dx/parse-query '[:where [?e :name ?name]
                                   :in [?name]]
                                 ["Ivan" "Petr"])
                 (dx/datalog->meander))))
    (t/is (= '{?table_?e {?e {:name (meander.epsilon/or "Ivan" "Petr"), :age (meander.epsilon/or 20 30)}}}
             (-> (dx/parse-query '[:where
                                   [?e :name ?name]
                                   [?e :age ?age]
                                   :in [?name] [?age]]
                                 ["Ivan" "Petr"]
                                 [20 30])
                 (dx/datalog->meander))))
    (t/is (= '(meander.epsilon/or
              {?table_?e {?e {:name (meander.epsilon/and ?name "Ivan"), :age (meander.epsilon/and ?age 20)}}}
              {?table_?e {?e {:name (meander.epsilon/and ?name "Petr"), :age (meander.epsilon/and ?age 30)}}})
             (-> (dx/parse-query '[:where
                                   [?e :name ?name]
                                   [?e :age ?age]
                                   :in    [[?name ?age]]]
                                 [["Ivan" 20]
                                  ["Petr" 30]])
                 (dx/datalog->meander))))
    (t/is (= '(meander.epsilon/and {?table_?e {?e {:friend (meander.epsilon/scan [(meander.epsilon/some ?t) (meander.epsilon/some ?f)])}}}
                                   {?table_?f {?f {:name (meander.epsilon/some ?name)}}})
             (-> (dx/parse-query '[:where
                                   [?e :friend [?t ?f]]
                                   [?f :name ?name]])
                 (dx/datalog->meander))))))

;;

(comment
  (def db (-> (reduce
               (fn [acc i] (dx/commit acc [:dx/put {:db/id i :name "David" :age (rand-int 100)}]))
               (dx/create-dx [] {::dx/with-diff? true ::dx/max-txs-count 32})
               (range 100)))))

(t/deftest db-metadata
  (let [db (-> (reduce
                (fn [acc i] (dx/commit acc [:dx/put {:db/id i :name "David"}]))
                (dx/create-dx [] {::dx/with-diff? true ::dx/max-txs-count 32})
                (range 100)))]
    (t/is (some? (some-> db meta ::dx/with-diff?)))
    (t/is (some? (some-> db meta ::dx/txs)))
    ;; TODO
    ;; (t/is (some-> db meta ::dx/txs .-txs_))
    ))

;; * apply tx

(comment
  (def db {:db/id {1 {:db/id 1 :name "Petr" :aka ["Devil"]}}}))

(t/deftest commit
  (let [db {:db/id {1 {:db/id 1 :name "Petr" :aka ["Devil"]}}}]
    (t/testing "testing put"
      (t/is (= #:db{:id {1 {:db/id 1 :name "David", :aka ["Devil"]}}}
               (dx/commit {} [[:dx/put {:db/id 1 :name "David" :aka ["Devil"]}]])))
      (t/is (= #:db{:id {1 {:db/id 1 :aka ["Tupen"]}}}
               (dx/commit {} [[:dx/put [:db/id 1] :aka ["Tupen"]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "David", :aka ["Devil"]}}}
               (dx/commit {} [[:dx/put [:db/id 1] {:name "David" :aka ["Devil"]}]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "David", :aka ["Devil"]}}}
               (dx/commit db [[:dx/put [:db/id 1] :name "David"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Tupen"]}}}
               (dx/commit db [[:dx/put [:db/id 1] :aka ["Tupen"]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"], :friend #{[:db/id 2] [:db/id 3]}}
                         2 {:db/id 2, :name "Ivan"}
                         3 {:db/id 3, :name "Lucy"}}}
               (dx/commit db [[:dx/put [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Lucy"}]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :a {:b 1, :c 2}}}}
               (dx/commit {} [[:dx/put [:db/id 1] :a {:b 1 :c 2}]])))
      (t/is (= #:db{:id {1 {:a [:db/id 2]}, 2 {:b 1, :c 2, :db/id 2}}}
               (dx/commit {} [[:dx/put [:db/id 1] :a {:b 1 :c 2 :db/id 2}]])))
      (t/is (= #:db{:id {1 {:a #{[:db/id 2] [:db/id 3]}}, 2 {:b 1, :c 2, :db/id 2}, 3 {:b 3, :c 4, :db/id 3}}}
               (dx/commit {} [[:dx/put [:db/id 1] :a [{:b 1 :c 2 :db/id 2}
                                                      {:b 3 :c 4 :db/id 3}]]]))))

    (t/testing "testing delete"
      (t/is (= {}
               (dx/commit db [[:dx/delete [:db/id 1]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr"}}}
               (dx/commit db [[:dx/delete [:db/id 1] :aka]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr"}}}
               (dx/commit db [[:dx/delete [:db/id 1] :aka "Devil"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"]}}}
               (dx/commit db [[:dx/delete [:db/id 1] :AKA "Devil"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"]}}}
               (dx/commit db [[:dx/put [:db/id 1] :friend {:db/id 2 :name "Ivan"}]
                              [:dx/delete [:db/id 2]]])
               (dx/commit db [[:dx/conj [:db/id 1] :friend {:db/id 2 :name "Ivan"}]
                              [:dx/delete [:db/id 2]]]))))

    (t/testing "testing conj"
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil" "Tupen"]}}}
               (dx/commit db [[:dx/conj [:db/id 1] :aka "Tupen"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name ["Petr" "Ivan"], :aka ["Devil"]}}}
               (dx/commit db [[:dx/conj [:db/id 1] :name "Ivan"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"], :friend [[:db/id 2]]}
                         2 {:db/id 2, :name "Ivan"}}}
               (dx/commit db [[:dx/conj [:db/id 1] :friend {:db/id 2 :name "Ivan"}]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"], :friend [[:db/id 2] [:db/id 3]]}
                         2 {:db/id 2, :name "Ivan"}
                         3 {:db/id 3, :name "Lucy"}}}
               (dx/commit db [[:dx/conj [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Lucy"}]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"] :sex ["male"]}}}
               (dx/commit db [[:dx/conj [:db/id 1] :sex "male"]]))))

    (t/testing "testing merge"
      (t/is (= #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"], :address {:city "Warsaw"}}}}
               (dx/commit db [:dx/merge [:db/id 1] :address {:city "Warsaw"}])))
      (t/is (= #:db{:id {1 {:db/id 1, :name "Petr", :aka "Tupen"}}}
               (dx/commit db [:dx/merge [:db/id 1] {:aka "Tupen"}])))
      (t/is (= #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}, 2 #:db{:id 2}}}
               (dx/commit db [:dx/merge [:db/id 2] {}]))))

    (t/testing "testing update"
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka "Tupen"}}}
               (dx/commit db [[:dx/update [:db/id 1] assoc :aka "Tupen"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil" "Tupen"]}}}
               (dx/commit db [[:dx/update [:db/id 1] :aka conj "Tupen"]]))))

    (t/testing "testing match"
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"]}}}
               (dx/commit db [[:dx/match  [:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"]}}}
               (dx/commit db [[:dx/match  [:db/id 1] :aka ["Devil"]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Tupen"]}}}
               (dx/commit db [[:dx/match  [:db/id 1] :aka ["Devil"]]
                              [:dx/put    [:db/id 1] :aka ["Tupen"]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"]}}}
               (dx/commit db [[:dx/match  [:db/id 1] :aka ["Tupen"]]
                              [:dx/delete [:db/id 1] :aka]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"] :sex :male}}}
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
  (def db (dx/commit {} (into [] (map (fn [tx] [:dx/put tx]))  (into people-docs part-docs))))
  (def db2 (dx/commit (dx/create-dx [] {::dx/with-diff? true})
                      (into [] (map (fn [tx] [:dx/put tx]))  (into people-docs part-docs)))))

(t/deftest test-pull
  (let [db (dx/commit {} (into [] (map (fn [tx] [:dx/put tx])) (into people-docs part-docs)))]
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

      (t/is (or (= {:name "Petr" :_father [[:db/id 3] [:db/id 2]]}
                   (dx/pull db [:name :_father] [:db/id 1]))
                (= {:name "Petr" :_father [[:db/id 2] [:db/id 3]]}
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
      (t/is (= {:db/id 1 :name "Petr" :aka ["Devil" "Tupen"] :child [[:db/id 2] [:db/id 3]]}
               (dx/pull db [:*] [:db/id 1])))
      (t/is (= {:db/id 2 :name "David" :_child [:db/id 1] :father [:db/id 1]}
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

    (t/testing "union"
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
            db (dx/db-with [tx])
            r (dx/pull db [{:chat/entries {:message/id [:message/id :message/text :chat.entry/timestamp]
                                              :photo/id   [:photo/id :photo/url :photo/width :photo/height :chat.entry/timestamp]}}]
                          [:chat/id 0])]
        (t/is (or (= {:chat/entries
                      [{:photo/id 0, :photo/url "photo://asdf_10x10.jkl" :photo/width 10, :photo/height 10, :chat.entry/timestamp "7890"}
                       {:message/id 1, :message/text "bar" :chat.entry/timestamp "1235"}
                       {:message/id 0, :message/text "foo" :chat.entry/timestamp "1234"}]}
                     r)
                  ;; cljs
                  (= {:chat/entries
                      [{:message/id 0, :message/text "foo" :chat.entry/timestamp "1234"}
                       {:message/id 1, :message/text "bar" :chat.entry/timestamp "1235"}
                       {:photo/id 0, :photo/url "photo://asdf_10x10.jkl" :photo/width 10, :photo/height 10, :chat.entry/timestamp "7890"}]}
                     r)))))))

;; * query

(comment
  (def db (dx/db-with [{:db/id 1, :name "Ivan" :age 15 :friend [[:db/id 2] [:db/id 3]]}
                       {:db/id 2, :name "Petr" :age 37 :friend [:db/id 3]}
                       {:db/id 3, :name "Ivan" :age 37}
                       {:db/id 4, :age 15}])))

(t/deftest test-joins
  (let [db (dx/db-with [{:db/id 1, :name "Ivan", :age 15 :friend [[:db/id 2] [:db/id 3]]}
                        {:db/id 2, :name "Petr", :age 37 :friend [:db/id 3]}
                        {:db/id 3, :name "Ivan", :age 37}
                        {:db/id 4, :age 15}])]

    (t/is (= #{[1] [2] [3]}
             (dx/q [:find ?e
                    :where [?e :name]] db)))
    (t/is (= #{[1 15] [3 37]}
             (dx/q [:find ?e ?v
                    :where
                    [?e :name "Ivan"]
                    [?e :age ?v]] db)
             (dx/q [:find ?e ?v
                    :in ?name
                    :where
                    [?e :name ?name]
                    [?e :age ?v]]
               db "Ivan")
             (let [x "Ivan"]
               (dx/q [:find ?e ?v
                      :in ?name
                      :where
                      [?e :name ?name]
                      [?e :age ?v]]
                 db x))))

    (t/is (= #{[2 2] [3 3] [1 1] [1 3] [3 1]}
             (dx/q [:find ?e1 ?e2
                    :where
                    [?e1 :name ?n]
                    [?e2 :name ?n]] db)))

    (t/is (= #{[1 1 "Ivan"] [3 3 "Ivan"] [3 2 "Petr"]}
             (dx/q [:find ?e1 ?e2 ?n
                    :where
                    [?e1 :name "Ivan"]
                    [?e1 :age      ?a]
                    [?e2 :age      ?a]
                    [?e2 :name     ?n]]
               db)))

    (t/is (= #{[2 "Petr"] [3 "Ivan"]}
             (dx/q [:find ?f ?fname
                    :where
                    [?f :name   ?fname]
                    [?e :name    "Ivan"]
                    [?e :friend  [_ ?f]]]
               db)))
    (t/is (= #{[1]}
             (dx/q [:find ?e
                    :where
                    [?e :name    "Ivan"]
                    [?e :friend  [_ ?f]]]
               db)))
    (t/is (= #{[1]}
             (dx/q [:find ?e
                    :in ?f
                    :where
                    [?e :name    "Ivan"]
                    [?e :friend  [_ ?f]]]
               db 3)))
    (t/is (= #{[1]}
             (dx/q [:find ?e
                    :where
                    [?e :name    "Ivan"]
                    :limit 1]
               db)))
    (t/is (= #{[1]}
             (dx/q [:find ?e
                    :where
                    [?e :name    "Ivan"]
                    :xf (comp (take 1))]
               db)))))

(comment
  (def db (dx/db-with [{:db/id 1
                        :name  "Ivan"
                        :aka   ["ivolga" "pi"]}
                       {:db/id 2
                        :name  "Petr"
                        :aka   ["porosenok" "pi"]}])))

(t/deftest test-q-many
  (let [db (dx/db-with [{:db/id 1
                         :name  "Ivan"
                         :aka   ["ivolga" "pi"]}
                        {:db/id 2
                         :name  "Petr"
                         :aka   ["porosenok" "pi"]}])]
    (t/is (= #{["Ivan" "Petr"] ["Petr" "Ivan"] ["Petr" "Petr"] ["Ivan" "Ivan"]}
             (dx/q [:find ?n1 ?n2
                    :where
                    [?e1 :aka (m/scan ?x)]
                    [?e2 :aka (m/scan ?x)]
                    [?e1 :name ?n1]
                    [?e2 :name ?n2]]
               db)))))

(comment
  (def db (dx/db-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                       {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                       {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])))

(t/deftest test-q-in
  (let [db (dx/db-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                        {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                        {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])]

    (t/is (= #{[1] [3]}
             (dx/q [:find  ?e
                    :in    ?attr ?value
                    :where [?e ?attr ?value]]
               db :name "Ivan")))

    (t/is (= #{[1] [2] [3]}
             (dx/q [:find  ?e
                    :in    ?attr [?value]
                    :where [?e ?attr ?value]]
               db :name ["Ivan" "Petr"])))

    (t/is (= #{[2] [3]}
             (dx/q [:find ?e
                    :in    ?attr ?value
                    :where [?e ?attr ?value]]
               db :age 37)))

    (t/is (= #{[1 "ivan@mail.ru"]
               [2 "petr@gmail.com"]
               [3 "ivan@mail.ru"]}
             (dx/q [:find ?e ?email
                    :in [[?n ?email]]
                    :where
                    [?e :name ?n]
                    [?e :email ?email]]
               db
               [["Ivan" "ivan@mail.ru"]
                ["Petr" "petr@gmail.com"]])))))

(t/deftest test-q-keys
  (let [db (dx/db-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                        {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                        {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])]
    (t/is (= #{{:name "Ivan" :email "ivan@mail.ru" :age 15}
               {:name "Petr" :email "petr@gmail.com" :age 37}
               {:name "Ivan" :email "ivan@mail.ru" :age 37}}
             (dx/q [:find ?name ?email ?age
                    :keys [:name :email :age]
                    :where
                    [?e :name ?name]
                    [?e :age ?age]
                    [?e :email ?email]]
               db)))))

(comment
  (def db (dx/db-with
            [{:db/id 1 :follow [:db/id 2]}
             {:db/id 2 :follow [[:db/id 3] [:db/id 4]]}
             {:db/id 3 :follow [:db/id 4]}
             {:db/id 4 :follow [:db/id 6]}
             {:db/id 5 :follow [:db/id 3]}])))

(t/deftest test-q-rules
  (let [db (dx/db-with
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
  (def db (dx/db-with [{:db/id 1 :name "Ivan" :age 10}
                       {:db/id 2 :name "Ivan" :age 20}
                       {:db/id 3 :name "Oleg" :age 10}
                       {:db/id 4 :name "Oleg" :age 20}
                       {:db/id 5 :name "Ivan" :age 10}
                       {:db/id 6 :name "Ivan" :age 20}])))

(t/deftest test-q-or
  (let [db (dx/db-with [{:db/id 1 :name "Ivan" :age 10}
                        {:db/id 2 :name "Ivan" :age 20}
                        {:db/id 3 :name "Oleg" :age 10}
                        {:db/id 4 :name "Oleg" :age 20}
                        {:db/id 5 :name "Ivan" :age 10}
                        {:db/id 6 :name "Ivan" :age 20}])]
    (t/is (= #{[4] [3] [5] [1]}
             (dx/q [:find ?e
                    :where
                    (or [?e :name "Oleg"]
                        [?e :age 10])]
               db)))
    (t/is (= #{[4] [3]}
             (dx/q [:find ?e
                    :where
                    (or [?e :name "Oleg"]
                        [?e :age 30])]
               db)))
    (t/is (= #{}
             (dx/q [:find ?e
                    :where
                    (or [?e :name "Petr"]
                        [?e :age 30])]
               db)))
    (t/is (= #{[5] [1]}
             (dx/q [:find ?e
                    :where
                    [?e :name "Ivan"]
                    (or [?e :name "Oleg"]
                        [?e :age 10])]
               db)))
    (t/is (= #{[5] [1] [4]}
             (dx/q [:find ?e
                    :where
                    [?e :age ?a]
                    (or (and [?e :name "Ivan"]
                             [1 :age ?a])
                        (and [?e :name "Oleg"]
                             [2 :age ?a]))]
               db)))
    (t/is (= #{[5] [1] [4]}
             (dx/q [:find ?e
                    :where
                    (or (and [?e :name "Ivan"]
                             [1 :age ?a])
                        (and [?e :name "Oleg"]
                             [2 :age ?a]))
                    [?e :age ?a]]
               db)))
    ;; diffrent order
    (t/is (= #{[5] [1] [4]}
             (dx/q [:find ?e
                    :where
                    (or (and [1 :age ?a]
                             [?e :name "Ivan"])
                        (and [?e :name "Oleg"]
                             [2 :age ?a]))
                    [?e :age ?a]]
               db)))))

;; crux

#?(:clj
   (def bond-db
     (let [data (->> (read-string (slurp "./resources/james-bond.edn"))
                     (mapv (fn [m]
                             (-> m
                                 (update :film/director   (partial vector :db/id))
                                 (update :film/bond       (partial vector :db/id))
                                 (update :film/vehicles   (fn [xs] (mapv (partial vector :db/id) xs)))
                                 (update :film/box        (partial vector :db/id))
                                 (update :film/bond-girls (fn [xs] (mapv (partial vector :db/id) xs)))
                                 (update :film/box        (partial vector :db/id))))))]
       (-> (dx/create-dx {} {:with-diff? true})
           (dx/commit (into [] (map (fn [tx] [:dx/put tx])) data))))))

#?(:clj
   (t/testing "project fn"
     (t/is (= #:film{:name "Spectre", :year "2015"}
              (dx/pull bond-db [:film/name :film/year] [:db/id :spectre])))))

#?(:clj
   (t/deftest query-pull
     (t/testing "query"
       (t/is (= #{{}}
                (dx/q [:find (pull [] [?table ?e])
                       :where [?table ?e :vehicle/brand "Aston Martin"]]
                  bond-db)))

       (let [expected #{{:vehicle/brand "Aston Martin", :vehicle/model "DB10"}
                        {:vehicle/brand "Aston Martin", :vehicle/model "DBS V12"}
                        {:vehicle/brand "Aston Martin", :vehicle/model "DB5"}
                        {:vehicle/brand "Aston Martin", :vehicle/model "V12 Vanquish"}
                        {:vehicle/brand "Aston Martin", :vehicle/model "V8 Vantage Volante"}
                        {:vehicle/brand "Aston Martin", :vehicle/model "DBS"}}]
         (t/is (= expected
                  (dx/q [:find  (pull [:vehicle/brand :vehicle/model] [?table ?e])
                         :where [?table ?e :vehicle/brand "Aston Martin"]]
                    bond-db)))))))

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

(t/deftest gh-8
  ;; https://github.com/ribelo/doxa/issues/8
  (let [person1 {:person/id 1
                 :gender    "MALE"
                 :name      "Bob"
                 :face      {:eyes "BLUE"}}
        person2 {:person/id 2
                 :gender    "FEMALE"
                 :name      "Joanne"
                 :face      {:eyes "GREEN"}}
        db      (dx/commit (dx/create-dx) [[:dx/put person1]
                                           [:dx/put person2]])]
    (t/is (= #{[1 "Bob" "BLUE"]}
             (dx/q [:find ?person ?name ?eye-color
                    :in ?table ?color
                    :where
                    [?table ?person :name ?name]
                    [?table ?person :face {:eyes ?eye-color}]
                    [(= ?color ?eye-color)]]
               db :person/id "BLUE")))))

(t/deftest gh-14
  (let [db  (dx/create-dx [{:db/id 1, :name "Ivan" :age 15}
                           {:db/id 2, :name "Petr" :age 37}])
        age 15]
    (t/is (= #{["Ivan"]}
             (dx/q [:find ?name
                    :in ?age
                    :where
                    [?e :name ?name]
                    [?e :age ?age]]
               db ~age)))))

(t/deftest gh-16
  ;; https://github.com/ribelo/doxa/issues/16
  (let [db (dx/create-dx [{:db/id 1 :name "ivan" :cars [{:db/id 10 :name "tesla"}
                                                        {:db/id 11 :name "ferrari"}]}
                          {:db/id 2 :name "petr" :cars [{:db/id 10 :name "peugot"}]}
                          {:db/id 3 :name "mike" :cars {:db/id 10 :name "peugot"}}
                          {:db/id 4 :name "mike" :cars []}])]

    (t/is (vector? (:cars (dx/pull db [:name {:cars [:name]}] [:db/id 2])))
          "pull returns vector when 1 entity in join")))

(t/deftest gh-17
  ;; https://github.com/ribelo/doxa/issues/17
  (let [db (dx/create-dx [{:db/id 1 :name "ivan" :car {:db/id 10 :name "tesla"}}])]
    (t/is (map? (:car (dx/pull db [:name {:car [:name]}] [:db/id 1]))))))

(defmacro generate-matched-tests [datom]
  (m/rewrite datom
    (m/or
     [(m/or (m/and ?table (m/not (m/pred dx/qsymbol?))) (m/let [?table :db/id]))
      (m/or (m/and ?e     (m/not (m/pred dx/qsymbol?))) (m/let [?e          1]))
      (m/or (m/and ?a     (m/not (m/pred dx/qsymbol?))) (m/let [?a         :a]))
      (m/or (m/and ?v     (m/not (m/pred dx/qsymbol?))) (m/let [?v          1]))]
     (m/and
      (m/let [?table :db/id])
      [(m/or (m/and ?e     (m/not (m/pred dx/qsymbol?))) (m/let [?e         1]))
       (m/or (m/and ?a     (m/not (m/pred dx/qsymbol?))) (m/let [?a        :a]))
       (m/or (m/and ?v     (m/not (m/pred dx/qsymbol?))) (m/let [?v         1]))]))
    (do (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms [[[~?table] :+ {~?e {~?a ~?v}}]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table] :r {~?e {~?a ~?v}}]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table ~?e] :+ {~?a ~?v}]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table ~?e] :r {~?a ~?v}]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table ~?e ~?a] :+ ~?v]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table ~?e ~?a] :r ~?v]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table] :-]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table ~?e] :-]])
                ~datom)))
        (t/is (true?
               (dx/-datoms-match-datom?
                (dx/-edits->datoms  [[[~?table ~?e ~?a] :-]])
                ~datom))))))

(comment
  (def db (dx/create-dx [] {::dx/with-diff? true})))

(t/deftest match-changes
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
  (def conn_ (atom (dx/create-dx [] {::dx/with-diff? true}))))

(t/deftest cached-query
  (let [conn_ (atom (dx/create-dx [] {::dx/with-diff? true}))]
    (dx/commit! conn_ [:dx/put [:db/id 1] {:name "ivan"}])
    (Thread/sleep 10)
    (t/is (true?  (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))
    (t/is (false? (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))
    (dx/commit! conn_ [:dx/put [:db/id 2] {:name "ivan"}])
    (Thread/sleep 10)
    (t/is (true?  (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))
    (dx/commit! conn_ [:dx/put [:dog/id 1] {:name "pixel"}])
    (t/is (false? (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))
    (dx/commit! conn_ [:dx/put [:db/id 3] {:name "ivan"}])
    (t/is (true?  (::dx/fresh? (meta ^{::dx/cache? true} (dx/q [:find ?e ... :where [?e :name "ivan"]] @conn_)))))))

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
  (let [db1 (dx/create-dx [] {::dx/with-diff? true})
        people (fn [db]
                 (->
                   ^{::dx/cache ::people} ; comment this out to see correct behaviour
                   (dx/q [:find ?name ...
                          :where
                          [?e :name ?name]]
                     db)))
        r1 (people db1)
        db2 (->> [{:id   "20"
                   :name "Chris"
                   :age  15}]
                 (vector :dx/put)
                 (dx/commit db1))
        r2 (people db2)]
    (t/is (= #{} r1))
    (t/is (= #{"Chris"} r2))))

(t/deftest gh-26
  (let [db (dx/create-dx [{:id 1
                           :name "Steve"
                           :languages/spoken [{:id 2
                                               :name "english"}
                                              {:id 3
                                               :name "dutch"}]
                           :languages/computer [{:id 4
                                                 :name "clojure"}
                                                {:id 5
                                                 :name "typescript"}]}])]
    (t/is (= {:id 1
              :languages/spoken [{:id 2, :name "english"} {:id 3, :name "dutch"}]
              :languages/computer [{:id 4, :name "clojure"} {:id 5, :name "typescript"}]}
             (dx/-pull db [:id {:languages/spoken [:*]} {:languages/computer [:*]}] [:id 1])))))

(t/deftest gh-27 []
  (let [db (dx/create-dx [{:id       1
                           :name   "Olympics"
                           :sports [{:id   2
                                     :name "Boardercross"}
                                    {:id   3
                                     :name "Curling"}]}]
                         {::dx/with-diff? true})]
    (t/is (= {:id 1, :sports [{:id 2, :name "Boardercross"} {:id 3, :name "Curling"}]}
             ^{::dx/cache :sports-event}
             (dx/pull db [:id {:sports [:id :name]}] [:id 1])))))
