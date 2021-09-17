(ns ribelo.doxa-test
  (:require
   [ribelo.doxa :as dx]
   [clojure.test :as t]
   [taoensso.encore :as enc]
   [meander.epsilon :as m]
   [meander.interpreter.epsilon :as mi]))

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
    (t/is (= '[{?attr :name ?value [:or ["ivan" "petr"]]}]
             (-> (dx/parse-query '[:in ?attr [?value]]
                                 :name ["ivan" "petr"])
                 (dx/build-args-map)))))

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
    (t/is (= '[{?attr :name ?value [:or ["ivan" "petr"]]}]
             (-> (dx/parse-query '[:in ?attr [?value]]
                                 :name ["ivan" "petr"])
                 (dx/build-args-map))))))

(t/deftest datalog->meander
  (t/testing "where"
    (t/is (= `{~'_ {~'?e {:name "Ivan"}}}
             (-> (dx/parse-query '[:where [?e :name "Ivan"]])
                 (dx/datalog->meander))))
    (t/is (= `{~'_ {~'?e1 {:name "Ivan"}
                    ~'?e2 {:name "Petr"}}}
             (-> (dx/parse-query '[:where
                                   [?e1 :name "Ivan"]
                                   [?e2 :name "Petr"]])
                 (dx/datalog->meander))))
    (t/is (= `(m/and {~'_ {~'?e {:name "Ivan" :age (m/some ~'?age)}}}
                     (m/guard ~'(> ?age 18)))
             (-> (dx/parse-query '[:where
                                   [?e :name "Ivan"]
                                   [?e :age ?age]
                                   [(> ?age 18)]])
                 (dx/datalog->meander))))
    (t/is (= `(m/and {~'_ {~'?e {:name "Ivan" :age (m/some ~'?age)}}}
                     (m/let [~'?adult ~'(> ?age 18)]))
             (-> (dx/parse-query '[:where
                                   [?e :name "Ivan"]
                                   [?e :age ?age]
                                   [(> ?age 18) ?adult]])
                 (dx/datalog->meander)))))
  (t/testing "in & args"
    (t/is (= `{~'_ {~'?e {:name (m/and ~'?name "Ivan")}}}
             (-> (dx/parse-query '[:where [?e :name ?name]
                                   :in ?name]
                                 "Ivan")
                 (dx/datalog->meander))))
    (t/is (= `{~'_ {~'?e {:name (m/or "Ivan" "Petr")}}}
             (-> (dx/parse-query '[:where [?e :name ?name]
                                   :in [?name]]
                                 ["Ivan" "Petr"])
                 (dx/datalog->meander))))
    (t/is (= `{~'_ {~'?e {:name (m/or "Ivan" "Petr"), :age (m/or 20 30)}}}
             (-> (dx/parse-query '[:where
                                   [?e :name ?name]
                                   [?e :age ?age]
                                   :in [?name] [?age]]
                                 ["Ivan" "Petr"]
                                 [20 30])
                 (dx/datalog->meander))))
    (t/is (= `(m/or {~'_ {~'?e {:name (m/and ~'?name "Ivan")
                                :age  (m/and ~'?age 20)}}}
                    {~'_ {~'?e {:name (m/and ~'?name "Petr")
                                :age  (m/and ~'?age 30)}}})
             (-> (dx/parse-query '[:where
                                   [?e :name ?name]
                                   [?e :age ?age]
                                   :in    [[?name ?age]]]
                                 [["Ivan" 20]
                                  ["Petr" 30]])
                 (dx/datalog->meander))))
    (t/is (= `{~'_ {~'?e {:friend (m/or [~'?t ~'?f] (m/scan [~'?t ~'?f]))}
                    ~'?f {:name   (m/some ~'?name)}}}
             (-> (dx/parse-query '[:where
                                   [?e :friend [?t ?f]]
                                   [?f :name ?name]])
                 (dx/datalog->meander))))))

;; * apply tx

(comment
  (def db {:db/id {1 {:db/id 1 :name "Petr" :aka ["Devil"]}}}))

(t/deftest commit
  (let [db {:db/id {1 {:db/id 1 :name "Petr" :aka ["Devil"]}}}]
    (t/testing "testing put"
      (t/is (= #:db{:id {1 {:db/id 1 :name "David", :aka ["Devil"]}}}
               (dx/commit {} [[:dx/put {:db/id 1 :name "David" :aka ["Devil"]}]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "David", :aka ["Devil"]}}}
               (dx/commit {} [[:dx/put [:db/id 1] {:name "David" :aka ["Devil"]}]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "David", :aka ["Devil"]}}}
               (dx/commit db [[:dx/put [:db/id 1] :name "David"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Tupen"]}}}
               (dx/commit db [[:dx/put [:db/id 1] :aka ["Tupen"]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"], :friend [[:db/id 2] [:db/id 3]]}
                         2 {:db/id 2, :name "Ivan"}
                         3 {:db/id 3, :name "Lucy"}}}
               (dx/commit db [[:dx/put [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Lucy"}]]])))
      (t/is (= #:db{:id {1 {:a {:b 1, :c 2}}}}
               (dx/commit {} [[:dx/put [:db/id 1] :a {:b 1 :c 2}]])))
      (t/is (= #:db{:id {1 {:a [:db/id 2]}, 2 {:b 1, :c 2, :db/id 2}}}
               (dx/commit {} [[:dx/put [:db/id 1] :a {:b 1 :c 2 :db/id 2}]])))
      (t/is (= #:db{:id {1 {:a [[:db/id 2] [:db/id 3]]}, 2 {:b 1, :c 2, :db/id 2}, 3 {:b 3, :c 4, :db/id 3}}}
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
;; => #'ribelo.doxa-test/commit


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
  (def db (dx/commit {} (into [] (map (fn [tx] [:dx/put tx]))  (into people-docs part-docs)))))

(t/deftest test-pull
  (let [db (dx/commit {} (into [] (map (fn [tx] [:dx/put tx])) (into people-docs part-docs)))]
    (t/testing "test pull attr"
      (t/is (= {:name "Petr" :aka ["Devil" "Tupen"]}
               (dx/pull db [:name :aka] [:db/id 1])
               (m/find db
                 {:db/id {1 {:name ?name :aka ?aka}}}
                 {:name ?name :aka ?aka})))

      (t/is (= {:name "Matthew" :father [:db/id 3] :db/id 6}
               (dx/pull db [:name :father :db/id] [:db/id 6])
               (m/find db
                 {:db/id {6 {:name ?name :father ?father}}}
                 {:name ?name :father ?father :db/id 6})))

      (t/is (= {:name "Matthew" :father {:name "Thomas"} :db/id 6}
               (dx/pull db [:name :db/id {:father [:name]}] [:db/id 6])
               (m/find db
                 (m/and {:db/id {6 {:name ?name :father [?table ?id]}}}
                        {?table {?id {:name ?father-name}}})
                 {:name ?name :father {:name ?father-name} :db/id 6})))

      (t/is (= [{:name "Petr"}
                {:name "Elizabeth"}
                {:name "Eunan"}
                {:name "Rebecca"}]
               (dx/pull db [:name] [[:db/id 1] [:db/id 5] [:db/id 7] [:db/id 9]])
               (mapv (fn [[table id]]
                       (m/find db
                         {~table {~id {:name ?name}}}
                         {:name ?name}))
                     [[:db/id 1] [:db/id 5] [:db/id 7] [:db/id 9]]))))

    (t/testing "test pull reverse attr"
      (t/is (= {:name "David" :_child [:db/id 1]}
               (dx/pull db [:name :_child] [:db/id 2])
               (m/find db
                 (m/and {?table {?id {:child (m/or [:db/id 2] (m/scan [:db/id 2]))}}}
                        {:db/id {2 {:name ?name}}})
                 {:name ?name :_child [?table ?id]})))

      (t/is (= {:name "David" :_child {:name "Petr"}}
               (dx/pull db [:name {:_child [:name]}] [:db/id 2])
               (m/find db
                 (m/and {?table {?id {:name  ?name1
                                      :child (m/or [:db/id 2] (m/scan [:db/id 2]))}}}
                        {:db/id {2 {:name ?name2}}})
                 {:name ?name2 :_child {:name ?name1}})))

      (t/testing "reverse non-component references yield collections"
        (t/is (= {:name "Thomas" :_father [:db/id 6]}
                 (dx/pull db [:name :_father] [:db/id 3])
                 (m/find db
                   (m/and {?table {?id {:father (m/or [:db/id 3] (m/scan [:db/id 3]))}}}
                          {:db/id {3 {:name ?name}}})
                   {:name ?name :_father [?table ?id]})))

        (t/is (= {:name "Petr" :_father [[:db/id 3] [:db/id 2]]}
                 (dx/pull db [:name :_father] [:db/id 1])))

        (t/is (= {:name "Thomas" :_father {:name "Matthew"}}
                 (dx/pull db [:name {:_father [:name]}] [:db/id 3])))

        (t/is (= {:name "Petr" :_father [{:name "Thomas"} {:name "David"}]}
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
        (t/is (= {:name "Petr" :child []}
                 (dx/pull db [:name {:child [:foo]}] [:db/id 1]))))

      (t/testing "Map specs can override component expansion"
        (t/is (= {:part-name "Part A", :_part-of [{:part-name "Part A.B"} {:part-name "Part A.A"}]}
                 (dx/pull db [:part-name {:_part-of [:part-name]}] [:db/id 10])))))

    (t/testing "eql"
      (t/is (= {:name "Petr"}
               (dx/pull db {[:db/id 1] [:name]})
               (dx/pull db [:name] [:db/id 1]))))))

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

    (t/is (= [[1] [2] [3]]
             (dx/q [:find ?e
                    :where [?e :name]] db)
             (m/search db
               {_ {?e {:name (m/some)}}} [?e])))

    (t/is (= [[1 15] [3 37]]
             (dx/q [:find ?e ?v
                    :where
                    [?e :name "Ivan"]
                    [?e :age ?v]] db)
             (m/search db
               {_ {?e {:name "Ivan"
                       :age  ?v}}}
               [?e ?v])))

    (t/is (= [[1 3] [3 1]]
             (dx/q [:find ?e1 ?e2
                    :where
                    [?e1 :name ?n]
                    [?e2 :name ?n]] db)
             (m/search db
               {_ {?e1 {:name (m/some ?n)}
                   ?e2 {:name (m/some ?n)}}}
               [?e1 ?e2])))

    (t/is (= [[3 2 "Petr"]]
             (dx/q [:find ?e1 ?e2 ?n
                    :where
                    [?e1 :name "Ivan"]
                    [?e1 :age      ?a]
                    [?e2 :age      ?a]
                    [?e2 :name     ?n]]
               db)
             (m/search db
               {_ {?e1 {:name "Ivan"
                        :age  ?a}
                   ?e2 {:name (m/some ?n)
                        :age  ?a}}}
               [?e1 ?e2 ?n])))

    (t/is (= [[2 "Petr"] [3 "Ivan"]]
             (dx/q [:find ?f ?fname
                    :where
                    [?e :name    "Ivan"]
                    [?e :friend  [_ ?f]]
                    [?f :name   ?fname]]
               db)
             (m/search db
               {_ {?e1 {:name "Ivan"
                        :friend (m/scan [_ ?f])}
                   ?f  {:name (m/some ?fname)}}}
               [?f ?fname])))))

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
    (t/is (= [["Ivan" "Petr"]
              ["Petr" "Ivan"]]
             (dx/q [:find ?n1 ?n2
                    :where
                    [?e1 :aka (m/scan ?x)]
                    [?e2 :aka (m/scan ?x)]
                    [?e1 :name ?n1]
                    [?e2 :name ?n2]]
               db)
             (m/search db
               {_ {?e1 {:aka  (m/scan ?x)
                        :name (m/some ?n1)}
                   ?e2 {:aka  (m/scan ?x)
                        :name (m/some ?n2)}}}
               [?n1 ?n2])))))

(comment
  (def db (dx/db-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                       {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                       {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])))

(t/deftest test-q-in
  (let [db (dx/db-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                        {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                        {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])]

    (t/is (= [[1] [3]]
             (dx/q [:find  ?e
                    :in    ?attr ?value
                    :where [?e ?attr ?value]]
               db :name "Ivan")
             (m/search db
               {_ {?e {:name "Ivan"}}} [?e])))

    (t/is (= [[1] [2] [3]]
             (dx/q [:find  ?e
                    :in    ?attr [?value]
                    :where [?e ?attr ?value]]
               db :name ["Ivan" "Petr"])
             (m/search db
               (m/or {_ {?e {:name "Ivan"}}}
                     {_ {?e {:name "Petr"}}}) [?e])))

    (t/is (= [[2] [3]]
             (dx/q [:find ?e
                    :in    ?attr ?value
                    :where [?e ?attr ?value]]
               db :age 37)
             (m/search db
               {_ {?e {:age 37}}} [?e])))

    (t/is (= [[1 "ivan@mail.ru"]
              [2 "petr@gmail.com"]
              [3 "ivan@mail.ru"]]
             (dx/q [:find ?e ?email
                    :in [[?n ?email]]
                    :where
                    [?e :name ?n]
                    [?e :email ?email]]
               db
               [["Ivan" "ivan@mail.ru"]
                ["Petr" "petr@gmail.com"]])
             (m/search db
               (m/or {_ {?e {:name (m/and ?name "Ivan")
                             :email (m/and ?email "ivan@mail.ru")}}}
                     {_ {?e {:name (m/and ?name "Petr")
                             :email (m/and ?email "petr@gmail.com")}}})
               [?e ?email])))))

(t/deftest test-q-keys
  (let [db (dx/db-with [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                        {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                        {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}])]
    (t/is (= [{:name "Ivan", :email "ivan@mail.ru", :age 15}
              {:name "Petr", :email "petr@gmail.com", :age 37}
              {:name "Ivan", :email "ivan@mail.ru", :age 37}]
             (dx/q [:find ?name ?email ?age
                    :keys [:name :email :age]
                    :where
                    [?e :name ?name]
                    [?e :age ?age]
                    [?e :email ?email]]
               db)
             (m/search db
               {_ {?e {:name (m/some ?name)
                       :age  (m/some ?age)
                       :email (m/some ?email)}}}
               {:name ?name :age ?age :email ?email})))))

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
       (dx/commit {} (into [] (map (fn [tx] [:dx/put tx])) data)))))

#?(:clj
   (t/testing "project fn"
     (t/is (= #:film{:name "Spectre", :year "2015"}
              (dx/pull bond-db [:film/name :film/year] [:db/id :spectre])))))

#?(:clj
   (t/deftest query-pull
     (t/testing "query"
       (t/is (= [{} {} {} {} {} {}]
                (dx/q [:find (pull [] [?table ?e])
                       :where [?table ?e :vehicle/brand "Aston Martin"]]
                  bond-db)))

       (let [expected [{:vehicle/brand "Aston Martin", :vehicle/model "DB10"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "DBS V12"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "DB5"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "V12 Vanquish"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "V8 Vantage Volante"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "DBS"}]]
         (t/is (= expected
                  (dx/q [:find  (pull [:vehicle/brand :vehicle/model] [?table ?e])
                         :where [?table ?e :vehicle/brand "Aston Martin"]]
                    bond-db)
                  (m/search bond-db
                    {_ {_ {:vehicle/brand (m/and ?brand "Aston Martin")
                           :vehicle/model ?model}}}
                    {:vehicle/brand ?brand
                     :vehicle/model ?model})))))))

(t/deftest gh-7
  ;; https://github.com/ribelo/doxa/issues/7
  (let [entity-id  "6037b7a5-5a77-48a3-a294-8dba786d8e9d"
        gql-entity {:__typename "release"
                    :id         entity-id
                    :created    "2021-05-10T09:39:28"
                    :release/id entity-id}
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
        db (dx/commit (dx/create-dx) [[:dx/put person1]
                                      [:dx/put person2]])]
    (t/is (= [[1 "Bob" "BLUE"]]
             (dx/q [:find ?person ?name ?eye-color
                    :in ?table ?color
                    :where
                    [?table ?person :name ?name]
                    [?table ?person :face {:eyes ?eye-color}]
                    [(= ?color ?eye-color)]]
               db :person/id "BLUE")))))

(t/deftest gh-14
  (let [db (dx/create-dx [{:db/id 1, :name "Ivan" :age 15}
                          {:db/id 2, :name "Petr" :age 37}])
        age 15]
    (t/is (= [["Ivan"]]
             (dx/q [:find ?name
                    :in ?age
                    :where
                    [?e :name ?name]
                    [?e :age ?age]]
               db age)))))

(comment
  (m/rewrites
    db
    {_
     {?e
      {:name (m/some ?name),
       :age (m/some (unquote age))}}}
    [?name]))

(t/deftest gh-16
  ;; https://github.com/ribelo/doxa/issues/16
  (let [db (dx/create-dx [{:db/id 1 :name "ivan" :cars [{:db/id 10 :name "tesla"}
                                                        {:db/id 11 :name "ferrari"}]}
                          {:db/id 2 :name "petr" :cars [{:db/id 10 :name "peugot"}]}
                          {:db/id 3 :name "mike" :cars []}])]
    (t/is (vector? (:cars (dx/pull db [:name {:cars [:name]}] [:db/id 1])))
          "pull returns vector when N entities in join")
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
               (dx/tx-match-datom?
                [[~?table] :+ {~?e {~?a ~?v}}]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table] :r {~?e {~?a ~?v}}]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table ~?e] :+ {~?a ~?v}]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table ~?e] :r {~?a ~?v}]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table ~?e ~?a] :+ ~?v]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table ~?e ~?a] :r ~?v]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table] :-]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table ~?e] :-]
                ~datom)))
        (t/is (true?
               (dx/tx-match-datom?
                [[~?table ~?e ~?a] :-]
                ~datom))))))

(comment
  (def db (dx/create-dx [] {:with-diff? true})))

(t/deftest match-changes
  (let [db (dx/create-dx [] {:with-diff? true})]
    (t/testing "generated mached tests"
      (doseq [table ['?table :db/id]
              e     ['?e          1]
              a     ['?a         :a]
              v     ['?v          1]]
        (generate-matched-tests [table e a v])))
    (t/testing "??"
      (t/is (true?
             (-> (dx/commit db [:dx/put [:db/id 1] :name "ivan"])
                 dx/last-tx
                 (dx/tx-match-datom?
                  ['?table '?e '?a 'v]))))
      (t/is (true?
             (-> (dx/commit db [:dx/put [:db/id 1] :name "ivan"])
                 dx/last-tx
                 (dx/tx-match-datom?
                  [:db/id '?e '?a '?v]))))
      (t/is (true?
             (-> (dx/commit db [:dx/put [:db/id 1] :name "ivan"])
                 dx/last-tx
                 (dx/tx-match-datom?
                  ['?table '?e '?a "ivan"]))))
      (t/is (false?
             (-> (dx/commit db [:dx/put [:db/id 1] :name "ivan"])
                 dx/last-tx
                 (dx/tx-match-datom?
                  ['?table '?e '?a "petr"])))))))

;; (def team {:team/id 1
;;            :name    "Red Bull"
;;            :owner   {:person/id 1000
;;                      :name      "Helmut Marko"
;;                      :gender    :male}})
;; (def jack {:person/id 1001
;;            :name      "Jack Brabham"
;;            :gender    :male})

;; (def partial-jack (dissoc jack :gender))
;; (def db1 (dx/create-dx [team jack]))
;;                                         ; want to change the owner ref

;;                                         ; this works but...
;; (def db2 (dx/commit db1 [[:dx/put [:team/id 1] :owner partial-jack]]))

;;                                         ; would prefer to use a lookup ref for the update so not concerned about changing the referenced entity
;;                                         ; this fails.
;;                                         ;db2 (dx/commit db1 [[:dx/put [:team/id 1] :owner [:person/id 1001]]])
;; (dx/commit db1 [[:dx/put [:team/id 1] :owner [:person/id 1002]]])
;; ;; => {:person/id {1000 {:person/id 1000, :name "Helmut Marko", :gender :male},
;; ;;                 1001 {:person/id 1001, :name "Jack Brabham", :gender :male}},
;; ;;     :team/id {1 {:team/id 1, :name "Red Bull", :owner [:person/id 1001]}}}

;; (dx/pull db1 [:* {:owner [:*]}] [:team/id 1])
;;                                         ; gender not lost for new owner. that's good but it's a bit magical.
;; (dx/pull db2 [:* {:owner [:*]}] [:team/id 1])
;; => {:team/id 1, :name "Red Bull", :owner {:person/id 1001, :name "Jack Brabham"}}
