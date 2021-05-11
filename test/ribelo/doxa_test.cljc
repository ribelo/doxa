(ns ribelo.doxa-test
  (:require
   [ribelo.doxa :as dx]
   [clojure.test :as t]
   [taoensso.encore :as enc]
   [meander.epsilon :as m]
   [meander.interpreter.epsilon :as mi]))

;; * datalog->meander

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
    (t/is (= `(m/and {~'_ {~'?e {:friend (m/scan [~'?t ~'?f])}}}
                     {~'_ {~'?f {:name   (m/some ~'?name)}}})
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
               (dx/commit db [[:dx/put [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Lucy"}]]]))))

    (t/testing "testing delete"
      (t/is (= {}
               (dx/commit db [[:dx/delete [:db/id 1]]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr"}}}
               (dx/commit db [[:dx/delete [:db/id 1] :aka]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr"}}}
               (dx/commit db [[:dx/delete [:db/id 1] :aka "Devil"]])))
      (t/is (= #:db{:id {1 {:db/id 1 :name "Petr", :aka ["Devil"]}}}
               (dx/commit db [[:dx/delete [:db/id 1] :AKA "Devil"]]))))

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
   {:db/id 2, :name "David", :father [[:db/id 1]]}
   {:db/id 3, :name "Thomas", :father [[:db/id 1]]}
   {:db/id 4, :name "Lucy" :friend [[:db/id 5]], :enemy [[:db/id 6]]}
   {:db/id 5, :name "Elizabeth" :friend [[:db/id 6]], :enemy [[:db/id 7]]}
   {:db/id 6, :name "Matthew", :father [[:db/id 3]], :friend [[:db/id 7]], :enemy [[:db/id 8]]}
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

(def test-db
  (dx/commit {} (into [] (map (fn [tx] [:dx/put tx])) (into people-docs part-docs))))

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
                 {:db/id {6 {:name ?name :father [?father]}}}
                 {:name ?name :father ?father :db/id 6})))

      (t/is (= {:name "Matthew" :father {:name "Thomas"} :db/id 6}
               (dx/pull db '[:name :db/id {:father [:name]}] [:db/id 6])
               (m/match db
                 (m/and {:db/id {6 {:name ?name :father [[?table ?id]]}}}
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
               (dx/pull db [:name :_child] [:db/id 2])))

      (t/is (= {:name "David" :_child {:name "Petr"}}
               (dx/pull db [:name {:_child [:name]}] [:db/id 2])))

      (t/testing "reverse non-component references yield collections"
        (t/is (= {:name "Thomas" :_father [:db/id 6]}
                 (dx/pull db '[:name :_father] [:db/id 3])))

        (t/is (= {:name "Petr" :_father [[:db/id 3] [:db/id 2]]}
                 (dx/pull db '[:name :_father] [:db/id 1])))

        (t/is (= {:name "Thomas" :_father {:name "Matthew"}}
                 (dx/pull db '[:name {:_father [:name]}] [:db/id 3])))

        (t/is (= {:name "Petr" :_father [{:name "Thomas"} {:name "David"}]}
                 (dx/pull db '[:name {:_father [:name]}] [:db/id 1])))))

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
  (def db (dx/db-with [{:db/id 1, :name "Ivan", :age 15 :friend [[:db/id 2] [:db/id 3]]}
                        {:db/id 2, :name "Petr", :age 37 :friend [:db/id 3]}
                        {:db/id 3, :name "Ivan", :age 37}
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
               {_ {?e {:name "Ivan"}}} [?e])
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

;; crux

#?(:clj
   (def bond-db
     (let [data (read-string (slurp "./resources/james-bond.edn"))]
       (dx/commit {} (into [] (map (fn [tx] [:dx/put tx])) data)))))

#?(:clj
   (t/testing "project fn"
     (t/is (= #:film{:name "Spectre", :year "2015"}
              (dx/pull bond-db [:film/name :film/year] [:db/id :spectre])))))

#?(:clj
   (t/deftest query-pull
     (t/testing "query"
       (let [expected [{:vehicle/brand "Aston Martin", :vehicle/model "DB10"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "DBS V12"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "DB5"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "V12 Vanquish"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "V8 Vantage Volante"}
                       {:vehicle/brand "Aston Martin", :vehicle/model "DBS"}]]
         (t/is (= expected
                  (dx/q [:find  [(pull [:vehicle/brand :vehicle/model] [?table ?e]) ...]
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
