(ns ribelo.doxa-test
  (:require
   [ribelo.doxa :as dx]
   [clojure.test :as t]
   [meander.epsilon :as m]))

;; datascript

(def people-docs
  [{:db/id 1, :name "Petr", :aka ["Devil" "Tupen"] :child [2 3]}
   {:db/id 2, :name "David", :father [1]}
   {:db/id 3, :name "Thomas", :father [1]}
   {:db/id 4, :name "Lucy" :friend [5], :enemy [6]}
   {:db/id 5, :name "Elizabeth" :friend [6], :enemy [7]}
   {:db/id 6, :name "Matthew", :father [3], :friend [7], :enemy [8]}
   {:db/id 7, :name "Eunan", :friend [8], :enemy [4]}
   {:db/id 8, :name "Kerri"}
   {:db/id 9, :name "Rebecca"}])

(def part-docs
  [{:db/id 10, :part-name "Part A"}
   {:db/id 11, :part-name "Part A.A", :part-of 10}
   {:db/id 12, :part-name "Part A.A.A", :part-of 11}
   {:db/id 13, :part-name "Part A.A.A.A", :part-of 12}
   {:db/id 14, :part-name "Part A.A.A.B", :part-of 12}
   {:db/id 15, :part-name "Part A.B", :part-of 10}
   {:db/id 16, :part-name "Part A.B.A", :part-of 15}
   {:db/id 17, :part-name "Part A.B.A.A", :part-of 16}
   {:db/id 18, :part-name "Part A.B.A.B", :part-of 16}])

(def test-db
  (dx/transact {} (into [] (map (fn [tx] [:db/add tx])) (into people-docs part-docs))))

(t/deftest test-pull-attr-spec
  (t/is (= {:name "Petr" :aka ["Devil" "Tupen"]}
           (dx/eql test-db '[:name :aka] 1)))

  (t/is (= {:name "Matthew" :father [3] :db/id 6}
           (dx/eql test-db '[:name :father :db/id] 6)))

  (t/is (= [{:name "Petr"} {:name "Elizabeth"}
            {:name "Eunan"} {:name "Rebecca"}]
           (dx/eql test-db '[:name] [1 5 7 9]))))

(dx/eql test-db '[{:father [:name]}] 2)

(t/deftest test-pull-reverse-attr-spec
  (t/is (= {:name "David" :_child [1]}
           (dx/eql test-db '[:name :_child] 2)))

  (t/is (= {:name "David" :_child [{:name "Petr"}]}
           (dx/eql test-db '[:name {:_child [:name]}] 2)))

  (t/testing "Reverse non-component references yield collections"
    (t/is (= {:name "Thomas" :_father [6]}
             (dx/eql test-db '[:name :_father] 3)))

    (t/is (= {:name "Petr" :_father [3 2]}
             (dx/eql test-db '[:name :_father] 1)))

    (t/is (= {:name "Thomas" :_father [{:name "Matthew"}]}
             (dx/eql test-db '[:name {:_father [:name]}] 3)))

    (t/is (= {:name "Petr" :_father [{:name "Thomas"} {:name "David"}]}
             (dx/eql test-db '[:name {:_father [:name]}] 1)))))

(t/deftest test-pull-component-attr
  (t/is (= {:part-name "Part A.A", :part-of 10}
           (dx/eql test-db [:part-name :part-of] 11)))
  (t/is (= {:part-name "Part A.A", :_part-of [12]}
           (dx/eql test-db [:part-name :_part-of] 11)))
  (t/is (= {:part-name "Part A.A", :part-of [{:part-name "Part A"}]}
           (dx/eql test-db [:part-name {:part-of [:part-name]}] 11))))

(t/deftest test-pull-wildcard
  (t/is (= {:db/id 1 :name "Petr" :aka ["Devil" "Tupen"]
            :child [2 3]}
           (dx/eql test-db [:*] 1)))

  (t/is (= {:db/id 2 :name "David" :_child [1] :father [1]}
           (dx/eql test-db [:* :_child] 2))))

;; crux

(def bond-db
  (let [data (read-string (slurp "./resources/james-bond.edn"))]
    (dx/transact {} (into [] (map (fn [tx] [:db/add tx])) data))))

(deftest process-test
  (testing "read"
    (t/is (= (sut/eql cords [::coords])
             {::coords
              [{:x 10 :y 20}
               {::left 20 ::width 5}]})))

  ;; (testing "reading with *"
  ;;   (is (= (-> (p.eql/process (-> (pci/register geo/full-registry)
  ;;                                 (p.ent/with-entity {:left 10}))
  ;;                             [::geo/x '*]))
  ;;          {::geo/x    10
  ;;           ::geo/left 10
  ;;           :left      10})))

  ;; (testing "nested read"
  ;;   (is (= (p.eql/process (-> (pci/register geo/full-registry)
  ;;                             (p.ent/with-entity {:left 10 :top 5}))
  ;;                         [{::geo/turn-point [:right]}])
  ;;          {::geo/turn-point {:right 10}}))

  ;;   (is (= (p.eql/process (-> (pci/register geo/full-registry)
  ;;                             (p.ent/with-entity {:foo        {::geo/x 10}
  ;;                                                 :bar        {::geo/y 4
  ;;                                                              :mess   "here"}
  ;;                                                 ::geo/width 50
  ;;                                                 :other      "value"}))
  ;;                         [{:foo [:x]}
  ;;                          {:bar [:top]}
  ;;                          :width])
  ;;          {:foo   {:x 10}
  ;;           :bar   {:top 4}
  ;;           :width 50})))

  ;; (testing "process sequence"
  ;;   (is (= (p.eql/process (-> (pci/register registry)
  ;;                             (p.ent/with-entity {::coords (list
  ;;                                                            {:x 10 :y 20}
  ;;                                                            {::geo/left 20 ::geo/width 5})}))
  ;;                         [{::coords [:right]}])
  ;;          {::coords [{} {:right 25}]})))

  ;; (testing "process vector"
  ;;   (let [res (p.eql/process (-> (pci/register registry)
  ;;                                (p.ent/with-entity {::coords [{:x 10 :y 20}
  ;;                                                              {::geo/left 20 ::geo/width 5}]}))
  ;;                            [{::coords [:right]}])]
  ;;     (is (= res {::coords [{} {:right 25}]}))
  ;;     (is (vector? (::coords res)))))

  ;; (testing "process set"
  ;;   (is (= (p.eql/process (-> (pci/register registry)
  ;;                             (p.ent/with-entity {::coords #{{:x 10 :y 20}
  ;;                                                            {::geo/left 20 ::geo/width 5}}}))
  ;;                         [{::coords [:right]}])
  ;;          {::coords #{{} {:right 25}}})))
  )

;; pathom

(def cords
  {::coords
   [{:x 10 :y 20}
    {::left 20 ::width 5}]})

(t/testing "project fn"
  (t/is (= #:film{:name "Spectre", :year "2015"}
           (sut/eql bond-db [:film/name :film/year] :spectre)))
  (t/is (= #:film{:name "Spectre", :year "2015"}
           (crux/project db [:film/name :film/year] :spectre))))
