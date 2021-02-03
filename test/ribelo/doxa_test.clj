(ns ribelo.doxa-test
  (:require [ribelo.doxa :as sut]
            #?(:clj  [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(def people-docs
  [{:db/id :petr, :name "Petr", :aka #{"Devil" "Tupen"}}
   {:db/id :david, :name "David", :parent #{:petr}}
   {:db/id :thomas, :name "Thomas", :parent #{:petr}}
   {:db/id :lucy, :name "Lucy" :friend #{:elizabeth}, :enemy #{:matthew}}
   {:db/id :elizabeth, :name "Elizabeth" :friend #{:matthew}, :enemy #{:eunan}}
   {:db/id :matthew, :name "Matthew", :parent #{:thomas}, :friend #{:eunan}, :enemy #{:kerri}}
   {:db/id :eunan, :name "Eunan", :friend #{:kerri}, :enemy #{:lucy}}
   {:db/id :kerri, :name "Kerri"}
   {:db/id :rebecca, :name "Rebecca"}])

(def part-docs
  [{:db/id :a, :part-name "Part A"}
   {:db/id :a.a, :part-name "Part A.A", :part-of :a}
   {:db/id :a.a.a, :part-name "Part A.A.A", :part-of :a.a}
   {:db/id :a.a.a.a, :part-name "Part A.A.A.A", :part-of :a.a.a}
   {:db/id :a.a.a.b, :part-name "Part A.A.A.B", :part-of :a.a.a}
   {:db/id :a.b, :part-name "Part A.B", :part-of :a}
   {:db/id :a.b.a, :part-name "Part A.B.A", :part-of :a.b}
   {:db/id :a.b.a.a, :part-name "Part A.B.A.A", :part-of :a.b.a}
   {:db/id :a.b.a.b, :part-name "Part A.B.A.B", :part-of :a.b.a}])

(def cords
  {::coords
   [{:x 10 :y 20}
    {::left 20 ::width 5}]})

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
