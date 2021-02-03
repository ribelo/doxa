(ns ribelo.doxa-test
  (:require
   [ribelo.doxa :as dx]
   [datascript.core :as d]
   [clojure.test :as t]
   [meander.epsilon :as m]))

;; datascript

(def ^:private test-schema
  {:aka    { :db/cardinality :db.cardinality/many }
   :child  { :db/cardinality :db.cardinality/many
             :db/valueType :db.type/ref }
   :friend { :db/cardinality :db.cardinality/many
             :db/valueType :db.type/ref }
   :enemy  { :db/cardinality :db.cardinality/many
             :db/valueType :db.type/ref }
   :father { :db/valueType :db.type/ref }

   :part   { :db/valueType :db.type/ref
             :db/isComponent true
             :db/cardinality :db.cardinality/many }
   :spec   { :db/valueType :db.type/ref
             :db/isComponent true
             :db/cardinality :db.cardinality/one }})

(def test-datoms
  (->>
    [[1 :name  "Petr"]
     [1 :aka   "Devil"]
     [1 :aka   "Tupen"]
     [2 :name  "David"]
     [3 :name  "Thomas"]
     [4 :name  "Lucy"]
     [5 :name  "Elizabeth"]
     [6 :name  "Matthew"]
     [7 :name  "Eunan"]
     [8 :name  "Kerri"]
     [9 :name  "Rebecca"]
     [1 :child 2]
     [1 :child 3]
     [2 :father 1]
     [3 :father 1]
     [6 :father 3]
     [10 :name  "Part A"]
     [11 :name  "Part A.A"]
     [10 :part 11]
     [12 :name  "Part A.A.A"]
     [11 :part 12]
     [13 :name  "Part A.A.A.A"]
     [12 :part 13]
     [14 :name  "Part A.A.A.B"]
     [12 :part 14]
     [15 :name  "Part A.B"]
     [10 :part 15]
     [16 :name  "Part A.B.A"]
     [15 :part 16]
     [17 :name  "Part A.B.A.A"]
     [16 :part 17]
     [18 :name  "Part A.B.A.B"]
     [16 :part 18]]
   (map #(apply d/datom %))))

(def ^:private d-db (d/init-db test-datoms test-schema))

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

(require '[taoensso.encore :as e])

(e/qb 1e3 (d/pull d-db '[:name :aka] 1))
(e/qb 1e3 (dx/eql test-db [:name :aka] 1))

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
  (t/is (= {:part-name "Part A.A", :part-of {:part-name "Part A"}}
           (dx/eql test-db [:part-name {:part-of [:part-name]}] 11))))

(t/deftest test-pull-wildcard
  (t/is (= {:db/id 1 :name "Petr" :aka ["Devil" "Tupen"]
            :child [2 3]}
           (dx/eql test-db [:*] 1)))

  (t/is (= {:db/id 2 :name "David" :_child [1] :father [1]}
           (dx/eql test-db [:* :_child] 2))))


(t/deftest test-pull-map
  (t/testing "Single attrs yield a map"
    (t/is (= {:name "Matthew" :father {:name "Thomas"}}
             (dx/eql test-db [:name {:father [:name]}] 6))))

  (t/testing "Multi attrs yield a collection of maps"
    (t/is (= {:name "Petr" :child [{:name "David"}
                                   {:name "Thomas"}]}
             (dx/eql test-db [:name {:child [:name]}] 1))))

  (t/testing "Missing attrs are dropped"
    (t/is (= {:name "Petr"}
             (dx/eql test-db [:name {:father [:name]}] 1))))

  (t/testing "Non matching results are removed from collections"
    (t/is (= {:name "Petr" :child []}
             (dx/eql test-db [:name {:child [:foo]}] 1))))

  (t/testing "Map specs can override component expansion"
    (let [parts {:part-name "Part A", :_part-of [{:part-name "Part A.B"} {:part-name "Part A.A"}]}]
      (t/is (= parts
               (dx/eql test-db [:part-name {:_part-of [:part-name]}] 10)))

      (t/is (= parts
               (dx/eql test-db [:part-name {:part-of 1}] 10))))))

;; crux

(def bond-db
  (let [data (read-string (slurp "./resources/james-bond.edn"))]
    (dx/transact {} (into [] (map (fn [tx] [:db/add tx])) data))))

(t/testing "project fn"
  (t/is (= #:film{:name "Spectre", :year "2015"}
           (dx/eql bond-db [:film/name :film/year] :spectre))))
