(ns ribelo.query-test
  (:require
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])
   [ribelo.doxa.query :as dxq]))

(comment
  (def me [[:db/id 1] {:name "Ivan"}])
  )

(t/deftest match-datom
  (let [me [[:db/id 1] {:name "Ivan"}]]
    (t/testing "[?e :name]"
      (let [matcher (dxq/-datom-matcher '[?e :name])
            acc (java.util.HashMap.)]
        (t/is (= [:db/id 1] (get (matcher acc me) '?e)))))
    (t/testing "[?e :name \"Ivan\"]"
      (let [matcher (dxq/-datom-matcher '[?e :name "Ivan"])
            acc (java.util.HashMap.)]
        (t/is (= {?e [:db/id 1]}
                 (matcher acc me)))))
    (t/testing "[?e :name ?name]"
      (let [matcher (dxq/-datom-matcher '[?e :name ?name])
            acc (java.util.HashMap.)]
        (t/is (= {?e [:db/id 1], ?name "Ivan"}
                 (matcher acc me)))))))
