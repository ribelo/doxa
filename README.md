- [rationale](#org4293fff)
- [db structure](#org76fc065)
- [usage examples](#org1e397c8)
  - [transactions](#orgff1c089)
    - [put](#org4bd3e87)
    - [delete](#org1ad4ed8)
    - [conj](#org338786b)
    - [merge](#orgdab7071)
    - [update](#orgac18cc4)
    - [match](#org922c370)
  - [with metadata](#orgd7a930a)
  - [pull](#orgc9f4338)
    - [eql](#org7ca64f0)
    - [datomic like pull syntax](#orgddd8079)
  - [datalog](#orgc58203c)
    - [joins](#org4304d6c)
  - [benchmark](#org373ad76)
    - [transaction](#org96a76c0)
    - [query](#org89e66e3)
    - [pull](#org72a1687)

[![img](https://img.shields.io/clojars/v/com.github.ribelo/doxa.svg)](https://clojars.org/com.github.ribelo/doxa)

a simple in-memory database, trying to copy the best solutions from [datascript](https://github.com/tonsky/datascript), [xtdb](https://github.com/xtdb/xtdb/), [fulcro](https://github.com/fulcrologic/fulcro), [autonormal](https://github.com/lilactown/autonormal) and especially [shadow-grove](https://github.com/thheller/shadow-experiments/blob/master/src/main/shadow/experiments/grove/db.cljc). each of them has its own strengths, and is an excellent solution, but each lacks some of the cool things that the others have. `doxa` tries to take the best from every solution while being damn fast and lightweight.


<a id="org4293fff"></a>

# rationale

one of the biggest challenges when working on the front end is state management. [re-frame](https://github.com/day8/re-frame) was one of the first solutions to propose one central `app-db`, one source of truth. solution works great, but as the application grows, there is a problem with data denormalization and fatigue with multiple nested maps.

the ideal solution seems to be `datascript`, but there have been several attempts to incorporate it into the `re-frame` ecosystem, eg. [posh](https://github.com/mpdairy/posh) and [re-posh](https://github.com/denistakeda/re-posh), and in my humble opinion, despite much desire and hard work, the transplant has failed. `datascript` seems to be too heavy for the frontend. biggest inconvenience is that `datascript` is built around its own data types. `re-frame` has a whole bunch of tools with [re-frame-10x](https://github.com/day8/re-frame-10x) that allow you to preview `app-db` in real time. `shadow-cljs` also offers `tap>` and there is no problem to spit out entire `app-db` and check each individual map, leaf and node.

`doxa` is an attempt to create a `db` that can be treated as a simple `hashmap`, which makes it possible to use a whole set of Clojure functions on it, from `filter` to `transreducers`, but also using transactions similar to `datascript`, `datalog query` and `pull query`.


<a id="org76fc065"></a>

# db structure

```clojure

(require '[ribelo.doxa     :as dx])
(require '[ribelo.doxa     :as dx])

```

`db` is a simple map with two levels of nesting

```clojure

{[:person/id 1] {:person/id 1 :name "ivan" :age 18}
 [:person/id 2] {:person/id 2 :name "petr" :age 24 :friend [:person/id 1]}}

```


<a id="org1e397c8"></a>

# usage examples


<a id="orgff1c089"></a>

## transactions

```clojure

(def data [{:db/id 1 :name "Petr" :aka ["Devil"]}])

(def db (dx/create-dx data)
  ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
  )

```

each map that is supposed to be an `entity` needs a `:id` key or key with name one of the `#{"id" "by-id" "list"}` suffixes, which are defined in the dynamic variable `dx/*id-sufixes*`, if that wasn&rsquo;t enough. in addition it is possible to explicitly declare via `metadata` which key is to be treated as the primary key of an entity. examples in [metadata](https://github.com/ribelo/doxa#with-metadata).

`doxa` tries to use the full potential of `meander` and be as forgiving as possible with the data it receives. `commit` can accept either a single `transaction` or any collection of `transtactions` that is `sequable?`. the reference of `transaction` can be a `map` or `lookup-ref`, the arguments can be a `key-value pair`, where the value can be anything, a `map`, a `variable`, a `collection of anything`, a lookup-ref etc. arguments can be mixed and mangled. data automatically normalizes, adds and is converted into `lookup-ref` or `collection of lookup-refs` if needed and required.


<a id="org4bd3e87"></a>

### put

1.  add entity

    ```clojure
    
    (dx/commit {} [[:dx/put {:db/id 1 :name "David" :aka ["Devil"]}]])
    ;; => #:db{:id {1 {:db/id 1, :name "David", :aka ["Devil"]}}}
    
    (dx/commit {} [:dx/put [:db/id 1] {:name "David" :aka ["Devil"]}])
    ;; => #:db{:id {1 {:name "David", :aka ["Devil"], :db/id 1}}}
    
    ```

2.  single keyword change

    ```clojure
    
    (dx/commit db [[:dx/put [:db/id 1] :name "David"]])
    ;; => #:db{:id {1 {:db/id 1, :name "David", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/put [:db/id 1] :aka ["Tupen"]]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Tupen"]}}}
    
    ```

3.  multiple kv

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [:dx/put [:db/id 1] :name "Ivan" :age 18 :aka ["Tupen"]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Tupen"]}}}
    ```

4.  add data with autonormalization

    ```clojure
    
    (dx/commit db [[:dx/put [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Lucy"}]]])
    ;; =>
    ;; #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"], :friend [[:db/id 2] [:db/id 3]]},
    ;;           2 {:db/id 2, :name "Ivan"},
    ;;           3 {:db/id 3, :name "Lucy"}}}
    
    ```


<a id="org1ad4ed8"></a>

### delete

deleting data automatically cleans up the database. i.e. if you delete an `entity`, all `look-up refs` refering to it will be deleted at the same time. if you delete the last `map entry` from a map, the whole map will be deleted etc.

1.  delete entity

    ```clojure
    
    (dx/commit db [[:dx/delete [:db/id 1]]])
    ;; => {}
    
    ```

2.  delete keyword

    ```clojure
    
    (dx/commit db [[:dx/delete [:db/id 1] :aka]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr"}}}
    
    (dx/commit db [[:dx/delete [:db/id 1] :aka]
                   [:dx/delete [:db/id 1] :name]])
    ;; => {}
    
    ```

3.  remove elem from vector

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/delete [:db/id 1] :aka "Devil"]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr"}}}
    
    ```

4.  remove an invalid key

    ```clojure
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/delete [:db/id 1] :AKA "Devil"]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    ```


<a id="org338786b"></a>

### conj

because `doxa` is schemeless, if we want to add something to the vector we have to use `:dx/conj`

1.  add elem

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/conj [:db/id 1] :aka "Tupen"]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil" "Tupen"]}}}
    
    (dx/commit db [[:dx/conj [:db/id 1] :name "Ivan"]])
    ;; => #:db{:id {1 {:db/id 1, :name ["Petr" "Ivan"], :aka ["Devil"]}}}
    
    ```

2.  with autonormalisation

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/conj [:db/id 1] :friend {:db/id 2 :name "Ivan"}]])
    ;; =>
    ;; #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"], :friend [[:db/id 2]]},
    ;;           2 {:db/id 2, :name "Ivan"}}}
    
    (dx/commit db [[:dx/conj [:db/id 1] :friend [{:db/id 2 :name "Ivan"} {:db/id 3 :name "Lucy"}]]])
    ;; =>
    ;; #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"], :friend [[:db/id 2] [:db/id 3]]},
    ;;           2 {:db/id 2, :name "Ivan"}, 3
    ;;           {:db/id 3, :name "Lucy"}}}
    
    ```


<a id="orgdab7071"></a>

### merge

`:dx/merge` can easily be replaced by update, but using a specialized transaction allows for speed optimizations and a few other minor things

```clojure

db
;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}

(dx/commit db [:dx/merge [:db/id 1] {:age 18}])
;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"], :age 18}}}
```


<a id="orgac18cc4"></a>

### update

```clojure

db
;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}

(dx/commit db [[:dx/update [:db/id 1] assoc :aka "Tupen"]])
;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka "Tupen"}}}

(dx/commit db [[:dx/update [:db/id 1] :aka conj "Tupen"]])
;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil" "Tupen"]}}}

```


<a id="org922c370"></a>

### match

just like in `xtdb`, we can use match. if data match, `db` is returned unchanged, otherwise `nil`.

1.  match entity

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/match [:db/id 1] {:db/id 1 :name "Petr", :aka ["Devil"]}]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    ```

2.  match keyword

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/match [:db/id 1] :aka ["Devil"]]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    ```

3.  conditional put

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/match [:db/id 1] :aka ["Devil"]]
                   [:dx/put   [:db/id 1] :aka ["Tupen"]]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Tupen"]}}}
    
    ```

4.  conditional delete

    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/match  [:db/id 1]  :aka ["Tupen"]]
                   [:dx/delete [:db/id 1] :aka]])
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    ```
    
    transactions are dropped until the next valid match occurs
    
    ```clojure
    
    db
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"]}}}
    
    (dx/commit db [[:dx/match [:db/id 1] :aka ["Tupen"]]       ;; unmatched
                   [:dx/put [:db/id 1] :age 15]                ;;    skiped
                   [:dx/match [:db/id 1] :name "Petr"]         ;;   matched
                   [:dx/put [:db/id 1] :sex :male]])           ;;  commited
    ;; => #:db{:id {1 {:db/id 1, :name "Petr", :aka ["Devil"], :sex :male}}}
    
    ```


<a id="orgd7a930a"></a>

## with metadata

explicitly declaring a key allows you to indicate exactly which key should be used to build the db tree. this makes working with `graphql` much easier, while also allowing you to split the data in a way that makes `q` queries [much faster](https://github.com/ribelo/doxa#can-we-be-faster-than-datascript-yes)

```clojure
(def db (->>
          ^{::dx/entity-key :person/id}
          {:id        "10"
           :person/id "10"
           :name      "Enzo"
           :car
           ^{::dx/entity-key :automobile/id}
           {:id            "20"
            :automobile/id "20"
            :name          "Audi"}}
          (vector :dx/put)
          (dx/commit (dx/create-dx))))
db
;; => {:automobile/id {"20" {:id "20", :automobile/id "20", :name "Audi"}},
;;     :person/id     {"10" {:id "10", :person/id "10", :name "Enzo", :car [:automobile/id "20"]}}}

(dx/pull db [:name {:car [:autombile/id :name]}] [:person/id "10"])
;; => {:name "Enzo", :car {:name "Audi"}}
```


<a id="orgc9f4338"></a>

## pull

```clojure

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

(def db (dx/create-dx people-docs))
;; #:db{:id {7 {:db/id 7, :name "Eunan",     :friend #{[:db/id 8]},     :enemy #{[:db/id 4]}}
;;           1 {:db/id 1, :name "Petr",      :aka    ["Devil" "Tupen"], :child #{[:db/id 3] [:db/id 2]}}
;;           4 {:db/id 4, :name "Lucy",      :friend #{[:db/id 5]},     :enemy #{[:db/id 6]}}
;;           6 {:db/id 6, :name "Matthew",   :father #{[:db/id 3]},     :friend #{[:db/id 7]}, :enemy #{[:db/id 8]}}
;;           3 {:db/id 3, :name "Thomas",    :father #{[:db/id 1]}}
;;           2 {:db/id 2, :name "David",     :father #{[:db/id 1]}}
;;           9 {:db/id 9, :name "Rebecca"}
;;           5 {:db/id 5, :name "Elizabeth", :friend #{[:db/id 6]}, :enemy #{[:db/id 7]}}
;;           8 {:db/id 8, :name "Kerri"}}}

```


<a id="org7ca64f0"></a>

### eql

```clojure

(dx/pull db {[:db/id 1] [:name :aka]})
;; => {:name "Petr", :aka ["Devil"]}

```


<a id="orgddd8079"></a>

### datomic like pull syntax

```clojure

(dx/pull db [:name :aka] [:db/id 1])
;; => {:name "Petr", :aka ["Devil"]}

```

1.  simple query

    ```clojure
    
    (dx/pull db  [:name :father :db/id] [:db/id 6])
    ;; => {:name "Matthew", :father [:db/id 3], :db/id 6}
    
    ```

2.  pull many

    ```clojure
    
    (dx/pull db [:name] [[:db/id 1] [:db/id 5] [:db/id 7] [:db/id 9]])
    ;; => [{:name "Petr"} {:name "Elizabeth"} {:name "Eunan"} {:name "Rebecca"}]
    
    ```

3.  reverse search

    ```clojure
    
    (dx/pull db [:name :_child] [:db/id 2])
    ;; => {:name "David", :_child [:db/id 1]}
    
    (dx/pull db [:name {:_child [:name]}] [:db/id 2])
    ;; => {:name "David", :_child {:name "Petr"}}
    
    ```

4.  reverse non-component references yield collections

    ```clojure
    
    (dx/pull db '[:name :_father] [:db/id 3])
    ;; => {:name "Thomas", :_father [:db/id 6]}
    
    (dx/pull db '[:name :_father] [:db/id 1])
    ;; => {:name "Petr", :_father [[:db/id 3] [:db/id 2]]}
    
    (dx/pull db '[:name {:_father [:name]}] [:db/id 3])
    ;; => {:name "Thomas", :_father {:name "Matthew"}}
    
    (dx/pull db '[:name {:_father [:name]}] [:db/id 1])
    ;; => {:name "Petr", :_father [{:name "Thomas"} {:name "David"}]}
    
    ```

5.  wildcard

    ```clojure
    
    (dx/pull db [:*] [:db/id 1])
    ;; =>
    ;; {:db/id 1, :name "Petr", :aka ["Devil" "Tupen"], :child [[:db/id 2] [:db/id 3]]}
    
    (dx/pull db [:* :_child] [:db/id 2])
    ;; => {:db/id 2, :name "David", :father [:db/id 1], :_child [:db/id 1]}
    
    ```

6.  missing attrs are dropped

    ```clojure
    
    (dx/pull db [:name {:child [:name]}] [:db/id 2])
    ;; => {:name "David"}
    
    ```

7.  non matching results are removed from collections

    ```clojure
    
    (dx/pull db [:name {:child [:foo]}] [:db/id 1])
    ;; => {:name "Petr", :child []}
    
    ```


<a id="orgc58203c"></a>

## datalog

```clojure

(def db (dx/create-dx [{:db/id 1, :name "Ivan" :age 15}
                       {:db/id 2, :name "Petr" :age 37}
                       {:db/id 3, :name "Ivan" :age 37}
                       {:db/id 4, :age 15}]))

```


<a id="org4304d6c"></a>

### joins

exactly as in datascript and datomic, `q` returns `set`

```clojure

db
;; => #:db{:id {1 {:db/id 1, :name "Ivan", :age 15},
;;              2 {:db/id 2, :name "Petr", :age 37},
;;              3 {:db/id 3, :name "Ivan", :age 37},
;;              4 {:db/id 4, :age 15}}}


(dx/q [:find ?e
       :where [?e :name]]
  db)
;; => #{[3] [2] [1]}

(dx/q [:find ?e ?v
       :where
       [?e :name "Ivan"]
       [?e :age ?v]]
  db)
;; => #{[1 15] [3 37]}

```

```clojure

db
;; => #:db{:id {1 {:db/id 1, :name "Ivan", :age 15},
;;              2 {:db/id 2, :name "Petr", :age 37},
;;              3 {:db/id 3, :name "Ivan", :age 37},
;;              4 {:db/id 4, :age 15}}}

(dx/q [:find ?e1 ?e2
       :where
       [?e1 :name ?n]
       [?e2 :name ?n]] db)
;; => #{[2 2] [3 3] [1 1] [1 3] [3 1]}

(dx/q [:find ?e1 ?e2 ?n
       :where
       [?e1 :name "Ivan"]
       [?e1 :age ?a]
       [?e2 :age ?a]
       [?e2 :name ?n]]
  db)
;; => #{[1 1 "Ivan"] [3 3 "Ivan"] [3 2 "Petr"]}

```

1.  many

    `meander` is running underneath, so you can use all the functions available in the `meander`, e.g. `scan`
    
    ```clojure
    
    (def db (dx/create-dx [{:db/id 1
                            :name  "Ivan"
                            :aka   ["ivolga" "pi"]}
                           {:db/id 2
                            :name  "Petr"
                            :aka   ["porosenok" "pi"]}]))
    
    (dx/q [:find ?n1 ?n2
           :where
           [?e1 :aka (m/scan ?x)]
           [?e2 :aka (m/scan ?x)]
           [?e1 :name ?n1]
           [?e2 :name ?n2]]
      db)
    ;; => #{["Ivan" "Petr"] ["Petr" "Ivan"] ["Petr" "Petr"] ["Ivan" "Ivan"]}
    ```

2.  in

    ```clojure
    
    (def db (dx/create-dx [{:db/id 1, :name "Ivan" :age 15 :email "ivan@mail.ru"}
                           {:db/id 2, :name "Petr" :age 37 :email "petr@gmail.com"}
                           {:db/id 3, :name "Ivan" :age 37 :email "ivan@mail.ru"}]))
    
    (dx/q [:find ?e
           :in ?attr ?value
           :where [?e ?attr ?value]]
      db :name "Ivan")
    ;; => #{[3] [1]}
    
    (dx/q [:find ?e
           :in ?attr [?value]
           :where [?e ?attr ?value]]
      db :name ["Ivan" "Petr"])
    ;; => #{[3] [2] [1]}
    
    (dx/q [:find ?e
           :in ?attr ?value
           :where [?e ?attr ?value]]
      db :age 37)
    ;; => #{[3] [2]}
    ```

3.  relation binding

    ```clojure
    
    (dx/q [:find ?e ?email
           :in [[?n ?email]]
           :where
           [?e :name ?n]
           [?e :email ?email]]
      db
      [["Ivan" "ivan@mail.ru"]
       ["Petr" "petr@gmail.com"]])
    ;; => #{[1 "ivan@mail.ru"] [2 "petr@gmail.com"] [3 "ivan@mail.ru"]}
    
    ```

4.  joins with idents

    unfortunately, but using ref links in the form of `[?table ?id]` also entails disadvantages and difficulties.
    
    ```clojure
    
    (def db (dx/create-dx [{:db/id 1
                            :name  "Ivan"
                            :friend   [{:db/id 2
                                        :name "Petr"}
                                       {:db/id 3
                                        :name "Oleg"}]}]))
    
    db
    ;; {:db/id {2 {:db/id 2, :name "Petr"}
    ;;          3 {:db/id 3, :name "Oleg"}
    ;;          1 {:db/id 1,
    ;;             :name "Ivan",
    ;;             :friend #{[:db/id 2] [:db/id 3]}}}}
    
    ```
    
    references are always `vector` and must be treated as such
    
    ```clojure
    
    (dx/q [:find ?friends ...
           :where
           [?e :name "Ivan"]
           [?e :friend ?friends]]
      db)
    ;; => #{#{[:db/id 3] [:db/id 2]}}
    
    ```
    
    if we try to do a simple join we get nothing `:(`
    
    ```clojure
    
    (dx/q [:find ?fname .
           :where
           [?e :name "Ivan"]
           [?e :friend ?friends]
           [?friends :name ?fname]]
      db)
    ;; => #{}
    
    ```
    
    but knowing what a reference looks like, we can get around this
    
    ```clojure
    
    (dx/q [:find ?fname ...
           :where
           [?friend :name ?fname]
           [?e :name "Ivan"]
           [?e :friend [_ ?friend]]]
      db)
    ;; => #{"Petr" "Oleg"}
    
    ```
    
    at the moment my knowledge of meader internals is too limited to make it nicer

5.  caching & matching

    `q` allows results caching and re-run queries only if the last transaction changes data that may have an impact. [editscript](https://github.com/juji-io/editscript) diffs are used, which, when converted into datoms, are compared with each query datom. the most pessimistic scenario is taken into consideration, because it is better to have false positives than false negatives. in other words, it is better to re-run a query unnecessarily than not to run it when you need to
    
    ```clojure
    (def conn_ (atom (dx/create-dx [] {::dx/with-diff? true})))
    (meta @conn_)
    ;; => #:ribelo.doxa{:with-diff? true,
    ;;                  :last-transaction-timestamp 1632083203039,
    ;;                  :tx nil,
    ;;                  :cache_ #atom[{} 0x281c70de]}
    
    ```
    
    ```clojure
    
    (dx/commit! conn_ [:dx/put [:db/id 1] {:name "ivan"}])
    
    @conn_
    ;; => #:db{:id {1 {:name "ivan", :db/id 1}}}
    
    (meta @conn_)
    ;; => #:ribelo.doxa{:with-diff? true,
    ;;                  :last-transaction-timestamp 1632083289779,
    ;;                  :tx [[[:db/id] :+ {1 {:name "ivan", :db/id 1}}]],
    ;;                  :cache_ #atom[{} 0x281c70de],
    ;;                  :h -69594259}
    
    (dx/-last-tx @conn_)
    ;; => [[:db/id] :+ {1 {:name "ivan", :db/id 1}}]
    
    (-> (dx/-last-tx @conn_)
        (dx/-tx->datoms))
    ;; => [[:db/id 1 :name "ivan"] [:db/id 1 :db/id 1]]
    
    (-> (dx/-last-tx-match-where? @conn_ '[[?e ?attr ?v]]))
    ;; => true
    
    ;; meander allows to match after each element of the datom
    (-> (dx/-last-tx-match-where? @conn_ '[[:db/id ?e ?attr ?v]]))
    ;; => true
    
    (-> (dx/-last-tx-match-where? @conn_ '[[:db/id 1 ?attr ?v]]))
    ;; => true
    
    (-> (dx/-last-tx-match-where? @conn_ '[[:db/id 2 ?attr ?v]]))
    ;; => false
    
    (-> (dx/-last-tx-match-where? @conn_ '[[?e ?attr "ivan"]]))
    ;; => true
    
    (-> (dx/-last-tx-match-where? @conn_ '[[?e ?attr "petr"]]))
    ;; => false
    ```
    
    each data returned by `q` has metadata attached to it showing whether the results are fresh etc
    
    ```clojure
    ^{::cache? true}
    (defn do-query []
      ^{::dx/cache ::my-query}
      (dx/q [:find (pull [:*] [?table ?e]) .
             :where
             [?table ?e :name "ivan"]]
        @conn_))
    (def r (do-query))
    r
    ;; => {:name "ivan", :db/id 1}
    (meta r)
    ;; => #:ribelo.doxa{:last-transaction-timestamp 1632083289779,
    ;;                  :last-query-timestamp nil,
    ;;                  :fresh? true}
    (meta (do-query))
    ;; => #:ribelo.doxa{:last-transaction-timestamp 1632083289779,
    ;;                  :last-query-timestamp 1632084271082,
    ;;                  :fresh? false}
    ```
    
    this allows you to write advanced queries that are not re-run until needed, and thus views are re-rendered only when data changes.
    
    ```clojure
    @conn_
    ;; => #:db{:id {1 {:name "ivan", :db/id 1}}}
    
    (defn find-petr []
      ^{::dx/cache? true}
      (dx/q [:find (pull [:*] [?table ?e]) .
             :where
             [?table ?e :name "petr"]]
        @conn_))
    
    (::dx/fresh? (meta (find-petr)))
    ;; => true
    (::dx/fresh? (meta (find-petr)))
    ;; => false
    (dx/commit! conn_ [:dx/put [:db/id 1] :age 18])
    @conn_
    ;; => #:db{:id {1 {:name "ivan", :db/id 1, :age 18}}}
    
    ;; data is still retrieved from the cache and the query is not executed again
    (::dx/fresh? (meta (find-petr)))
    ;; => false
    
    (dx/commit! conn_ [:dx/put [:db/id 2] :name "petr"])
    @conn_
    ;; => #:db{:id {1 {:name "ivan", :db/id 1, :age 18}, 2 {:name "petr"}}}
    
    (def r (find-petr))
    r
    ;; => {:name "petr"}
    (::dx/fresh? (meta r))
    ;; => true
    
    ;; query has been re-run and the data has been pulled correctly
    
    (find-petr)
    ;; => {:name "petr"}
    
    (::dx/fresh? (meta (find-petr)))
    ;; => false
    ```
    
    what effect this has on performance can be found in the [benchmark](https://github.com/ribelo/doxa#query)


<a id="org373ad76"></a>

## benchmark

```clojure

(require '[taoensso.encore :as enc])
(require '[meander.epsilon :as   m])
(require '[datascript.core :as   d])
(require '[ribelo.doxa     :as  dx])
(require '[pyramid.core    :as  pc])
(require '[pyramid.query   :as  pq])

```

it is rare for a spa database to contain things that cannot be divided into tables or assigned categories. so let&rsquo;s create 100k maps for 10 different categories

```clojure

(let [next-eid (volatile! 0)]

  (defn random-man []
    {:db/id     (vswap! next-eid inc)
     :name      (rand-nth ["Ivan" "Petr" "Sergei" "Oleg" "Yuri" "Dmitry" "Fedor" "Denis"])
     :last-name (rand-nth ["Ivanov" "Petrov" "Sidorov" "Kovalev" "Kuznetsov" "Voronoi"])
     :alias     (vec
                 (repeatedly (rand-int 10) #(rand-nth ["A. C. Q. W." "A. J. Finn" "A.A. Fair" "Aapeli" "Aaron Wolfe" "Abigail Van Buren" "Jeanne Phillips" "Abram Tertz" "Abu Nuwas" "Acton Bell" "Adunis"])))
     :age       (rand-int 100)
     :sex       (rand-nth [:male :female])
     :salary    (rand-int 100000)
     :friend    {:db/ref-id (rand-int 20000)}})

  (defn random-fruit []
    {:fruit/id     (vswap! next-eid inc)
     :name      (rand-nth ["Avocado" "Grape" "Plum" "Apple" "Orange"])
     :price     (rand-int 100)})

  (defn random-vegetable []
    {:vegetable/id     (vswap! next-eid inc)
     :name      (rand-nth ["Onion" "Cabbage" "Pea" "Tomatto" "Lettuce"])
     :price     (rand-int 100)})

  (defn random-car []
    {:car/id     (vswap! next-eid inc)
     :name      (rand-nth ["Audi" "Mercedes" "BMW" "Ford" "Honda" "Toyota"])
     :price     (rand-int 100)})

  (defn random-animal []
    {:animal/id     (vswap! next-eid inc)
     :name      (rand-nth ["Otter" "Dog" "Panda" "Lynx" "Cat" "Lion"])
     :price     (rand-int 100)})

  (defn random-cat []
    {:cat/id     (vswap! next-eid inc)
     :name      (rand-nth ["Traditional Persian" "Ocicat" "Munchkin cat" "Persian cat" "Burmese cat"])
     :price     (rand-int 100)})

  (defn random-dog []
    {:dog/id     (vswap! next-eid inc)
     :name      (rand-nth ["Croatian Shepherd" "Deutch Langhaar" "Miniature Pincher" "Italian Sighthound" "Jack Russell Terrier"])
     :price     (rand-int 100)})

  (defn random-country []
    {:country/id     (vswap! next-eid inc)
     :name      (rand-nth ["Seychelles" "Greenland" "Iceland" "Bahrain" "Bhutan"])
     :price     (rand-int 100)})

  (defn random-language []
    {:language/id     (vswap! next-eid inc)
     :name      (rand-nth ["Malagasy" "Kashmiri" "Amharic" "Inuktitut" "Esperanto"])
     :price     (rand-int 100)})

  (defn random-marijuana-strain []
    {:marijuana/id     (vswap! next-eid inc)
     :name      (rand-nth ["Lemonder" "Black-Mamba" "Blueberry-Space-Cake" "Strawberry-Amnesia"])
     :price     (rand-int 100)})

  (defn random-planet []
    {:planet/id     (vswap! next-eid inc)
     :name      (rand-nth ["Pluto" "Saturn" "Venus" "Mars" "Jupyter"])
     :price     (rand-int 100)}))

(def people           (repeatedly random-man))
(def fruit            (repeatedly random-fruit))
(def vegetable        (repeatedly random-vegetable))
(def car              (repeatedly random-car))
(def animal           (repeatedly random-animal))
(def cat              (repeatedly random-cat))
(def dog              (repeatedly random-dog))
(def country          (repeatedly random-country))
(def language         (repeatedly random-language))
(def marijuana-strain (repeatedly random-marijuana-strain))

(def planet           (repeatedly random-planet))

(def people50k           (shuffle (take 50000 people)))

(def fruit10k            (shuffle (take 10000 fruit)))
(def vegetable10k        (shuffle (take 10000 vegetable)))
(def car10k              (shuffle (take 10000 car)))
(def animal10k           (shuffle (take 10000 animal)))
(def cat10k              (shuffle (take 10000 cat)))
(def dog10k              (shuffle (take 10000 dog)))
(def country10k          (shuffle (take 10000 country)))
(def language10k         (shuffle (take 10000 language)))
(def marijuana-strain10k (shuffle (take 10000 marijuana-strain)))
(def planet10k           (shuffle (take 10000 planet)))

(def data100k (enc/into-all []
                            fruit10k vegetable10k car10k animal10k cat10k dog10k
                            country10k language10k marijuana-strain10k planet10k))

(def schema
  {:friend {:db/valueType   :db.type/ref
            :db/cardinality :db.cardinality/many}
   :alias   {:db/cardinality :db.cardinality/many}})
```


<a id="org96a76c0"></a>

### transaction

1.  adding data one transaction at a time

    ```clojure
    
    (defn datascript-add-1 [data]
      (enc/qb 1
        (reduce
         (fn [db p]
           (-> db
               (d/db-with [[:db/add (:db/id p) :name      (:name p)]])
               (d/db-with [[:db/add (:db/id p) :last-name (:last-name p)]])
               (d/db-with [[:db/add (:db/id p) :age       (:age p)]])
               (d/db-with [[:db/add (:db/id p) :salary    (:salary p)]])))
         (d/empty-db schema)
         data)))
    
    (defn doxa-add-1 [data]
      (enc/qb 1
        (reduce
         (fn [db p]
           (dx/commit db [[:dx/put p]]))
         {}
         data)))
    
    ;; result in ms
    [(datascript-add-1 people50k) (doxa-add-1 people50k)]
    ;; clj => [1155.09 226.2]
    
    ```

2.  add all data in single transaction

    ```clojure
    
    (defn datascript-add-all []
      (enc/qb 1
        (d/db-with (d/empty-db schema) people50k)))
    
    (defn doxa-add-all []
      (enc/qb 1
        (->> (into []
                   (map (fn [p] [:dx/put p]))
                   people50k)
             (dx/commit {}))))
    
    [(datascript-add-all) (doxa-add-all)]
    ;; clj => [3285.33 221.5]
    
    ```


<a id="org89e66e3"></a>

### query

1.  can we be faster than datascript? yes!

    ```clojure
    
    (def db100k
      (d/db-with (d/empty-db)
                 (mapv
                  (fn [m]
                    (reduce-kv
                     (fn [acc k v]
                       (if (= :id (name k))
                         (assoc acc :db/id v)
                         (assoc acc k v)))
                     {}
                     m))
                  data100k)))
    
    (require '[ribelo.doxa.impl.map :as dxim])
    (def dx100k (dx/create-dx (dxim/empty-db) data100k))
    (def pc100k (pc/db data100k))
    
    ```
    
    ```clojure
    
    (require '[ribelo.doxa.query :as dxq])
    
    (defn datascript-query []
      (enc/qb 1e1
        (d/q '[:find ?e
               :where
               [?e :name "Avocado"]
               [?e :price ?price]
               [(< ?price 50)]]
          db100k)))
    
    (defn dx-query []
      (enc/qb 1e1
        (dxq/-q '[:find ?e
                  :where
                  [?e :name "Avocado"]
                  [?e :price ?price]
                  [(< ?price 50)]]
                dx100k)))
    
    (defn cached-dx-query []
      (enc/qb 1e1
        ^{::dx/cache? true}
        (dx/q [:find ?e
               :where
               [?e :name "Avocado"]
               [?e :price ?price]
               [(< ?price 50)]]
          dx100k)))
    
    (defn fast-dx-query []
      (enc/qb 1e1
        (dxq/-q '[:find ?e
                  :where
                  [?e :name "Avocado"]
                  [?e :price ?price]
                  [(< ?price 50)]]
                (dx/table dx100k :fruit/id))))
    
    (defn fast-cached-dx-query []
      (enc/qb 1e1
        ^{::dx/cache :any-value}
        (dx/q [:find ?e
               :in ?table
               :where
               [?table ?e :name "Avocado"]
               [?table ?e :price ?price]
               [(< ?price 50)]]
          dx100k :fruit/id)))
    
    (defn meander-query []
      (enc/qb 1e1
        (doall
         ^::m/dangerous
         (m/search (.-db dx100k)
           {?eid {:name "Avocado"
                  :price (m/pred #(< 50 ^long %))}}
           ?eid))))
    
    (defn transducer-query []
      (enc/qb 1e1
        (into []
              (comp
               (map (fn [^clojure.lang.IMapEntry me] (.val me)))
               (filter (fn [^clojure.lang.ILookup m]
                         (and (= "Avocado" (.valAt m :name))
                              (< 50 (.valAt m :price)))))
               (map (fn [^clojure.lang.ILookup m] (.valAt m :fruit/id))))
              (dx/table dx100k :fruit/id))))
    
    [(datascript-query) (dx-query) #_(cached-dx-query) (fast-dx-query) #_(fast-cached-dx-query) (transducer-query) (meander-query)]
    ;; clj => [159.42 531.67 0.05 50.15 0.03]
    
    (require '[ribelo.extropy :as ex])
    (require '[ribelo.doxa.impl.protocols :as p])
    (require '[criterium.core :as cc])
    ```

2.  query by one condition

    ```clojure
    
    (def db50k
      (d/db-with (d/empty-db)
                 (mapv
                  (fn [m]
                    (reduce-kv
                     (fn [acc k v]
                       (if (= :id (name k))
                         (assoc acc :db/id v)
                         (assoc acc k v)))
                     {}
                     m))
                  people50k)))
    
    (def dx50k (dx/create-dx people50k))
    (def pc50k (pc/db people50k))
    
    ```
    
    ```clojure
    
    (defn datascript-q1 []
      (enc/qb 1
        (d/q '[:find ?e
               :where [?e :name "Ivan"]]
          db50k)))
    
    (defn pq-q1 []
      (enc/qb 1
        (doall
         (pq/q '[:find ?e
                 :where [?e :name "Ivan"]]
           pc50k))))
    
    (defn dx-q1 []
      (enc/qb 1
        (dx/q [:find  ?e
               :where [?e :name "Ivan"]]
          dx50k)))
    
    [(datascript-q1) (pq-q1) (dx-q1)]
    ;; => [5.08 43.9 33.34]
    
    ```

3.  two conditions

    ```clojure
    
    (defn datascript-q2 []
      (enc/qb 1e1
        (d/q '[:find ?e ?a
               :where
               [?e :name "Ivan"]
               [?e :age ?a]]
          db50k)))
    
    (defn pq-q2 []
      (enc/qb 1e1
        (doall
         (pq/q '[:find ?e ?a
                 :where
                 [?e :name "Ivan"]
                 [?e :age ?a]]
           pc50k))))
    
    (defn dx-q2 []
      (enc/qb 1e1
        (dx/q [:find  [?e ?a]
               :where
               [?e :name "Ivan"]
               [?e :age ?a]]
          dx50k)))
    
    [(datascript-q2) (pq-q2) (dx-q2)]
    ;; => [145.09 914.3 346.78]
    ```

4.  3

    ```clojure
    
    (defn datascript-q3 []
      (enc/qb 1e1
        (d/q '[:find ?e ?a
               :where
               [?e :name "Ivan"]
               [?e :age ?a]
               [?e :sex :male]]
          db50k)))
    
    (defn pq-q3 []
      (enc/qb 1e1
        (doall
         (pq/q '[:find ?e ?a
                 :where
                 [?e :name "Ivan"]
                 [?e :age ?a]
                 [?e :sex :male]]
           pc50k))))
    
    (defn dx-q3 []
      (enc/qb 1e1
        (dx/q [:find  [?e ?a]
               :where
               [?e :name "Ivan"]
               [?e :age ?a]
               [?e :sex :male]]
          dx50k)))
    
    [(datascript-q3) (pq-q3) (dx-q3)]
    ;; => [217.01 1157.64 334.31]
    ;; cljs => [   409    646]
    ;; clj  => [133.26 307.05]
    ```

5.  4

    ```clojure
    
    (defn datascript-q4 []
      (enc/qb 1e1
        (d/q '[:find ?e ?l ?a
               :where [?e :name "Ivan"]
               [?e :last-name ?l]
               [?e :age ?a]
               [?e :sex :male]]
          db50k)))
    
    (defn pq-q4 []
      (enc/qb 1e1
        (doall
         (pq/q '[:find ?e ?l ?a
                 :where [?e :name "Ivan"]
                 [?e :last-name ?l]
                 [?e :age ?a]
                 [?e :sex :male]]
           pc50k))))
    
    (defn dx-q4 []
      (enc/qb 1e1
        (doall
         (dx/q [:find [?e ?l ?a]
                :where [?e :name "Ivan"]
                [?e :last-name ?l]
                [?e :age ?a]
                [?e :sex :male]]
           dx50k))))
    
    [(datascript-q4) (pq-q4) (dx-q4)]
    ;; => [351.17 1612.01 343.59]
    
    ```

6.  one pred

    ```clojure
    
    (defn datascript-qpred1 []
      (enc/qb 1e1
        (d/q '[:find ?e ?s
               :where [?e :salary ?s]
               [(> ?s 50000)]]
          db50k)))
    
    (defn pq-qpred1 []
      (enc/qb 1e1
        (doall
         (pq/q '[:find ?e ?s
                 :where [?e :salary ?s]
                 [(> ?s 50000)]]
           pc50k))))
    
    (defn dx-qpred1 []
      (enc/qb 1e1
        (dx/q [:find ?e ?s
              :where [?e :salary ?s]
              [(> ?s 50000)]]
          dx50k)))
    
    [(datascript-qpred1) (pq-qpred1) (dx-qpred1)]
    ;; => [256.56 4410.23 531.31]
    ```


<a id="org72a1687"></a>

### pull

1.  one key

    ```clojure
    
    (defn datascript-pull1 []
      (enc/qb 1e3
        (d/pull db100k [:name] 50300)))
    
    (defn pyramid-pull1 []
      (enc/qb 1e3
        (pc/pull pc100k [[:fruit/id 50300]])))
    
    (defn dx-pull1 []
      (enc/qb 1e3
        (dx/pull dx100k [:name] [:fruit/id 50300])))
    
    [(datascript-pull1) (pyramid-pull1) (dx-pull1)]
    ;; => [10.22 5.52 1.31]
    ```

2.  entire map

    ```clojure
    
    (defn datascript-pull2 []
      (enc/qb 1e3
        (d/pull db100k ['*] (rand-int 20000))))
    
    (defn dx-pull2 []
      (enc/qb 1e3
        (dx/pull dx100k [:*] [:db/id (rand-int 20000)])))
    
    [(datascript-pull2) (dx-pull2)]
    ;; cljs => [   43   11]
    ;; clj  => [38.52 3.81]
    
    ```

3.  joins

    ```clojure
    
    (defn datascript-pull3 []
      (enc/qb 1e3
        (d/pull db100k [:name {:friend [:name]}] (rand-int 20000))))
    
    (defn dx-pull3 []
      (enc/qb 1e3
        (dx/pull dx100k [:name {:friend [:name]}] [:db/id (rand-int 20000)])))
    
    [(datascript-pull3) (dx-pull3)]
    ;; cljs => [   42   19]
    ;; clj  => [20.63 2.84]
    ```
