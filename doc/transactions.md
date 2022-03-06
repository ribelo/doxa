- [dx/put](#orgb4320f1)
- [dx/delete](#orgfd05f9b)
- [dx/merge](#org4f168e2)
- [dx/update](#org9f38158)
- [dx/match](#org3a6caff)

the commit function which is used for transactions has been implemented using `defmulti`, allowing you to easily implement your own transactions. a transaction must be a vector, where the first element is used to dispatch the transaction to the corresponding `defmethod`.

the five basic transaction types are `:dx/put` `:dx/delete` `:dx/merge`, `:dx/update` and `:dx/match`

`commit` accepts either a single transaction and a collection of transactions


<a id="orgb4320f1"></a>

# dx/put

```clojure

;; (dx/commit {} [:dx/put {?table-id ?entity-id k v}])
(dx/commit {} [:dx/put {:db/id 1 :name "Ivan" :age 18}])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 18}}

;; (dx/commit {} [:dx/put [{?table-id ?entity-id k v} ...]])
(dx/commit {} [:dx/put [{:db/id 1 :name "Ivan" :age 18} {:db/id 2 :name "Petr" :age 33}]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 18}, [:db/id 2] {:db/id 2, :name "Petr", :age 33}}

;; (dx/commit {} [:dx/put [?table-id ?entity-id] {k v}])
(dx/commit {} [:dx/put [:db/id 1] {:name "Ivan" :age 18}])
;; => {[:db/id 1] {:name "Ivan", :age 18, :db/id 1}}

;; (dx/commit {} [:dx/put [?table-id ?entity-id] k v])
(dx/commit {} [:dx/put [:db/id 1] :name "Ivan"])
;; => {[:db/id 1] {:db/id 1, :name "Ivan"}}

;; (dx/commit {} [:dx/put [?table-id ?entity-id] k v & kvs])
(dx/commit {} [:dx/put [:db/id 1] :name "Ivan" :age 18])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 18}}

```

`dx/put` acts as assoc and always replaces values

```clojure

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan"}} [:dx/put [:db/id 1] {:name "Petr"}])
;; => {[:db/id 1] {:name "Petr", :db/id 1}}

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan"}} [:dx/put [:db/id 1] :name "Petr"])
;; => {[:db/id 1] {:db/id 1, :name "Petr"}}

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}} [:dx/put [:db/id 1] :aka ["Tupen"]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :aka ["Tupen"]}}

```

references are created automatically and data is normalised

```clojure

(dx/commit {} [:dx/put {:db/id 1 :name "Ivan" :friend {:db/id 2 :name "Petr"}}])
; => {[:db/id 2] {:_friend [:db/id 1], :db/id 2, :name "Petr"}, [:db/id 1] {:db/id 1, :name "Ivan", :friend [:db/id 2]}}

(dx/commit {} [:dx/put {:db/id 1 :name "Ivan" :friend [{:db/id 2 :name "Petr"} {:db/id 3 :name "Lucy"}]}])
;; => {[:db/id 2] {:_friend [:db/id 1], :db/id 2, :name "Petr"},
;;     [:db/id 3] {:_friend [:db/id 1], :db/id 3, :name "Lucy"},
;;     [:db/id 1] {:db/id 1, :name "Ivan", :friend #ordered/set #{[:db/id 2] [:db/id 3]}}}
```

for improved performance, back references are kept explicitly in the map.


<a id="orgfd05f9b"></a>

# dx/delete

```clojure

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}}
           [:dx/delete [:db/id 1]])
;; => {}

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}}
           [:dx/delete {:db/id 1}])
;; => {}

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}}
           [:dx/delete [:db/id 1] :name])
;; => {[:db/id 1] {:db/id 1, :aka ["Devil"]}}

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}}
           [:dx/delete [:db/id 1] [:name :aka]])
;; => {}

```

`dx/delete` allows you to delete an item from the collection

```clojure

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil" "Tupen"]}}
           [:dx/delete [:db/id 1] :aka "Devil"])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :aka ["Tupen"]}}

```

`dx/delete` keeps data compact

```clojure

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan"}}
           [:dx/delete [:db/id 1] :name])
;; => {}

```

deleting entire entities also deletes all references to them

```clojure

(def db (dx/commit {} [:dx/put {:db/id 1 :name "Ivan" :friend [{:db/id 2 :name "Petr"} {:db/id 3 :name "Lucy"}]}]))
db
;; => {[:db/id 2] {:_friend [:db/id 1], :db/id 2, :name "Petr"},
;;     [:db/id 3] {:_friend [:db/id 1], :db/id 3, :name "Lucy"},
;;     [:db/id 1] {:db/id 1, :name "Ivan", :friend #ordered/set #{[:db/id 2] [:db/id 3]}}}

(dx/commit db [:dx/delete [:db/id 2]])
;; => {[:db/id 3] {:_friend [:db/id 1], :db/id 3, :name "Lucy"},
;;     [:db/id 1] {:db/id 1, :name "Ivan", :friend #ordered/set #{[:db/id 3]}}}

(dx/commit db [:dx/delete [:db/id 1]])
;; => {[:db/id 2] {:db/id 2, :name "Petr"},
;;     [:db/id 3] {:db/id 3, :name "Lucy"}}

```


<a id="org4f168e2"></a>

# dx/merge

`dx/merge` always tries to merge data instead of replacing it, which is especially important for collections

```clojure

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}}
           [:dx/merge [:db/id 1] :name "Petr"])
;; => {[:db/id 1] {:db/id 1, :name "Petr", :aka ["Devil"]}}

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}}
           [:dx/merge [:db/id 1] {:aka "Tupen"}])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :aka ["Devil" "Tupen"]}}

(dx/commit {[:db/id 1] {:db/id 1 :name "Ivan" :aka ["Devil"]}}
           [:dx/merge [:db/id 1] {:aka ["Tupen"]}])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :aka ["Devil" "Tupen"]}}
```


<a id="org9f38158"></a>

# dx/update

`dx/update` works identically to `update` and `update-in`, depending on the number of arguments. however, it should be used with extreme caution, because the correctness of the data is not verified in any way, so it is possible, for example, to add a reference to the collection without automatically adding a back reference

if the third argument to a transaction is a function and fourth is a key, then the value on which function is called will be the entire document

```clojure

(dx/commit {[:db/id 1] {:db/id 1 :name "ivan" :aka ["devil"]}}
           [:dx/update [:db/id 1] assoc :aka "tupen"])
;; => {[:db/id 1] {:db/id 1, :name "ivan", :aka "tupen"}}

```

if the third argument to a transaction is a key and the fourth argument is a function, then the value on which the function is called will be the value under the key

```clojure

(dx/commit {[:db/id 1] {:db/id 1 :name "ivan" :aka ["devil"]}}
           [:dx/update [:db/id 1] :aka conj "tupen"])
;; => {[:db/id 1] {:db/id 1, :name "ivan", :aka ["devil" "tupen"]}}

```


<a id="org3a6caff"></a>

# dx/match

`dx/match` allows for conditional execution of transactions. the transaction itself returns a db if the condition is met, in a transaction chain this leads to transactions being executed until either `nil` is reached or another condition is met.

```clojure

(def db (dx/commit {} [:dx/put {:db/id 1 :name "Ivan" :age 18}]))
db
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 18}}

;; condition may be kv
(dx/commit db [:dx/match [:db/id 1] :name "Ivan"])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 18}}

;; or map, where all the individual kv are checked, rather than the identical to doc
(dx/commit db [[:dx/match [:db/id 1] {:name "Ivan"}]
               [:dx/update [:db/id 1] :age inc]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 19}}

;; the reference can be either a vector or a map
(dx/commit db [[:dx/match {:db/id 1 :name "Ivan"}]
               [:dx/update [:db/id 1] :age inc]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 19}}


(dx/commit db [[:dx/match [:db/id 1] :name "Ivan"]
               [:dx/update [:db/id 1] :age inc]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 19}}

(dx/commit db [[:dx/match [:db/id 1] :name "Petr"]
               [:dx/update [:db/id 1] :age inc]
               [:dx/match [:db/id 1] :name "Ivan"]
               [:dx/update [:db/id 1] :age dec]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 17}}

;; condition may be also fn?, then the function is called with the doc
(dx/commit db [[:dx/match [:db/id 1] (fn [m] (= 3 (count m)))]
               [:dx/update [:db/id 1] :age inc]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 19}}

;; condition may be k fn?, then the function is called with the value under key k
(dx/commit db [[:dx/match [:db/id 1] :age #(>= % 18)]
               [:dx/put [:db/id 1] :adult? true]])
;; => {[:db/id 1] {:db/id 1, :name "Ivan", :age 18, :adult? true}}

```
