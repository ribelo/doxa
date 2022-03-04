```clojure

(require '[ribelo.doxa :as dx])

```

as `doxa` does not use any special data type, there is no need to create a db, you can simply use a `{}`. however, `dx/create-dx` allows you to populate the db with data and also allows you to simply add the metadata.

```clojure

(dx/create-dx {} [{:db/id 1 :name "Petr" :aka ["Devil"]}])
(dx/create-dx {} [{:db/id 1 :name "Petr" :aka ["Devil"]}] {::dx/cache (atom (dxc/doxa-cache))})

```
