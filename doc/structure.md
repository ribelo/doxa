`db` is a simple map with two levels of nesting

```clojure

{[?table-id ?entity-id :as ref] {?key ?value}}

```

example

```clojure

{[:person/id 1] {:person/id 1 :name "ivan" :age 18 :_friend #{[:person/id 2]}}
 [:person/id 2] {:person/id 2 :name "petr" :age 24 :friend #{[:person/id 1]}}}

```

reference, is always a two-element vector. table-id must be a keyword where `(name ?k)` returns `id`, i.e `db/id`, `people/id`, etc. in the case of several keys that satisfy a condition, the behaviour will be unpredictable.

entity-id can by any value, which allows a great flexibility, and importantly is descriptive, e.g `[:country/id :andora]`, `[:people/id [:marketing "zbyszek.nowak@gmail.com"]]`.

references and back references are a own implementation of ordered/set based on [flatland/ordered](https://github.com/clj-commons/ordered/tree/master/src/flatland/ordered). unfortunately it does not support `cljs`, so i decided to rewrite it. the use of ordered/set ensures distinct values, while preserving the order of insertion.
