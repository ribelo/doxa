(ns ribelo.doxa.ordered-set
  (:require
   [ribelo.extropy :as ex]))

#?(:clj (set! *warn-on-reflection* true))

(deftype -skiping-iterator [#?(:clj ^java.util.Iterator it :cljs ^js it) ^:unsynchronized-mutable x]
  #?(:clj java.util.Iterator :cljs Object)
  (hasNext [_]
    (if-not (identical? x ex/-sentinel)
      true
      (loop []
        (if (.hasNext it)
          (let [v (.next it)]
            (if-not (identical? v ex/-sentinel)
              (do (set! x v) true)
              (recur)))
          false))))
  (next [this]
    (if (.hasNext this)
      (let [v x] (set! x ex/-sentinel) v)
      (throw (ex-info "No such element" {})))))

#?(:clj
   (deftype OrderedSet [meta ^clojure.lang.IPersistentMap m ^clojure.lang.IPersistentVector xs]
     java.util.Set

     Iterable
     (iterator [_]
       (-skiping-iterator. (ex/-iter xs) ex/-sentinel))

     clojure.lang.IObj
     (withMeta [this new-meta]
       (if (identical? new-meta meta)
         this
         (OrderedSet. new-meta m xs)))

     clojure.lang.Seqable
     (seq [_] (.seq ^clojure.lang.IPersistentVector (ex/-remove (partial identical? ex/-sentinel) xs)))

     clojure.lang.IMeta
     (meta [_] meta)

     java.util.Collection
     (size [_] (.count m))

     clojure.lang.Counted
     (count [_] (.count m))

     clojure.lang.IReduce
     (reduce [_ f]
       (reduce f (ex/-remove (partial identical? ex/-sentinel) xs)))

     clojure.lang.IReduceInit
     (reduce [_ f v]
       (reduce f v (ex/-remove (partial identical? ex/-sentinel) xs)))

     clojure.lang.IPersistentSet
     (disjoin [this v]
       (if-let [i (.valAt m v)]
         (OrderedSet. meta (dissoc m v) (.assoc xs i ex/-sentinel))
         this))

     clojure.lang.IPersistentCollection
     (equiv [this o]
       (and
         (instance? OrderedSet o)
         (== (count this) (count o))
         (loop [xs (ex/-iter this) ys (ex/-iter o)]
           (if (.hasNext xs)
             (if (= (.next xs) (.next ys))
               (recur xs ys)
               false)
             true))))

     (cons [this v]
       (if (.containsKey m v)
         this
         (OrderedSet. meta (.assoc ^clojure.lang.Associative m v (.count xs)) (.cons xs v))))

     (empty [_]
       (OrderedSet. {} {} []))

     clojure.lang.ILookup
     (valAt [_ v]
       (if (.containsKey m v)
         v
         nil))
     (valAt [_ v not-found]
       (if (.containsKey m v)
         v
         not-found))

     clojure.lang.IFn
     (invoke [this k]
       (.valAt this k))
     (invoke [this k not-found]
       (.valAt this k not-found)))

   :cljs
   (deftype OrderedSet [meta ^:mutable ^js obj ^:mutable ^js arr]
     Object
     (toString [coll]
       (pr-str* coll))
     (equiv [this other]
       (-equiv this other))
     (has [coll k]
       (contains? coll k))

     IIterable
     (-iterator [_]
       (ArrayIter. arr 0))

     IPrintWithWriter
     (-pr-writer [_ writer opts]
       (-write writer "#ordered/set ")
       (-write writer "#{")
       (let [it (ex/-iter arr)]
         (loop []
           (when (.hasNext it)
             (do (-write writer (pr-str (.next it)))
                 (when (.hasNext it)
                   (-write writer \space))
                 (recur)))))
       (-write writer "}"))

     IWithMeta
     (-with-meta [this new-meta]
       (if (identical? new-meta meta)
         this
         (OrderedSet. new-meta obj arr)))

     IMeta
     (-meta [_] meta)

     IEquiv
     (-equiv [_ ^js o]
       (and
         (instance? OrderedSet o)
         (== (.-length arr) (.-length (.-arr o)))
         ^boolean
         (ex/-loop [x arr y (.-arr o) :let [acc true]]
           (do
             (if (= x y)
               (recur true)
               false))
           acc)))


     ISet
     (-disjoin [this v]
       (if-let [i (aget obj v)]
         (OrderedSet.
           meta
           (doto obj (js-delete v))
           (doto arr (.splice i 1)))
         this))

     ICollection
     (-conj [this v]
       (if-let [i (aget obj v)]
         this
         (OrderedSet.
           meta
           (doto obj (aset v (.-length arr)))
           (doto arr (.push v)))))

     IEmptyableCollection
     (-empty [_]
       (OrderedSet. {} (js/Obj.) (js/Array.)))

     ICounted
     (-count [_] (.-length arr))

     ILookup
     (-lookup [_ v]
       (if (aget obj v)
         v
         nil))

     (-lookup [_ v not-found]
       (if (aget obj v)
         v
         not-found))))

#?(:clj
   (defmethod print-method OrderedSet
     [x ^java.io.Writer writer]
     (.write writer "#ordered/set ")
     (.write writer "#{")
     (let [it (ex/-iter x)]
       (loop []
         (when (.hasNext it)
           (do (.write writer (pr-str (.next it)))
               (when (.hasNext it)
                 (.write writer " "))
               (recur)))))
     (.write writer "}")))

#?(:clj
   (defn ordered-set
     ([] (ordered-set []))
     ([xs]
      (ex/-reduce
        (fn [acc x] (conj acc x))
        (OrderedSet. {} {} [])
        xs)))
   :cljs
   (defn ordered-set
     ([] (ordered-set []))
     ([xs]
      (reduce
        (fn [acc x]
          (conj acc x))
        (OrderedSet. {} (js/Object.) (js/Array.))
        xs))))

#?(:cljs
   (def transit-write-handlers
     {OrderedSet
      (reify Object
        (tag [_ _] "dx/os")
        (rep [_ ^js os] (.-arr os))
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))}))

(def transit-read-handlers
  {"dx/os" (fn [x] (ordered-set x))})
